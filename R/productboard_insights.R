# Productboard insights: a tidy, cached pull of the note corpus
#
# Productboard calls a piece of feedback a *note*; linking a note to a feature
# creates an *insight*. In conversation "insights" usually means the notes
# themselves, so this module pulls every note plus the context needed to
# analyse it: who it came from (company / user), what it's linked to
# (features), and who wrote it (owner / creator member).
#
# API facts this is built on (Productboard v2 reference, checked Oct 2026):
#   - GET /v2/notes returns each note's relationships INLINE (customer + linked
#     features), paginated per note via relationships$links$`next`. So the
#     one-call-per-note loop is only needed for the rare note whose
#     relationships overflow a page — not for every note.
#   - Companies and users are ENTITY types: GET /v2/entities?type[]=company and
#     ?type[]=user. There are no /v2/companies or /v2/users endpoints (the
#     relationship `links.self` URLs in the API's own examples are misleading).
#   - A user's company is its `parent` relationship.
#   - Company `metadata.source` carries the CRM record when companies are
#     synced from Salesforce, which allows an exact Account ID match.
#   - Without the users:pii:read / members:pii:read scopes, user names/emails
#     and owner/creator emails come back as "[redacted]".
#   - Rate limit: 50 requests/second per token; 429s carry Retry-After.

# ---- Internal: raw (unflattened) pulls --------------------------------------

#' @noRd
.pb_nz <- function(a, b) if (is.null(a)) b else a

# Walk a nested list by name; NULL as soon as a level is missing.
#' @noRd
.pb_pluck <- function(x, ...) {
  for (k in c(...)) {
    if (is.null(x) || !is.list(x) || is.null(x[[k]])) return(NULL)
    x <- x[[k]]
  }
  x
}

#' @noRd
.pb_chr1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- x[[1]]
  if (is.null(v) || is.list(v) || (length(v) == 1 && is.na(v))) return(NA_character_)
  as.character(v)
}

#' @noRd
.pb_lgl1 <- function(x) {
  if (is.null(x) || length(x) == 0 || is.list(x[[1]])) return(NA)
  as.logical(x[[1]])
}

# Request with throttling under the 50 req/s limit and retry on 429 / 5xx
# (httr2 honours Retry-After on its own).
#' @noRd
.pb_request_raw <- function(url) {
  req <- httr2::request(url)
  req <- .pb_auth(req)
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_throttle(req, rate = 40, realm = "productboard")
  httr2::req_retry(req, max_tries = 6,
                   is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504))
}

# Every item across all pages, as plain R lists (no flattening — notes have
# nested, polymorphic fields that flatten badly).
#' @noRd
.pb_get_raw <- function(path, query = list(), verbose = TRUE) {
  req <- .pb_request_raw(paste0(.pb_api_base, path))
  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)

  items <- list()
  page  <- 0
  repeat {
    page   <- page + 1
    resp   <- httr2::req_perform(req)
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
    data   <- .pb_nz(parsed$data, list())
    items  <- c(items, data)
    if (verbose && page %% 10 == 0) message("   ", path, ": ", length(items), " so far")
    next_url <- .pb_chr1(.pb_pluck(parsed, "links", "next"))
    if (is.na(next_url) || !nzchar(next_url)) break
    req <- httr2::req_url(req, next_url)
  }
  items
}

# A note's (or entity's) relationship list, in either shape the API uses:
# {data: [...], links: {next}} per the schema, or a bare array per the docs'
# examples. Returns list(rels = <list>, next = <url or NA>).
#' @noRd
.pb_rel_list <- function(r) {
  if (is.null(r) || length(r) == 0) return(list(rels = list(), more = NA_character_))
  if (!is.null(names(r)) && "data" %in% names(r)) {
    return(list(rels = .pb_nz(r$data, list()), more = .pb_chr1(.pb_pluck(r, "links", "next"))))
  }
  list(rels = r, more = NA_character_)
}

# ---- Internal: text ----------------------------------------------------------

# Note HTML -> readable plain text. Block tags become line breaks and list items
# keep a "- " marker, so structure (headings, bullets, "Label: value" lines)
# survives for the EDA to detect.
#' @noRd
.pb_html_to_text <- function(html) {
  out <- vapply(html, function(h) {
    if (is.na(h) || !nzchar(h)) return(NA_character_)
    x <- gsub("<br\\s*/?>", "\n", h, ignore.case = TRUE, perl = TRUE)
    x <- gsub("<li[^>]*>", "\n- ", x, ignore.case = TRUE, perl = TRUE)
    x <- gsub("</(p|h[1-6]|li|tr|blockquote|pre|ul|ol)>", "\n", x, ignore.case = TRUE, perl = TRUE)
    x <- gsub("<[^>]+>", "", x, perl = TRUE)
    x <- gsub("&nbsp;|&#160;", " ", x)
    x <- gsub("&amp;", "&", x); x <- gsub("&lt;", "<", x); x <- gsub("&gt;", ">", x)
    x <- gsub("&quot;", "\"", x); x <- gsub("&#39;|&apos;", "'", x)
    # Remaining numeric entities.
    m <- gregexpr("&#[0-9]+;", x)
    regmatches(x, m) <- lapply(regmatches(x, m), function(e)
      vapply(e, function(s) intToUtf8(as.integer(gsub("[^0-9]", "", s))), character(1)))
    x <- gsub("[ \t]+", " ", x)
    x <- gsub(" *\n *", "\n", x)
    x <- gsub("\n{3,}", "\n\n", x)
    trimws(x)
  }, character(1), USE.NAMES = FALSE)
  out
}

# Content in any of the three note shapes -> list(text, html).
#' @noRd
.pb_note_content <- function(content) {
  if (is.null(content) || length(content) == 0) return(list(text = NA_character_, html = NA_character_))
  if (is.character(content)) {
    html <- paste(content, collapse = "")
    return(list(text = .pb_html_to_text(html), html = html))
  }
  if (is.list(content) && is.null(names(content))) {
    # conversationNote: parts with authorName / content.
    parts <- vapply(content, function(p) {
      body <- .pb_html_to_text(.pb_chr1(p$content))
      who  <- .pb_chr1(p$authorName)
      if (is.na(body)) return("")
      if (is.na(who)) body else paste0(who, ": ", body)
    }, character(1))
    txt <- paste(parts[nzchar(parts)], collapse = "\n")
    return(list(text = if (nzchar(txt)) txt else NA_character_, html = NA_character_))
  }
  # opportunityNote or anything else structured: keep it readable as JSON.
  list(text = as.character(jsonlite::toJSON(content, auto_unbox = TRUE)), html = NA_character_)
}

# ---- Internal: tidy tables ---------------------------------------------------

#' @noRd
.pb_tidy_notes <- function(items, fetch_overflow = TRUE) {
  n_overflow <- 0
  notes <- vector("list", length(items))
  links <- vector("list", length(items))

  for (i in seq_along(items)) {
    n  <- items[[i]]
    f  <- .pb_nz(n$fields, list())
    ct <- .pb_note_content(f$content)
    tags <- vapply(.pb_nz(f$tags, list()), function(t) .pb_chr1(t$name), character(1))
    tags <- tags[!is.na(tags)]

    rl <- .pb_rel_list(n$relationships)
    rels <- rl$rels
    if (!is.na(rl$more) && fetch_overflow) {
      n_overflow <- n_overflow + 1
      rels <- tryCatch(.pb_get_raw(paste0("/notes/", n$id, "/relationships"), verbose = FALSE),
                       error = function(e) rels)
    }
    rel_type    <- vapply(rels, function(r) .pb_chr1(r$type), character(1))
    target_id   <- vapply(rels, function(r) .pb_chr1(.pb_pluck(r, "target", "id")), character(1))
    target_type <- vapply(rels, function(r) .pb_chr1(.pb_pluck(r, "target", "type")), character(1))
    cust <- which(rel_type == "customer")[1]

    notes[[i]] <- tibble::tibble(
      note_id          = .pb_chr1(n$id),
      note_type        = .pb_chr1(n$type),
      title            = .pb_chr1(f$name),
      content_text     = ct$text,
      content_html     = ct$html,
      tags             = list(tags),
      tag_string       = if (length(tags)) paste(tags, collapse = "; ") else NA_character_,
      owner_id         = .pb_chr1(.pb_pluck(f, "owner", "id")),
      owner_email      = .pb_chr1(.pb_pluck(f, "owner", "email")),
      creator_id       = .pb_chr1(.pb_pluck(f, "creator", "id")),
      creator_email    = .pb_chr1(.pb_pluck(f, "creator", "email")),
      processed        = .pb_lgl1(f$processed),
      archived         = .pb_lgl1(f$archived),
      created_at       = .pb_chr1(.pb_nz(n$createdAt, f$createdAt)),
      updated_at       = .pb_chr1(.pb_nz(n$updatedAt, f$updatedAt)),
      source_system    = .pb_chr1(.pb_nz(.pb_pluck(n, "metadata", "source", "system"),
                                         .pb_pluck(f, "source", "system"))),
      source_record_id = .pb_chr1(.pb_nz(.pb_pluck(n, "metadata", "source", "recordId"),
                                         .pb_pluck(f, "source", "recordId"))),
      source_url       = .pb_chr1(.pb_nz(.pb_pluck(n, "metadata", "source", "url"),
                                         .pb_pluck(f, "source", "url"))),
      html_url         = .pb_chr1(.pb_pluck(n, "links", "html")),
      customer_type    = if (is.na(cust)) NA_character_ else target_type[cust],
      customer_id      = if (is.na(cust)) NA_character_ else target_id[cust]
    )
    keep <- rel_type %in% "link"
    links[[i]] <- tibble::tibble(note_id = rep(.pb_chr1(n$id), sum(keep)),
                                 target_id = target_id[keep], target_type = target_type[keep])
  }
  if (n_overflow) message("   fetched overflow relationships for ", n_overflow, " notes")

  notes <- dplyr::bind_rows(notes)
  if (nrow(notes)) {
    notes$created_at <- as.POSIXct(notes$created_at, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    notes$updated_at <- as.POSIXct(notes$updated_at, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  }
  list(notes = notes, links = dplyr::bind_rows(links))
}

# Entities (company / user / feature / subfeature) -> one row each. The parent
# relationship gives a user's company and a feature's component/parent.
#' @noRd
.pb_tidy_entities <- function(items) {
  rows <- lapply(items, function(e) {
    f  <- .pb_nz(e$fields, list())
    rl <- .pb_rel_list(e$relationships)$rels
    parent <- Filter(function(r) identical(.pb_chr1(r$type), "parent"), rl)
    tibble::tibble(
      id               = .pb_chr1(e$id),
      type             = .pb_chr1(e$type),
      name             = .pb_chr1(f$name),
      email            = .pb_chr1(f$email),
      status           = .pb_chr1(.pb_pluck(f, "status", "name")),
      archived         = .pb_lgl1(f$archived),
      parent_id        = if (length(parent)) .pb_chr1(.pb_pluck(parent[[1]], "target", "id")) else NA_character_,
      parent_type      = if (length(parent)) .pb_chr1(.pb_pluck(parent[[1]], "target", "type")) else NA_character_,
      source_system    = .pb_chr1(.pb_pluck(e, "metadata", "source", "system")),
      source_record_id = .pb_chr1(.pb_pluck(e, "metadata", "source", "recordId")),
      created_at       = .pb_chr1(e$createdAt),
      html_url         = .pb_chr1(.pb_pluck(e, "links", "html"))
    )
  })
  dplyr::bind_rows(rows)
}

#' @noRd
.pb_pull_entities <- function(type) {
  tryCatch(
    .pb_tidy_entities(.pb_get_raw("/entities", query = list(`type[]` = type))),
    error = function(e) {
      warning("Productboard: could not list `", type, "` entities (", conditionMessage(e),
              "). Continuing without them.", call. = FALSE)
      tibble::tibble()
    }
  )
}

# ---- Public ------------------------------------------------------------------

#' Pull the Productboard insight (note) corpus into tidy tables
#'
#' One pass over the Productboard v2 API: all notes with their inline
#' relationships, plus the company, user, feature and subfeature entities
#' needed to interpret them. Roughly \code{n_notes / 100 + n_entities / 100}
#' requests rather than one per note, so a full pull takes seconds to a few
#' minutes. The result is cached as \code{<cache_dir>/pb_insights_<date>.rds}
#' (\code{data/} is gitignored, so customer text stays out of git).
#'
#' @param cache_dir Character. Where to write the cache. \code{NULL} skips it.
#' @param use_cache Logical. If \code{TRUE} and a cache from today exists,
#'   load it instead of calling the API.
#' @return A list of tibbles:
#'   \describe{
#'     \item{notes}{One row per note: \code{note_id}, \code{note_type},
#'       \code{title}, \code{content_text} (HTML stripped, line structure
#'       kept), \code{content_html}, \code{tags} (list) / \code{tag_string},
#'       owner/creator ids and emails, \code{processed}, \code{archived},
#'       \code{created_at}, \code{updated_at}, source metadata,
#'       \code{html_url}, \code{customer_type} (\code{"company"} /
#'       \code{"user"} / \code{NA}), \code{customer_id}, and
#'       \code{company_id} (the company directly, or the user's parent).}
#'     \item{links}{One row per note-to-hierarchy link (\code{note_id},
#'       \code{target_id}, \code{target_type}) — each feature/subfeature row is
#'       an insight in Productboard's sense.}
#'     \item{companies, users, features}{One row per entity (\code{id},
#'       \code{type}, \code{name}, \code{status}, \code{parent_id}, source
#'       metadata, ...). \code{features} holds features and subfeatures.}
#'     \item{pulled_at}{POSIXct.}
#'   }
#' @examples
#' \dontrun{
#'   pb <- pb_pull_insights()
#'   pb <- pb_load_insights()          # most recent cache, no API calls
#' }
#' @export
pb_pull_insights <- function(cache_dir = "data/productboard", use_cache = FALSE) {
  if (use_cache && !is.null(cache_dir)) {
    f <- file.path(cache_dir, paste0("pb_insights_", Sys.Date(), ".rds"))
    if (file.exists(f)) { message("Loading cached Productboard pull: ", f); return(readRDS(f)) }
  }

  message(".. Productboard: pulling notes")
  raw_notes <- .pb_get_raw("/notes")
  message("   ", length(raw_notes), " notes")
  tn <- .pb_tidy_notes(raw_notes)

  message(".. Productboard: pulling companies, users, features")
  companies <- .pb_pull_entities("company")
  users     <- .pb_pull_entities("user")
  features  <- dplyr::bind_rows(.pb_pull_entities("feature"), .pb_pull_entities("subfeature"))

  notes <- tn$notes
  if (nrow(notes)) {
    user_company <- if (nrow(users)) users$parent_id[match(notes$customer_id, users$id)] else NA_character_
    notes$company_id <- ifelse(notes$customer_type %in% "company", notes$customer_id,
                        ifelse(notes$customer_type %in% "user", user_company, NA_character_))
    notes$company_name <- if (nrow(companies)) companies$name[match(notes$company_id, companies$id)] else NA_character_
  }

  out <- list(notes = notes, links = tn$links, companies = companies, users = users,
              features = features, pulled_at = Sys.time())

  message("   ", nrow(notes), " notes | ", nrow(tn$links), " links | ",
          nrow(companies), " companies | ", nrow(users), " users | ",
          nrow(features), " features/subfeatures")

  if (!is.null(cache_dir)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    f <- file.path(cache_dir, paste0("pb_insights_", Sys.Date(), ".rds"))
    saveRDS(out, f)
    message("   cached to ", f)
  }
  out
}

#' Load the most recent cached Productboard pull
#'
#' @param cache_dir Character. Folder written by \code{\link{pb_pull_insights}}.
#' @return The list \code{pb_pull_insights()} returns.
#' @export
pb_load_insights <- function(cache_dir = "data/productboard") {
  files <- list.files(cache_dir, pattern = "^pb_insights_\\d{4}-\\d{2}-\\d{2}\\.rds$", full.names = TRUE)
  if (!length(files)) stop("No cached Productboard pull in ", cache_dir, " — run pb_pull_insights() first.",
                           call. = FALSE)
  f <- sort(files, decreasing = TRUE)[1]
  message("Loading ", f)
  readRDS(f)
}

#' Match Productboard companies to Immuta (Salesforce) accounts
#'
#' Exact Salesforce Account ID first, when the company was synced from the CRM
#' and its \code{source_record_id} carries an ID (\code{001...}); otherwise
#' the case/punctuation/suffix-insensitive name match used elsewhere
#' (\code{.pb_normalize_name()}).
#'
#' @param pb List from \code{\link{pb_pull_insights}}.
#' @param accounts Data frame, typically \code{op_list$accounts}
#'   (\code{Account Name}, \code{Account ID}).
#' @param customers_only Logical. If \code{TRUE}, keep only notes matched to
#'   an account in \code{accounts} (and the links / companies / users behind
#'   them). Unmatched companies are still reported in
#'   \code{pb$unmatched_companies} so naming drift stays visible.
#' @return \code{pb} with \code{companies} gaining \code{account_name},
#'   \code{account_id} and \code{match_method} (\code{"sfdc_id"},
#'   \code{"name"}, \code{NA}), \code{notes} gaining \code{account_name},
#'   \code{account_id}, and \code{unmatched_companies} (tibble of company
#'   name and note count for notes whose company matched no account).
#' @export
pb_match_accounts <- function(pb, accounts, customers_only = FALSE) {
  co <- pb$companies
  if (is.null(co) || !nrow(co)) {
    pb$notes$account_name <- NA_character_
    pb$notes$account_id   <- NA_character_
    pb$unmatched_companies <- tibble::tibble(company_name = character(0), notes = integer(0))
    if (customers_only) pb <- .pb_keep_customers(pb)
    return(pb)
  }
  acct_name <- as.character(accounts$`Account Name`)
  acct_id15 <- substr(as.character(accounts$`Account ID`), 1, 15)

  sf_id <- regmatches(co$source_record_id, regexpr("001[A-Za-z0-9]{12,15}", co$source_record_id))
  co_sf <- rep(NA_character_, nrow(co))
  co_sf[grepl("001[A-Za-z0-9]{12,15}", co$source_record_id)] <- substr(sf_id, 1, 15)

  by_id   <- match(co_sf, acct_id15)
  by_name <- match(.pb_normalize_name(co$name), .pb_normalize_name(acct_name))
  by_name[is.na(co$name)] <- NA
  idx <- ifelse(is.na(by_id), by_name, by_id)

  co$account_name <- acct_name[idx]
  co$account_id   <- acct_id15[idx]
  co$match_method <- ifelse(!is.na(by_id), "sfdc_id", ifelse(!is.na(by_name), "name", NA_character_))
  pb$companies <- co

  j <- match(pb$notes$company_id, co$id)
  pb$notes$account_name <- co$account_name[j]
  pb$notes$account_id   <- co$account_id[j]

  um <- pb$notes[!is.na(pb$notes$company_id) & is.na(pb$notes$account_name), , drop = FALSE]
  pb$unmatched_companies <- if (nrow(um)) {
    tb <- sort(table(um$company_name), decreasing = TRUE)
    tibble::tibble(company_name = names(tb), notes = as.integer(tb))
  } else tibble::tibble(company_name = character(0), notes = integer(0))

  message("  Productboard companies matched to accounts: ", sum(!is.na(idx)), " of ", nrow(co),
          " (", sum(co$match_method %in% "sfdc_id"), " by SFDC ID, ",
          sum(co$match_method %in% "name"), " by name); notes with an account: ",
          sum(!is.na(pb$notes$account_name)), " of ", nrow(pb$notes))

  if (customers_only) pb <- .pb_keep_customers(pb)
  pb
}

# Restrict a matched pull to notes tied to an existing customer account, and
# the links / companies / users those notes reference.
#' @noRd
.pb_keep_customers <- function(pb) {
  n0 <- nrow(pb$notes)
  pb$notes <- pb$notes[!is.na(pb$notes$account_name), , drop = FALSE]
  if (!is.null(pb$links) && nrow(pb$links))
    pb$links <- pb$links[pb$links$note_id %in% pb$notes$note_id, , drop = FALSE]
  if (!is.null(pb$companies) && nrow(pb$companies))
    pb$companies <- pb$companies[!is.na(pb$companies$account_name), , drop = FALSE]
  if (!is.null(pb$users) && nrow(pb$users))
    pb$users <- pb$users[pb$users$parent_id %in% pb$companies$id, , drop = FALSE]
  pb$customers_only <- TRUE
  message("  customers only: kept ", nrow(pb$notes), " of ", n0, " notes (",
          length(unique(pb$notes$account_name)), " accounts); dropped ", n0 - nrow(pb$notes),
          " from prospects / unmatched companies / no customer")
  pb
}
