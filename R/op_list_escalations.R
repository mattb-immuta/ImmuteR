# Open customer escalations, one row per CUSTX Jira issue.
#
# The BigQuery support rollup counts these (custx_open) but holds no detail —
# no severity, no status, no summary. That detail only exists in Jira, so this
# pulls it directly rather than trying to reconstruct it from a count.

# Standard Jira Cloud fields. These are stable across every Cloud instance, so
# they are named directly. The customer is the one field that is NOT standard —
# see .esc_customer() below, which is where instance-specific knowledge lives.
.ESC_FIELDS <- c("summary", "status", "priority", "created", "updated",
                 "assignee", "reporter", "labels", "components")

# Field-name fragments that suggest a custom field holding the account. Ordered
# most specific first: a field literally called "Customer" beats one called
# "Organization", which beats a generic "Company".
.ESC_CUSTOMER_HINTS <- c("customer", "account", "organi", "company", "client")

#' Open customer escalations from Jira
#'
#' One row per open issue in the CUSTX Jira project, with the fields the
#' Escalations tab renders: key, summary, severity, status, customer, age, and
#' a browse URL. Attached by \code{\link{enrich_opportunities}} as
#' \code{op_list$escalations}.
#'
#' @section Credentials:
#' Reads \code{JIRA_EMAIL} and \code{JIRA_TOKEN} from the environment (set them
#' in \code{.Renviron} via \code{usethis::edit_r_environ()}); create a token at
#' \url{https://id.atlassian.com/manage-profile/security/api-tokens}. Missing
#' credentials raise an error, which \code{enrich_opportunities()} catches and
#' downgrades to a warning — the rest of the pipeline is unaffected and the tab
#' renders an empty state.
#'
#' @section Which issues count as open:
#' \code{statusCategory != Done}, i.e. Jira's own notion of not-finished rather
#' than a hardcoded list of status names. A workflow can gain a status without
#' this function needing to know about it.
#'
#' @section Finding the customer:
#' Jira has no standard "customer" field, and which custom field CUSTX uses is
#' instance-specific. Rather than hardcode a \code{customerfield_NNNNN} that
#' would silently return nothing if it were wrong, this resolves it at runtime:
#' the API is asked for field \emph{names} alongside values, and the first field
#' whose name looks like a customer field \emph{and} whose value matches a known
#' account is used. The resolved id is reported in the message and can be pinned
#' with \code{customer_field} once known, which is faster and unambiguous.
#'
#' If no such field exists, it falls back to looking for an account name inside
#' the issue summary — escalation titles usually lead with the customer. Rows
#' that match no account keep \code{NA} and are counted in the message rather
#' than dropped, so a broken match is visible instead of silent.
#'
#' @param accounts Data frame. \code{op_list$accounts}; supplies the account
#'   list to match against, and the Region / CSM joined onto each escalation so
#'   the dashboard's global facets work on this tab too.
#' @param project Character. Jira project key. Default \code{"CUSTX"}.
#' @param base_url Character. Jira base URL. Defaults to the
#'   \code{JIRA_BASE_URL} environment variable, falling back to
#'   \code{"https://immuta.atlassian.net"}.
#' @param customer_field Character or \code{NULL}. Pin the custom field id that
#'   holds the account (e.g. \code{"customfield_10234"}) to skip auto-detection.
#' @param max_results Integer. Cap on issues fetched. Default \code{500}.
#' @param quiet Logical. Suppress the summary message. Default \code{FALSE}.
#'
#' @return A data frame, one row per open escalation, with columns \code{key},
#'   \code{url}, \code{summary}, \code{severity}, \code{severity_rank},
#'   \code{status}, \code{status_category}, \code{Account Name}, \code{Region},
#'   \code{csm}, \code{assignee}, \code{created}, \code{updated} and
#'   \code{age_days}. Zero rows (with the same columns) when the project has no
#'   open issues, so downstream code never has to test for NULL.
#'
#' @examples
#' \dontrun{
#'   op_list_escalations(op_list$accounts)
#'   op_list_escalations(op_list$accounts, customer_field = "customfield_10234")
#' }
#' @export
op_list_escalations <- function(accounts,
                                project        = "CUSTX",
                                base_url       = Sys.getenv("JIRA_BASE_URL",
                                                            "https://immuta.atlassian.net"),
                                customer_field = NULL,
                                max_results    = 500,
                                quiet          = FALSE) {

  email <- Sys.getenv("JIRA_EMAIL")
  token <- Sys.getenv("JIRA_TOKEN")
  if (!nzchar(email) || !nzchar(token)) {
    stop("JIRA_EMAIL and JIRA_TOKEN must be set in .Renviron. ",
         "Token: https://id.atlassian.com/manage-profile/security/api-tokens",
         call. = FALSE)
  }
  if (is.null(accounts) || !("Account Name" %in% names(accounts))) {
    stop("`accounts` must be op_list$accounts, with an `Account Name` column.",
         call. = FALSE)
  }

  jql <- paste0("project = ", project,
                " AND statusCategory != Done ORDER BY created DESC")

  issues <- .esc_search(base_url, email, token, jql,
                        fields = c(.ESC_FIELDS, if (!is.null(customer_field)) customer_field),
                        max_results = max_results)

  if (length(issues) == 0) {
    if (!quiet) message(".. op_list_escalations: no open ", project, " issues")
    return(.esc_empty())
  }

  acct_names <- trimws(as.character(accounts$`Account Name`))

  resolved <- .esc_customer(issues, acct_names, customer_field)

  out <- data.frame(
    key             = vapply(issues, function(i) .esc_chr(i$key), character(1)),
    summary         = vapply(issues, function(i) .esc_chr(i$fields$summary), character(1)),
    severity        = vapply(issues, function(i) .esc_chr(i$fields$priority$name), character(1)),
    status          = vapply(issues, function(i) .esc_chr(i$fields$status$name), character(1)),
    status_category = vapply(issues, function(i) .esc_chr(i$fields$status$statusCategory$name), character(1)),
    assignee        = vapply(issues, function(i) .esc_chr(i$fields$assignee$displayName), character(1)),
    created         = vapply(issues, function(i) .esc_chr(i$fields$created), character(1)),
    updated         = vapply(issues, function(i) .esc_chr(i$fields$updated), character(1)),
    `Account Name`  = resolved$account,
    check.names = FALSE, stringsAsFactors = FALSE
  )

  out$url <- paste0(sub("/+$", "", base_url), "/browse/", out$key)

  # Jira timestamps are ISO 8601 with an offset; only the date is wanted.
  out$created  <- substr(out$created, 1, 10)
  out$updated  <- substr(out$updated, 1, 10)
  out$age_days <- as.integer(Sys.Date() - suppressWarnings(as.Date(out$created)))

  # Sort rank for the severity facet. Jira priority names vary by instance
  # (P1 / Sev 1 / Highest all appear in the wild), so rank on the digit where
  # there is one and fall back to the default Jira ladder.
  out$severity_rank <- .esc_sev_rank(out$severity)

  # Region and CSM come from the account, not the issue — Jira does not know
  # about either, and the dashboard's global facets need them present.
  idx <- match(trimws(out$`Account Name`), acct_names)
  out$Region <- if ("Region" %in% names(accounts)) {
    as.character(accounts$Region)[idx]
  } else NA_character_
  out$csm <- if ("Customer Success Manager" %in% names(accounts)) {
    as.character(accounts$`Customer Success Manager`)[idx]
  } else NA_character_

  out <- out[order(out$severity_rank, -out$age_days), , drop = FALSE]
  rownames(out) <- NULL

  if (!quiet) {
    unmatched <- sum(is.na(out$`Account Name`))
    message(".. op_list_escalations [jira]: ", nrow(out), " open ", project,
            " issue(s); customer from ",
            if (is.null(resolved$field)) "summary text" else resolved$field,
            "; ", nrow(out) - unmatched, " matched to an account",
            if (unmatched > 0) paste0(", ", unmatched, " UNMATCHED") else "")
  }

  out
}


# ── Internal ──────────────────────────────────────────────────────────────────

.esc_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]

.esc_empty <- function() {
  data.frame(key = character(0), url = character(0), summary = character(0),
             severity = character(0), severity_rank = integer(0),
             status = character(0), status_category = character(0),
             `Account Name` = character(0), Region = character(0),
             csm = character(0), assignee = character(0),
             created = character(0), updated = character(0),
             age_days = integer(0),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# POST /rest/api/3/search/jql, paging until exhausted. The older GET
# /rest/api/3/search is deprecated on Jira Cloud; this is the replacement.
# `expand = names` is what makes runtime customer-field detection possible —
# it returns the human-readable label for every field id.
.esc_search <- function(base_url, email, token, jql, fields, max_results) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required.", call. = FALSE)
  }

  out   <- list()
  token_next <- NULL

  repeat {
    body <- list(jql = jql, maxResults = min(100L, max_results - length(out)),
                 fields = as.list(unique(c(fields, "*navigable"))),
                 expand = "names")
    if (!is.null(token_next)) body$nextPageToken <- token_next

    resp <- httr2::request(paste0(sub("/+$", "", base_url), "/rest/api/3/search/jql")) |>
      httr2::req_auth_basic(email, token) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()

    parsed <- httr2::resp_body_json(resp)
    page   <- parsed$issues %||% list()

    # Carry the field-name map onto the first issue so .esc_customer() can read
    # labels without a second request.
    if (length(page) > 0 && !is.null(parsed$names)) {
      attr(page, "names_map") <- parsed$names
    }
    if (length(out) == 0) names_map <- attr(page, "names_map")

    out <- c(out, page)

    token_next <- parsed$nextPageToken
    if (is.null(token_next) || length(out) >= max_results || length(page) == 0) break
  }

  if (exists("names_map", inherits = FALSE) && !is.null(names_map)) {
    attr(out, "names_map") <- names_map
  }
  out
}

# Resolve the account for each issue. Returns list(account = <chr>, field = id
# or NULL when the summary fallback was used).
.esc_customer <- function(issues, acct_names, pinned) {

  norm <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(as.character(x))))
  acct_norm <- norm(acct_names)

  val_of <- function(i, fid) {
    v <- i$fields[[fid]]
    if (is.null(v)) return(NA_character_)
    if (is.list(v)) {
      for (k in c("value", "name", "displayName", "title")) {
        if (!is.null(v[[k]])) return(as.character(v[[k]])[1])
      }
      return(NA_character_)
    }
    as.character(v)[1]
  }

  # Map a raw value onto an account name, exactly then normalised.
  to_account <- function(vals) {
    hit <- match(trimws(vals), acct_names)
    miss <- is.na(hit)
    if (any(miss)) hit[miss] <- match(norm(vals[miss]), acct_norm)
    ifelse(is.na(hit), NA_character_, acct_names[hit])
  }

  if (!is.null(pinned)) {
    vals <- vapply(issues, val_of, character(1), fid = pinned)
    return(list(account = to_account(vals), field = pinned))
  }

  # Candidate custom fields: name looks like a customer field, and the field is
  # actually populated on at least one issue.
  nm <- attr(issues, "names_map")
  if (!is.null(nm)) {
    cands <- names(nm)[vapply(names(nm), function(k)
      any(vapply(.ESC_CUSTOMER_HINTS,
                 function(h) grepl(h, nm[[k]], ignore.case = TRUE), logical(1))),
      logical(1))]

    # Score each candidate by how many issues it resolves to a real account —
    # the right field is the one that actually matches, not the one named best.
    best <- NULL; best_n <- 0L
    for (fid in cands) {
      vals <- vapply(issues, val_of, character(1), fid = fid)
      n    <- sum(!is.na(to_account(vals)))
      if (n > best_n) { best <- fid; best_n <- n }
    }
    if (!is.null(best) && best_n > 0) {
      vals <- vapply(issues, val_of, character(1), fid = best)
      return(list(account = to_account(vals), field = best))
    }
  }

  # Fallback: the account name inside the summary. Longest name first, so
  # "EY - New York" wins over a hypothetical "EY".
  summaries <- vapply(issues, function(i) .esc_chr(i$fields$summary), character(1))
  ord <- order(nchar(acct_names), decreasing = TRUE)
  matched <- vapply(summaries, function(s) {
    if (is.na(s)) return(NA_character_)
    sn <- norm(s)
    for (k in ord) if (nzchar(acct_norm[k]) && grepl(acct_norm[k], sn, fixed = TRUE)) {
      return(acct_names[k])
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  list(account = matched, field = NULL)
}

# P1/P2/... -> 1/2/...; otherwise the default Jira ladder; unknown sorts last.
.esc_sev_rank <- function(x) {
  s <- tolower(trimws(as.character(x)))
  d <- suppressWarnings(as.integer(sub(".*?([1-4]).*", "\\1", s)))
  ladder <- c(highest = 1L, critical = 1L, high = 2L, medium = 3L,
              low = 4L, lowest = 5L)
  out <- ifelse(grepl("[1-4]", s), d, unname(ladder[s]))
  out[is.na(out)] <- 9L
  as.integer(out)
}
