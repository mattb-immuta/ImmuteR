#' Productboard API integration (Personal Access Token)
#'
#' Read-only, internal use doesn't need the full OAuth 2.0 Authorization Code
#' dance (app registration, client ID/secret, browser consent). Productboard
#' offers a much simpler Personal Access Token method for exactly this case —
#' "best for internal tools, scripts, and getting started quickly" per their
#' own docs — so that's what this module uses.
#'
#' ## One-time setup
#' 1. In Productboard, go to Settings > Integrations > Public APIs > Access
#'    Token (requires the Pro plan or higher).
#' 2. Click the "+" icon to generate a token, and copy it immediately — you
#'    won't be able to see it again.
#' 3. Add it to your \code{.Renviron} (\code{usethis::edit_r_environ()}):
#'    \code{PRODUCTBOARD_API_TOKEN=your_token_here}, then restart R.
#'
#' That's it — no browser flow, no app registration, no client ID. The token
#' is sent as a Bearer token on every request. Treat it like a password: it's
#' valid until you revoke it in Workspace Settings.
#'
#' @name productboard
NULL

# ---- Internal: shared request builder --------------------------------------

.pb_api_base <- "https://api.productboard.com/v2"

#' @noRd
.pb_token <- function() {
  token <- Sys.getenv("PRODUCTBOARD_API_TOKEN")
  if (token == "") {
    stop(
      "Productboard API token not found. Set PRODUCTBOARD_API_TOKEN in your ",
      ".Renviron (see ?productboard for how to generate one in Workspace ",
      "Settings), then restart R.",
      call. = FALSE
    )
  }
  token
}

#' @noRd
.pb_request <- function(path) {
  httr2::request(paste0(.pb_api_base, path)) %>%
    httr2::req_auth_bearer_token(.pb_token()) %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_retry(max_tries = 3, backoff = ~ 2)
}

# ---- Internal: cursor pagination -------------------------------------------

#' @noRd
.pb_get_all <- function(path, query = list()) {
  req <- .pb_request(path)
  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)

  pages <- list()
  repeat {
    resp   <- httr2::req_perform(req)
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)

    page <- parsed[["data"]]
    if (!is.null(page) && length(page) > 0) {
      pages[[length(pages) + 1]] <- tibble::as_tibble(page)
    }

    next_url <- parsed[["links"]][["next"]]
    if (is.null(next_url) || length(next_url) == 0 || is.na(next_url)) break
    req <- httr2::req_url(req, next_url)
  }

  if (length(pages) == 0) return(tibble::tibble())
  dplyr::bind_rows(pages)
}

# ---- Public: data pulls ------------------------------------------------------

#' Pull Productboard notes (customer feedback)
#'
#' Wraps \code{GET /v2/notes}. A Personal Access Token has the same
#' permissions as the workspace member who generated it, so owner/creator
#' emails come through unredacted as long as that member can see PII.
#'
#' @param ... Named query parameters passed through as-is, e.g.
#'   \code{archived = "false"}, \code{createdFrom = "2026-01-01T00:00:00Z"}.
#'   See \url{https://developer.productboard.com/reference/listnotes}.
#' @return A tibble of notes, one row per note, with \code{fields.*} columns
#'   flattened (e.g. \code{fields.name}, \code{fields.content}).
#' @export
pb_get_notes <- function(...) {
  .pb_get_all("/notes", query = list(...))
}

#' Pull Productboard features (roadmap)
#'
#' Wraps \code{GET /v2/entities} filtered to \code{type[]=feature}.
#'
#' @param ... Named query parameters passed through as-is, e.g.
#'   \code{status = "in-progress"}. See
#'   \url{https://developer.productboard.com/reference/listentities}.
#' @return A tibble of features, one row per feature, with \code{fields.*}
#'   columns flattened (e.g. \code{fields.name}, \code{fields.status.name}).
#' @export
pb_get_features <- function(...) {
  extra <- list(...)
  query <- c(list(`type[]` = "feature"), extra)
  .pb_get_all("/entities", query = query)
}

#' List a note's relationships (customer + linked features)
#'
#' Wraps \code{GET /v2/notes/{id}/relationships}. There is no bulk/list
#' endpoint for this in Productboard's API — it must be called once per note,
#' which is what makes \code{op_list_product_board()} slower than the other
#' \code{enrich_*()} steps. At the default rate limit (50 req/s per token)
#' this is fine for a few thousand notes but worth knowing about.
#'
#' @param note_id Character. A note UUID (e.g. from \code{pb_get_notes()$id}).
#' @return A tibble with columns \code{type} (\code{"customer"} or
#'   \code{"link"}), \code{target.id}, and \code{target.type} (one of
#'   \code{"user"}, \code{"company"}, \code{"feature"}, \code{"subfeature"},
#'   \code{"product"}, \code{"component"}). Empty tibble if the note has no
#'   relationships.
#' @export
pb_get_note_relationships <- function(note_id) {
  .pb_get_all(paste0("/notes/", note_id, "/relationships"))
}

# ---- Internal: company/user name resolution + caching ----------------------
#
# NOTE: Company and User are *not* part of the generic Entities API (per
# Productboard's own docs: "Notes reference companies through the note's
# customer relationship — not through the Entities API relationship system").
# That means their exact field shape isn't in the Entities/Notes reference
# pages this module was built against. The `/companies/{id}` and `/users/{id}`
# paths below follow the same `{data: {fields: {...}}}` envelope every other
# v2 endpoint uses, and the relationship target's `links.self` we've seen
# does point at `.../v2/users/{id}` — but this is inferred, not confirmed
# against a live response. If `pb_get_note_relationships()` or the company/
# user lookups below error or come back empty in practice, that's the first
# place to check; the field name to extract may need adjusting.

.pb_name_cache <- new.env(parent = emptyenv())

#' @noRd
.pb_lookup_name <- function(path, id) {
  key <- paste0(path, ":", id)
  if (!is.null(.pb_name_cache[[key]])) return(.pb_name_cache[[key]])

  name <- tryCatch({
    resp   <- httr2::req_perform(.pb_request(paste0(path, "/", id)))
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
    nm <- parsed[["data"]][["fields.name"]]
    if (is.null(nm) || length(nm) == 0) parsed[["data"]][["fields"]][["name"]] else nm
  }, error = function(e) NA_character_)

  if (is.null(name) || length(name) == 0) name <- NA_character_
  .pb_name_cache[[key]] <- name
  name
}

#' @noRd
.pb_company_name <- function(company_id) .pb_lookup_name("/companies", company_id)

#' @noRd
.pb_user_company_name <- function(user_id) {
  key <- paste0("/users:", user_id)
  if (!is.null(.pb_name_cache[[key]])) return(.pb_name_cache[[key]])

  name <- tryCatch({
    resp   <- httr2::req_perform(.pb_request(paste0("/users/", user_id)))
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
    company_id <- parsed[["data"]][["fields.company.id"]]
    if (is.null(company_id) || length(company_id) == 0) {
      NA_character_
    } else {
      .pb_company_name(company_id)
    }
  }, error = function(e) NA_character_)

  if (is.null(name) || length(name) == 0) name <- NA_character_
  .pb_name_cache[[key]] <- name
  name
}
