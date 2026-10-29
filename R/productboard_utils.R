#' Productboard API integration
#'
#' Two ways to authenticate, set in \code{.Renviron}
#' (\code{usethis::edit_r_environ()}, then restart R). The API token wins if
#' both are present.
#'
#' ## Option A: Personal access token
#' Productboard Settings > Integrations > Public APIs > Access Token, click
#' "+", copy it, and set \code{PRODUCTBOARD_API_TOKEN=...}.
#'
#' ## Option B: Registered OAuth app (authorization code)
#' Set \code{PRODUCTBOARD_CLIENT_ID} and \code{PRODUCTBOARD_CLIENT_SECRET} from
#' \url{https://app.productboard.com/oauth2/applications}. On the app:
#' \itemize{
#'   \item add \code{http://localhost:1410/} as a redirect URI (or set
#'     \code{PRODUCTBOARD_REDIRECT_URI} to one already registered — it must
#'     match exactly);
#'   \item grant \code{notes:read} and \code{entities:read}, plus
#'     \code{members_pii:read} to see owner/creator emails.
#' }
#' The first request opens a browser; approve the app (only "Makers with admin
#' access" can). httr2 caches the tokens on disk and refreshes them silently;
#' after 180 days unused you'll be asked to approve again.
#'
#' Run \code{pb_check_connection()} to confirm access before a full pull.
#'
#' @name productboard
NULL

# ---- Internal: shared request builder --------------------------------------

.pb_api_base <- "https://api.productboard.com/v2"

# ---- Internal: authentication ------------------------------------------------
#
# Two ways in, checked in this order:
#   1. PRODUCTBOARD_API_TOKEN  — a personal access token (Settings > Integrations
#      > Public APIs > Access Token). Sent as a Bearer token.
#   2. PRODUCTBOARD_CLIENT_ID + PRODUCTBOARD_CLIENT_SECRET — a registered OAuth
#      app (app.productboard.com/oauth2/applications), authorization-code flow.
#      The first request opens a browser to approve the app; httr2 then caches
#      the access + refresh tokens on disk (cache key "productboard") and
#      refreshes them silently (access token 24h, refresh token 180 days).
#      The redirect URI must exactly match one registered on the app; default
#      http://localhost:1410/, override with PRODUCTBOARD_REDIRECT_URI.
#      Scopes are the ones set on the app registration — it needs notes:read
#      and entities:read, plus members_pii:read for owner/creator emails.

.pb_oauth_urls <- list(
  authorize = "https://app.productboard.com/oauth2/authorize",
  token     = "https://app.productboard.com/oauth2/token"
)

# First non-empty of several env var spellings.
#' @noRd
.pb_env <- function(...) {
  for (v in c(...)) { x <- Sys.getenv(v); if (nzchar(x)) return(x) }
  ""
}

.pb_id_vars     <- c("PRODUCTBOARD_CLIENT_ID", "PB_CLIENT_ID", "PRODUCTBOARD_OAUTH_CLIENT_ID")
.pb_secret_vars <- c("PRODUCTBOARD_CLIENT_SECRET", "PB_CLIENT_SECRET", "PRODUCTBOARD_OAUTH_CLIENT_SECRET")

# Env value with stray quotes / whitespace removed — .Renviron values pasted
# as "abc" or with a trailing space are a common cause of invalid_client.
#' @noRd
.pb_clean_secret <- function(x) gsub("^[\"' ]+|[\"' \r\n\t]+$", "", x)

#' @noRd
.pb_oauth_client <- function() {
  id     <- .pb_clean_secret(.pb_env(.pb_id_vars))
  secret <- .pb_clean_secret(.pb_env(.pb_secret_vars))
  if (!nzchar(id) || !nzchar(secret)) return(NULL)
  # "body" (default) sends client_id/secret as form fields; "header" uses HTTP
  # Basic. Switch with PRODUCTBOARD_OAUTH_AUTH if pb_oauth_diagnose() says so.
  auth <- .pb_env("PRODUCTBOARD_OAUTH_AUTH")
  if (!auth %in% c("body", "header")) auth <- "body"
  httr2::oauth_client(id = id, secret = secret, token_url = .pb_oauth_urls$token,
                      auth = auth, name = "productboard")
}

#' Diagnose Productboard OAuth client credentials
#'
#' Checks the client ID / secret in \code{.Renviron} without printing them:
#' which variables were read, their lengths, stray quotes or whitespace, and —
#' by calling the token-revoke endpoint with a dummy token, which validates
#' the client but revokes nothing — whether Productboard accepts them when
#' sent as form fields, as URL query parameters, or as HTTP Basic auth. Also
#' reports whether a cached OAuth token exists.
#'
#' @param reset Logical. If \code{TRUE}, delete the cached OAuth token so the
#'   next request re-runs the browser approval.
#' @return Invisibly, a data frame of the credential checks.
#' @export
pb_oauth_diagnose <- function(reset = FALSE) {
  which_var <- function(vars) { hit <- vars[nzchar(Sys.getenv(vars))]; if (length(hit)) hit[1] else NA }
  id_var <- which_var(.pb_id_vars); sec_var <- which_var(.pb_secret_vars)
  raw_id  <- if (is.na(id_var)) "" else Sys.getenv(id_var)
  raw_sec <- if (is.na(sec_var)) "" else Sys.getenv(sec_var)
  id <- .pb_clean_secret(raw_id); sec <- .pb_clean_secret(raw_sec)

  cat("Client ID variable:     ", ifelse(is.na(id_var), "(none found)", id_var),
      " | length ", nchar(id), if (!identical(id, raw_id)) "  ** had quotes/whitespace (now stripped) **", "\n", sep = "")
  cat("Client secret variable: ", ifelse(is.na(sec_var), "(none found)", sec_var),
      " | length ", nchar(sec), if (!identical(sec, raw_sec)) "  ** had quotes/whitespace (now stripped) **", "\n", sep = "")
  if (nzchar(Sys.getenv("PRODUCTBOARD_API_TOKEN")))
    cat("Note: PRODUCTBOARD_API_TOKEN is also set, and takes precedence over the OAuth app.\n")
  if (!nzchar(id) || !nzchar(sec)) { cat("Missing ID or secret — nothing to test.\n"); return(invisible(NULL)) }

  revoke <- "https://app.productboard.com/oauth2/revoke"
  try_style <- function(style) {
    req <- httr2::request(revoke)
    req <- switch(style,
      body   = httr2::req_body_form(req, client_id = id, client_secret = sec, token = "diagnostic-dummy-token"),
      query  = httr2::req_method(httr2::req_url_query(req, client_id = id, client_secret = sec,
                                                      token = "diagnostic-dummy-token"), "POST"),
      header = httr2::req_body_form(httr2::req_auth_basic(req, id, sec), token = "diagnostic-dummy-token"))
    req  <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) return(data.frame(style = style, status = NA, result = conditionMessage(resp)))
    body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    data.frame(style = style, status = httr2::resp_status(resp),
               result = if (httr2::resp_status(resp) < 300) "credentials accepted" else substr(body, 1, 160))
  }
  res <- rbind(try_style("body"), try_style("query"), try_style("header"))
  cat("\nCredential check against /oauth2/revoke (dummy token, nothing is revoked):\n")
  print(res, row.names = FALSE)

  ok <- res$style[res$status %in% 200:299]
  if (!length(ok)) {
    cat("\nProductboard rejected the ID/secret every way: re-copy both from",
        "app.productboard.com/oauth2/applications (check they aren't swapped, or the secret wasn't regenerated),",
        "update .Renviron, restart R, then run pb_oauth_diagnose(reset = TRUE).\n")
  } else if (!"body" %in% ok && "header" %in% ok) {
    cat("\nOnly Basic auth works: add PRODUCTBOARD_OAUTH_AUTH=header to .Renviron and restart R.\n")
  } else if (!any(c("body", "header") %in% ok)) {
    cat("\nOnly query-string credentials work — tell Claude; the token exchange needs a custom request.\n")
  } else {
    cat("\nCredentials are fine. If the browser step still fails, the error is elsewhere",
        "(redirect URI or scopes) — run with reset = TRUE and share the full error message.\n")
  }

  # httr2 names cached tokens by a hash, not the client name, so this lists
  # every httr2 OAuth token on the machine. In this project only Productboard
  # uses httr2's disk cache; clearing others just means another browser login.
  cache <- tryCatch(httr2::oauth_cache_path(), error = function(e) NA)
  if (!is.na(cache)) {
    cached <- list.files(cache, pattern = "-token\\.rds", full.names = TRUE)
    cat("\nCached httr2 OAuth token files in ", cache, ": ", length(cached), "\n", sep = "")
    if (reset && length(cached)) { unlink(cached); cat("Deleted — the next request will open the browser again.\n") }
  }
  invisible(res)
}

# Adds auth to a request: API token if set, else the OAuth app, else a clear
# error naming both options.
#' @noRd
.pb_auth <- function(req) {
  token <- Sys.getenv("PRODUCTBOARD_API_TOKEN")
  if (nzchar(token)) return(httr2::req_auth_bearer_token(req, token))

  client <- .pb_oauth_client()
  if (!is.null(client)) {
    redirect <- .pb_env("PRODUCTBOARD_REDIRECT_URI", "PB_REDIRECT_URI")
    if (!nzchar(redirect)) redirect <- "http://localhost:1410/"
    return(httr2::req_oauth_auth_code(
      req, client,
      auth_url     = .pb_oauth_urls$authorize,
      redirect_uri = redirect,
      cache_disk   = TRUE,
      cache_key    = "productboard"
    ))
  }

  stop(
    "No Productboard credentials found. Set either PRODUCTBOARD_API_TOKEN, or ",
    "PRODUCTBOARD_CLIENT_ID + PRODUCTBOARD_CLIENT_SECRET (a registered OAuth app), ",
    "in your .Renviron (usethis::edit_r_environ()), then restart R. See ?productboard.",
    call. = FALSE
  )
}

#' @noRd
.pb_request <- function(path) {
  req <- httr2::request(paste0(.pb_api_base, path))
  req <- .pb_auth(req)
  req <- httr2::req_headers(req, Accept = "application/json")
  httr2::req_retry(req, max_tries = 3, backoff = ~ 2)
}

#' Check the Productboard connection and what it can see
#'
#' Makes one small request to each endpoint the insights pull uses and reports
#' which worked — the quickest way to confirm credentials and OAuth scopes
#' before a full \code{\link{pb_pull_insights}}. With an OAuth app, the first
#' call opens a browser to authorize it.
#'
#' @return Invisibly, a data frame of endpoint / ok / detail.
#' @export
pb_check_connection <- function() {
  method <- if (nzchar(Sys.getenv("PRODUCTBOARD_API_TOKEN"))) "API token" else
            if (!is.null(.pb_oauth_client())) "OAuth app (authorization code)" else "none"
  message("Productboard auth method: ", method)

  probe <- function(label, path, query = list()) {
    req <- .pb_request(path)
    if (length(query)) req <- httr2::req_url_query(req, !!!query)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) return(data.frame(endpoint = label, ok = FALSE, detail = conditionMessage(resp)))
    st <- httr2::resp_status(resp)
    body <- tryCatch(jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE), error = function(e) list())
    detail <- if (st < 300) paste0(length(body$data), " rows on first page") else
      paste0("HTTP ", st, ": ", .pb_nz(body$errors[[1]]$detail, .pb_nz(body$errors[[1]]$title, "")))
    data.frame(endpoint = label, ok = st < 300, detail = detail)
  }

  out <- rbind(
    probe("notes",     "/notes"),
    probe("companies", "/entities", list(`type[]` = "company")),
    probe("users",     "/entities", list(`type[]` = "user")),
    probe("features",  "/entities", list(`type[]` = "feature"))
  )
  print(out, row.names = FALSE)
  invisible(out)
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

# Company / user / note-level analysis lives in R/productboard_insights.R:
# pb_pull_insights() lists companies and users as entity types
# (/v2/entities?type[]=company|user) and reads each note's relationships
# inline from /v2/notes, rather than looking names up one id at a time.
