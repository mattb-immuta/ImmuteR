#' Authenticate with the Looker API
#'
#' Reads credentials from environment variables \code{LOOKER_CLIENT_ID} and
#' \code{LOOKER_CLIENT_SECRET}. The base URL defaults to
#' \code{LOOKER_BASE_URL} or \code{"https://immuta.cloud.looker.com"}.
#'
#' @return Character. Bearer token for use in subsequent API calls.
#' @export
looker_auth <- function() {
  base_url      <- Sys.getenv("LOOKER_BASE_URL", "https://immuta.cloud.looker.com")
  client_id     <- Sys.getenv("LOOKER_CLIENT_ID")
  client_secret <- Sys.getenv("LOOKER_CLIENT_SECRET")

  if (client_id == "" || client_secret == "") {
    stop(
      "Looker credentials not found. ",
      "Set LOOKER_CLIENT_ID and LOOKER_CLIENT_SECRET environment variables."
    )
  }

  resp <- httr2::request(paste0(base_url, "/api/4.0/login")) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(client_id = client_id, client_secret = client_secret) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)$access_token
}

#' Run a saved Looker look and return a data frame
#'
#' Calls \code{GET /api/4.0/looks/{look_id}/run/csv} on the Looker instance
#' and parses the response into a tibble.
#'
#' @param look_id       Integer or character. The numeric look ID to run.
#' @param token         Character. Bearer token from \code{looker_auth()}.
#' @param result_format Character. \code{"csv"} (default) or \code{"json"}.
#' @param base_url      Character. Looker instance base URL.
#'
#' @return A tibble with the look's result rows.
#' @export
run_looker_look <- function(look_id,
                             token,
                             result_format = "csv",
                             base_url = Sys.getenv("LOOKER_BASE_URL",
                                                    "https://immuta.cloud.looker.com")) {
  url <- paste0(base_url, "/api/4.0/looks/", look_id, "/run/", result_format)

  resp <- httr2::request(url) |>
    httr2::req_headers(Authorization = paste("token", token)) |>
    httr2::req_perform()

  if (result_format == "csv") {
    readr::read_csv(httr2::resp_body_string(resp), show_col_types = FALSE)
  } else {
    httr2::resp_body_json(resp, simplifyVector = TRUE) |> tibble::as_tibble()
  }
}

# ── Dashboard introspection ───────────────────────────────────────────────────
# The looks in .OP_LIST_LOOKS were picked by hand off saved looks. Dashboards
# are the other half of Looker's surface: to reuse a dashboard's metrics in R we
# need its tile-level query definitions (model/explore/fields/filters), which
# only the API exposes. These helpers read that structure and run individual
# tiles, so a dashboard can be turned into an op_list data frame without
# re-deriving its LookML by hand.

# Bare GET against the Looker API, parsed as an R list. Internal — the exported
# wrappers below all funnel through here.
.looker_get <- function(path,
                        token,
                        query = list(),
                        base_url = Sys.getenv("LOOKER_BASE_URL",
                                              "https://immuta.cloud.looker.com")) {
  req <- httr2::request(paste0(base_url, "/api/4.0/", sub("^/", "", path))) |>
    httr2::req_headers(Authorization = paste("token", token))

  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)

  httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)
}

# A dashboard tile can carry its query in one of three places depending on how
# it was built (ad-hoc tile, tile from a saved look, or a merged/result-maker
# tile). Return whichever one is populated, or NULL.
.tile_query <- function(el) {
  .nz(el$query,
      .nz(el$result_maker$query, el$look$query))
}

# "x unless it's empty, else y". Deliberately not another `%||%` — the package
# already carries two subtly different definitions of that operator
# (gmail_mail_merge.R and op_list_support.R; the latter wins on collate order
# and doesn't treat length-0 as missing), and these helpers need the length-0
# case handled.
.nz <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Fetch a Looker dashboard definition
#'
#' Calls \code{GET /api/4.0/dashboards/{dashboard_id}} and returns the parsed
#' JSON as a nested R list. Useful raw material when you need something
#' \code{\link{looker_dashboard_elements}} does not surface (LookML links,
#' visualisation config, cross-filter setup).
#'
#' @param dashboard_id Integer or character. Numeric ID for a user-defined
#'   dashboard (e.g. \code{1009}), or \code{"model::name"} for a LookML one.
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#'   Defaults to calling \code{looker_auth()}.
#' @param base_url Character. Looker instance base URL.
#'
#' @return Named list — the Looker \code{Dashboard} object, including
#'   \code{dashboard_elements} and \code{dashboard_filters}.
#' @export
looker_dashboard <- function(dashboard_id,
                             token = NULL,
                             base_url = Sys.getenv("LOOKER_BASE_URL",
                                                   "https://immuta.cloud.looker.com")) {
  if (is.null(token)) token <- looker_auth()
  .looker_get(paste0("dashboards/", dashboard_id), token, base_url = base_url)
}

#' Summarise a Looker dashboard's tiles as a data frame
#'
#' Flattens \code{\link{looker_dashboard}} into one row per tile, with the
#' fields, filters, pivots and sorts of each tile's query collapsed to
#' \code{" | "}-separated strings. This is the table to read when deciding
#' which dashboard metrics to pull into \code{op_list}.
#'
#' @param dashboard_id Integer or character, or a list already returned by
#'   \code{\link{looker_dashboard}} (avoids a second API call).
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#' @param base_url Character. Looker instance base URL.
#'
#' @return A tibble with columns \code{element_id}, \code{title},
#'   \code{type}, \code{look_id}, \code{query_id}, \code{model},
#'   \code{explore}, \code{fields}, \code{filters}, \code{pivots},
#'   \code{sorts}, \code{row_limit}.
#' @export
looker_dashboard_elements <- function(dashboard_id,
                                      token = NULL,
                                      base_url = Sys.getenv("LOOKER_BASE_URL",
                                                            "https://immuta.cloud.looker.com")) {
  dash <- if (is.list(dashboard_id)) {
    dashboard_id
  } else {
    looker_dashboard(dashboard_id, token, base_url)
  }

  els <- dash$dashboard_elements
  if (length(els) == 0) {
    return(tibble::tibble())
  }

  .join <- function(x) if (length(x) == 0) NA_character_ else paste(unlist(x), collapse = " | ")

  # Filters arrive as a named list (field -> expression); keep both halves so a
  # tile's scope ("only Production instances", "last 4 weeks") is legible.
  .join_filters <- function(x) {
    if (length(x) == 0) return(NA_character_)
    paste(paste0(names(x), " = ", unlist(x)), collapse = " | ")
  }

  rows <- lapply(els, function(el) {
    q <- .tile_query(el)
    tibble::tibble(
      element_id = .join(el$id),
      title      = .join(.nz(el$title, .nz(el$title_text, el$look$title))),
      type       = .join(el$type),
      look_id    = .join(el$look_id),
      query_id   = .join(q$id),
      model      = .join(q$model),
      explore    = .join(q$view),
      fields     = .join(q$fields),
      filters    = .join_filters(q$filters),
      pivots     = .join(q$pivots),
      sorts      = .join(q$sorts),
      row_limit  = .join(q$limit)
    )
  })

  dplyr::bind_rows(rows)
}

#' Run one tile of a Looker dashboard and return its rows
#'
#' Re-runs a tile's saved query by ID (\code{GET
#' /api/4.0/queries/{query_id}/run/csv}), falling back to
#' \code{\link{run_looker_look}} for tiles backed by a saved look and, failing
#' both, to re-creating the query from the tile's own definition
#' (\code{POST /api/4.0/queries}). The last path is what handles tiles whose
#' saved query ID has expired — Looker garbage-collects those — and tiles built
#' ad hoc on the dashboard rather than from a look.
#'
#' Note that dashboard-level filters (the \code{?Type=Customer&Instance Type=}
#' style parameters in a dashboard URL) are \strong{not} applied — those are
#' merged into the query at render time by the Looker UI. Filter the returned
#' data frame in R instead, the way \code{\link{op_list_consumption}} filters
#' to \code{Instance Type == "Production"}.
#'
#' @param element A single tile, i.e. one element of
#'   \code{looker_dashboard(...)$dashboard_elements}.
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#' @param base_url Character. Looker instance base URL.
#'
#' @return A tibble of the tile's result rows, or \code{NULL} when the tile
#'   carries no runnable query (text tiles, for instance).
#' @export
run_looker_dashboard_tile <- function(element,
                                      token = NULL,
                                      base_url = Sys.getenv("LOOKER_BASE_URL",
                                                            "https://immuta.cloud.looker.com")) {
  if (is.null(token)) token <- looker_auth()

  q <- .tile_query(element)

  # 1. Saved query ID — cheapest, but the ID can be gone.
  if (!is.null(q$id)) {
    got <- tryCatch(
      .looker_run_query_id(q$id, token, base_url),
      error = function(e) NULL
    )
    if (!is.null(got)) return(got)
  }

  # 2. Saved look behind the tile.
  if (!is.null(element$look_id)) {
    got <- tryCatch(
      run_looker_look(element$look_id, token, base_url = base_url),
      error = function(e) NULL
    )
    if (!is.null(got)) return(got)
  }

  # 3. Rebuild the query from the tile's definition and run that.
  if (!is.null(q$model) && !is.null(q$view) && length(q$fields) > 0) {
    return(.looker_run_query_def(q, token, base_url))
  }

  NULL
}

# GET /queries/{id}/run/csv
.looker_run_query_id <- function(query_id, token, base_url) {
  resp <- httr2::request(paste0(base_url, "/api/4.0/queries/", query_id, "/run/csv")) |>
    httr2::req_headers(Authorization = paste("token", token)) |>
    httr2::req_perform()
  readr::read_csv(httr2::resp_body_string(resp), show_col_types = FALSE)
}

# POST /queries to register the tile's query definition, then run the ID it
# hands back. Only the fields Looker needs to reproduce the tile are sent;
# passing the whole element back (vis config, client_id, and so on) is rejected.
.looker_run_query_def <- function(q, token, base_url) {
  body <- list(
    model  = q$model,
    view   = q$view,
    fields = q$fields
  )
  for (k in c("filters", "pivots", "sorts", "limit", "column_limit",
              "filter_expression", "fill_fields", "dynamic_fields",
              "row_total", "subtotals")) {
    if (!is.null(q[[k]]) && length(q[[k]]) > 0) body[[k]] <- q[[k]]
  }

  created <- httr2::request(paste0(base_url, "/api/4.0/queries")) |>
    httr2::req_headers(Authorization = paste("token", token)) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)

  .looker_run_query_id(created$id, token, base_url)
}

# Find the account-name column in a Looker result frame. The tiles on the
# customer dashboards call it `Salesforce Accounts Account Name`; accept
# anything ending in "account name" so a differently-scoped explore matches.
.looker_account_col <- function(df) {
  nms   <- names(df)
  exact <- nms[nms == "Salesforce Accounts Account Name" | nms == "Account Name"]
  if (length(exact) > 0) return(exact[1])
  cand <- nms[grepl("account name$", tolower(nms))]
  if (length(cand) > 0) cand[1] else NA_character_
}

#' Account names listed on a Looker dashboard tile
#'
#' Runs the first tile whose title contains \code{pattern} and returns the
#' account names it lists. This is how the star/stage tiles on the feature
#' dashboards (1009 Request, 1053, and so on) are read into R: each lists the
#' customers that qualify for a stage, so membership \emph{is} the metric.
#'
#' Tiles carrying a \code{Show row} column can be gated on it. That column is a
#' Looker table calculation deciding which rows actually count toward the
#' stage, so reading a gated tile without it overstates membership — but it is
#' not present on, or meaningful for, every tile, hence the argument.
#'
#' Dashboard-level filters in a dashboard URL (\code{?Type=Customer} and the
#' like) are not applied when a tile is run through the API; see
#' \code{\link{run_looker_dashboard_tile}}.
#'
#' @param dashboard_id Integer or character. Ignored when \code{dash} is given.
#' @param pattern Character. Substring matched case-insensitively against tile
#'   titles, e.g. \code{"retained accounts"}. The first match wins and extra
#'   matches are reported, so prefer the longest unambiguous substring — a
#'   dashboard with "Retained", "Retained History" and "Retained Accounts"
#'   tiles will hand back whichever comes first for \code{"retained"}.
#' @param show_row Logical. Filter the tile to \code{`Show row` == "Yes"} when
#'   that column exists. Defaults to \code{TRUE}.
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#' @param dash Named list from \code{\link{looker_dashboard}}, or \code{NULL}
#'   to fetch it. Pass one you already have to avoid a second call.
#' @param base_url Character. Looker instance base URL.
#'
#' @return A character vector of unique, whitespace-trimmed account names.
#'   \code{character(0)} (with a warning) when no tile matches, the tile is
#'   empty, or it has no account-name column.
#'
#' @examples
#' \dontrun{
#'   tok  <- looker_auth()
#'   dash <- looker_dashboard(1053, tok)
#'
#'   # what the tiles are called
#'   vapply(dash$dashboard_elements,
#'          function(e) .nz(e$title, "<untitled>"), character(1))
#'
#'   looker_tile_accounts(1053, "retained", dash = dash, token = tok)
#' }
#' @export
looker_tile_accounts <- function(dashboard_id,
                                 pattern,
                                 show_row = TRUE,
                                 token    = NULL,
                                 dash     = NULL,
                                 base_url = Sys.getenv("LOOKER_BASE_URL",
                                                       "https://immuta.cloud.looker.com")) {
  df <- looker_tile_data(dashboard_id, pattern, show_row = show_row,
                         token = token, dash = dash, base_url = base_url)
  if (is.null(df) || nrow(df) == 0) return(character(0))

  acct_col <- .looker_account_col(df)
  if (is.na(acct_col)) {
    warning("Tile matching '", pattern,
            "' has no account-name column; columns: ",
            paste(names(df), collapse = ", "), call. = FALSE)
    return(character(0))
  }

  unique(trimws(as.character(df[[acct_col]])))
}

#' Rows of a Looker dashboard tile
#'
#' The data frame behind \code{\link{looker_tile_accounts}}. Use this when the
#' tile carries measures you need rather than membership alone — the
#' "Connections Adoption by Instance" tile on dashboard 995, for instance,
#' where the counts are the point and the account list is incidental.
#'
#' Tile titles are matched case-insensitively on a substring; the first match
#' wins and extra matches are reported. Dashboard-level filters are not
#' applied — see \code{\link{run_looker_dashboard_tile}} — so filter the result
#' in R.
#'
#' @param dashboard_id Integer or character. Ignored when \code{dash} is given.
#' @param pattern Character. Substring matched against tile titles.
#' @param show_row Logical. Filter to \code{`Show row` == "Yes"} when that
#'   column exists. Default \code{TRUE}.
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#' @param dash Named list from \code{\link{looker_dashboard}}, or \code{NULL}.
#' @param base_url Character. Looker instance base URL.
#'
#' @return A tibble of the tile's rows, or \code{NULL} with a warning when no
#'   tile matches or the tile has no runnable query.
#' @export
looker_tile_data <- function(dashboard_id,
                             pattern,
                             show_row = TRUE,
                             token    = NULL,
                             dash     = NULL,
                             base_url = Sys.getenv("LOOKER_BASE_URL",
                                                   "https://immuta.cloud.looker.com")) {
  if (is.null(token)) token <- looker_auth()
  if (is.null(dash))  dash  <- looker_dashboard(dashboard_id, token, base_url)

  # Titles are lowercased for the comparison, so the pattern has to be too —
  # otherwise "Retained" silently matches nothing while "retained" works.
  pattern <- tolower(trimws(as.character(pattern)))

  hits <- Filter(
    function(e) grepl(pattern, tolower(.nz(e$title, "")), fixed = TRUE),
    dash$dashboard_elements
  )

  if (length(hits) == 0) {
    warning("No tile title matched '", pattern, "' on the dashboard. Titles: ",
            paste(vapply(dash$dashboard_elements,
                         function(e) .nz(e$title, "<untitled>"), character(1)),
                  collapse = " | "),
            call. = FALSE)
    return(NULL)
  }
  if (length(hits) > 1) {
    message(".. '", pattern, "' matched ", length(hits), " tiles; using '",
            .nz(hits[[1]]$title, "<untitled>"), "'")
  }

  df <- run_looker_dashboard_tile(hits[[1]], token, base_url)
  if (is.null(df) || nrow(df) == 0) return(df)

  if (isTRUE(show_row) && "Show row" %in% names(df)) {
    df <- df[tolower(trimws(as.character(df[["Show row"]]))) == "yes", ,
             drop = FALSE]
  }
  df
}

# First column whose name matches `patterns` in order, excluding any matching
# `exclude`. Looker prefixes a column with its view name, and that prefix
# changes between explores, so the tile columns are resolved by shape rather
# than pinned by full name — the same reasoning as .looker_account_col().
.looker_col <- function(df, patterns, exclude = NULL) {
  nms <- names(df)
  for (p in patterns) {
    cand <- nms[grepl(p, nms, ignore.case = TRUE)]
    if (!is.null(exclude)) {
      cand <- cand[!grepl(exclude, cand, ignore.case = TRUE)]
    }
    if (length(cand) > 0) return(cand[1])
  }
  NA_character_
}

#' Dump a Looker dashboard's structure (and optionally its data) to disk
#'
#' Writes three things into \code{dir}: the full dashboard JSON, a tile
#' summary CSV from \code{\link{looker_dashboard_elements}}, and — when
#' \code{data = TRUE} — one CSV per runnable tile. Intended as a one-off
#' exploration step before wiring a dashboard's metrics into \code{op_list},
#' not as part of the pipeline.
#'
#' @param dashboard_id Integer or character. The dashboard to dump.
#' @param dir Character. Output directory, created if missing. Default
#'   \code{"data/looker"}.
#' @param data Logical. Also run every tile and save its rows. Default
#'   \code{FALSE} — tile queries can be slow.
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#' @param base_url Character. Looker instance base URL.
#'
#' @return Invisibly, a character vector of the files written.
#' @export
dump_looker_dashboard <- function(dashboard_id,
                                  dir = "data/looker",
                                  data = FALSE,
                                  token = NULL,
                                  base_url = Sys.getenv("LOOKER_BASE_URL",
                                                        "https://immuta.cloud.looker.com")) {
  if (is.null(token)) token <- looker_auth()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  slug <- gsub("[^A-Za-z0-9]+", "_", as.character(dashboard_id))

  message(glue::glue(".. dump_looker_dashboard: fetching dashboard {dashboard_id}"))
  dash <- looker_dashboard(dashboard_id, token, base_url)

  json_path <- file.path(dir, paste0("dashboard_", slug, ".json"))
  jsonlite::write_json(dash, json_path, auto_unbox = TRUE, pretty = TRUE, null = "null")

  els <- looker_dashboard_elements(dash)
  csv_path <- file.path(dir, paste0("dashboard_", slug, "_tiles.csv"))
  readr::write_csv(els, csv_path)

  written <- c(json_path, csv_path)

  # Dashboard-level filters, so the tile scopes above can be read in context.
  filters <- dash$dashboard_filters
  if (length(filters) > 0) {
    filt_df <- dplyr::bind_rows(lapply(filters, function(f) {
      tibble::tibble(
        name          = .nz(f$name,          NA_character_),
        title         = .nz(f$title,         NA_character_),
        type          = .nz(f$type,          NA_character_),
        model         = .nz(f$model,         NA_character_),
        explore       = .nz(f$explore,       NA_character_),
        dimension     = .nz(f$dimension,     NA_character_),
        default_value = .nz(f$default_value, NA_character_)
      )
    }))
    filt_path <- file.path(dir, paste0("dashboard_", slug, "_filters.csv"))
    readr::write_csv(filt_df, filt_path)
    written <- c(written, filt_path)
  }

  if (isTRUE(data)) {
    for (el in dash$dashboard_elements) {
      title <- .nz(el$title, .nz(el$look$title, paste0("element_", el$id)))
      nm    <- gsub("[^A-Za-z0-9]+", "_", tolower(title))
      message(glue::glue(".. tile: {title}"))

      df <- tryCatch(run_looker_dashboard_tile(el, token, base_url),
                     error = function(e) {
                       message(glue::glue("   !! failed: {conditionMessage(e)}"))
                       NULL
                     })
      if (is.null(df) || nrow(df) == 0) next

      p <- file.path(dir, paste0("dashboard_", slug, "_tile_", nm, ".csv"))
      readr::write_csv(df, p)
      written <- c(written, p)
    }
  }

  message(glue::glue(".. dump_looker_dashboard: wrote {length(written)} file(s) to {dir}"))
  invisible(written)
}
