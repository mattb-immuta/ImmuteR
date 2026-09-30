# Connections adoption, from dashboard 995's "Connections Adoption by Instance"
# tile. The tile reports, per Immuta instance, how many data sources have been
# upgraded to the new connections model out of how many exist. That ratio is
# the migration progress signal; the raw counts are what make it readable, so
# both are kept and the display string carries the pair.
#
# Stored as op_list$consumption$connections by op_list_consumption(), and
# joined onto low_consumption so .rah_account_records() can read it.

.CONN_DASHBOARD <- 995L
.CONN_TILE      <- "connections adoption by instance"

#' Connections adoption per account
#'
#' One row per account with a \code{connections} string summarising how far
#' each Production instance has migrated to the connections model:
#' \code{"12 / 40"} for a single instance, \code{"prod-us: 12 / 40, prod-eu:
#' 3 / 9"} when an account runs several. An instance with nothing upgraded
#' reads \code{"None"} rather than \code{"0 / 40"} — zero progress is a state,
#' not a measurement, and the distinction is the one people scan for.
#'
#' @section Resolving the columns:
#' Looker prefixes a column with the view it came from, and that prefix differs
#' between explores, so the tile's columns are matched by shape rather than
#' pinned by full name: the account column via \code{.looker_account_col()},
#' then the first column looking like an upgraded-count, a total-count, an
#' instance name and an instance type. \code{"Data Sources Count"} is matched
#' with \code{"upgraded"} excluded, so the two counts cannot resolve to the
#' same column. A column that cannot be found is reported by name in the
#' error, which is the fastest way to see that the tile has been renamed.
#'
#' @section Production only:
#' Filtered to \code{Instance Type == "Production"}, matching how
#' \code{\link{op_list_consumption}} scopes every other instance-grain metric.
#' An account whose only instances are Dev/Test therefore gets no row, not a
#' zero — it has no production migration to report on.
#'
#' @param dashboard_id Integer or character. Default \code{995}.
#' @param pattern Character. Tile title substring, matched case-insensitively.
#'   Default \code{"connections adoption by instance"}.
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#'   Defaults to calling \code{looker_auth()}.
#' @param dash Named list from \code{\link{looker_dashboard}}, or \code{NULL}
#'   to fetch it. Pass one you already have to avoid a second API call.
#' @param production_only Logical. Keep only Production instances. Default
#'   \code{TRUE}.
#' @param tile Data frame or \code{NULL}. A tile result you already have — a
#'   saved CSV, say. Supplying it skips the Looker call entirely, which is how
#'   the reshaping can be exercised offline against a fixture.
#' @param base_url Character. Looker instance base URL.
#' @param quiet Logical. Suppress the summary message. Default \code{FALSE}.
#'
#' @return A data frame with one row per account: \code{Account Name},
#'   \code{connections} (the display string), \code{connections_upgraded} and
#'   \code{connections_total} (summed across the account's Production
#'   instances), and \code{connections_ratio} (upgraded over total, or
#'   \code{NA} when there are no data sources at all). Zero rows with the same
#'   columns when the tile returns nothing, so callers never test for NULL.
#'
#' @examples
#' \dontrun{
#'   op_list_connections()
#'
#'   # Confirm the tile and its columns before trusting the match:
#'   tok  <- looker_auth()
#'   dash <- looker_dashboard(995, tok)
#'   vapply(dash$dashboard_elements, function(e) .nz(e$title, "<untitled>"), character(1))
#'   names(looker_tile_data(995, "connections adoption by instance", dash = dash, token = tok))
#' }
#' @export
op_list_connections <- function(dashboard_id    = .CONN_DASHBOARD,
                                pattern         = .CONN_TILE,
                                token           = NULL,
                                dash            = NULL,
                                production_only = TRUE,
                                tile            = NULL,
                                base_url        = Sys.getenv("LOOKER_BASE_URL",
                                                             "https://immuta.cloud.looker.com"),
                                quiet           = FALSE) {

  df <- if (!is.null(tile)) {
    tile
  } else {
    looker_tile_data(dashboard_id, pattern, token = token, dash = dash,
                     base_url = base_url)
  }

  if (is.null(df) || nrow(df) == 0) {
    if (!quiet) message(".. op_list_connections: tile returned no rows")
    return(.conn_empty())
  }

  acct_col <- .looker_account_col(df)
  up_col   <- .looker_col(df, c("upgraded.*data sources.*count",
                                "upgraded.*count", "upgraded"))
  tot_col  <- .looker_col(df, c("data sources count", "data sources"),
                          exclude = "upgraded")
  inst_col <- .looker_col(df, c("instance name", "instances name", "instance$"))
  type_col <- .looker_col(df, c("instance type"))

  missing <- c(
    "account name"   = acct_col,
    "upgraded count" = up_col,
    "data sources count" = tot_col
  )
  if (any(is.na(missing))) {
    stop("op_list_connections: could not find ",
         paste(names(missing)[is.na(missing)], collapse = ", "),
         " on tile '", pattern, "'. Columns present: ",
         paste(names(df), collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    `Account Name` = trimws(as.character(df[[acct_col]])),
    instance       = if (is.na(inst_col)) NA_character_ else trimws(as.character(df[[inst_col]])),
    inst_type      = if (is.na(type_col)) NA_character_ else trimws(as.character(df[[type_col]])),
    upgraded       = .conn_num(df[[up_col]]),
    total          = .conn_num(df[[tot_col]]),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  n_all <- nrow(out)
  if (isTRUE(production_only) && !is.na(type_col)) {
    out <- out[!is.na(out$inst_type) & out$inst_type == "Production", , drop = FALSE]
  } else if (isTRUE(production_only)) {
    warning("op_list_connections: no instance-type column on the tile; ",
            "returning every instance rather than Production only.", call. = FALSE)
  }

  out <- out[!is.na(out$`Account Name`) & nzchar(out$`Account Name`), , drop = FALSE]
  if (nrow(out) == 0) {
    if (!quiet) message(".. op_list_connections: no Production rows in ", n_all, " tile row(s)")
    return(.conn_empty())
  }

  # One row per instance becomes one string per account. The instance name is
  # only worth the width when there is more than one to tell apart, so a
  # single-instance account reads "12 / 40" and a multi-instance one names each.
  split_by <- split(seq_len(nrow(out)), out$`Account Name`)

  rows <- lapply(names(split_by), function(nm) {
    idx   <- split_by[[nm]]
    multi <- length(idx) > 1

    parts <- vapply(idx, function(i) {
      cell <- .conn_cell(out$upgraded[i], out$total[i])
      lbl  <- out$instance[i]
      if (multi && !is.na(lbl) && nzchar(lbl)) paste0(lbl, ": ", cell) else cell
    }, character(1))

    # An account with no measurement at all is NA, not 0. na.rm alone would
    # sum NA to zero and report an unmeasured account as 0% migrated, which
    # sorts it in with the accounts that genuinely have not started — the one
    # distinction the "None" label exists to make.
    measured <- !all(is.na(out$upgraded[idx]))
    up  <- if (measured) sum(out$upgraded[idx], na.rm = TRUE) else NA_real_
    tot <- if (all(is.na(out$total[idx]))) NA_real_ else sum(out$total[idx], na.rm = TRUE)

    data.frame(
      `Account Name`        = nm,
      connections           = paste(parts, collapse = ", "),
      connections_upgraded  = up,
      connections_total     = tot,
      # No data sources at all has no ratio either. Zero over zero is not 0%
      # adoption, it is nothing to adopt yet.
      connections_ratio     = if (is.na(up) || is.na(tot) || tot == 0) NA_real_ else up / tot,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, rows)
  res <- res[order(res$`Account Name`), , drop = FALSE]
  rownames(res) <- NULL

  if (!quiet) {
    none <- sum(res$connections_upgraded == 0, na.rm = TRUE)
    message(".. op_list_connections [dash ", dashboard_id, "]: ", nrow(res),
            " account(s) from ", nrow(out), " Production instance(s); ",
            none, " with nothing upgraded")
  }

  res
}


# ── Internal ──────────────────────────────────────────────────────────────────

.conn_empty <- function() {
  data.frame(`Account Name` = character(0), connections = character(0),
             connections_upgraded = numeric(0), connections_total = numeric(0),
             connections_ratio = numeric(0),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# Looker hands counts back as formatted strings ("1,204") often enough that
# as.numeric() alone silently produces NA.
.conn_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))

# One instance's cell. Nothing upgraded reads "None": a bare "0 / 40" invites
# the eye to compare it with "3 / 40" as though they were the same kind of
# thing, when the first means the migration has not started.
.conn_cell <- function(upgraded, total) {
  if (is.na(upgraded)) return("—")
  if (upgraded == 0)   return("None")
  paste0(format(upgraded, big.mark = ",", trim = TRUE), " / ",
         if (is.na(total)) "—" else format(total, big.mark = ",", trim = TRUE))
}
