# Look IDs backing the CPO op_list consumption spreadsheet
# (All Customers + Active SKUs – https://docs.google.com/spreadsheets/d/1SAvZo1bUpKM3OlGz4a-wK8M5Nhfni6kbMOI6z8JIW0g)
.OP_LIST_LOOKS <- list(
  data_sources  = 1744L,   # RAW DATA - DATA SOURCES
  queries       = 1745L,   # RAW DATA - QUERIES
  users         = 1746L,   # RAW DATA - USERS
  policies      = 1747L,   # RAW DATA - POLICIES
  sdd_active    = 1748L,   # RAW DATA - SDD ACTIVE
  support_cases = 1776L    # RAW DATA - SUPPORT CASES
)

# ── Internal helpers ──────────────────────────────────────────────────────────

.parse_currency <- function(x) {
  as.numeric(gsub(",", "", gsub("[$]", "", x)))
}

.parse_pct <- function(x) {
  # "190%" -> 1.90 ;  "10%" -> 0.10
  as.numeric(gsub("%", "", x)) / 100
}

# ── Public API ────────────────────────────────────────────────────────────────

#' Load raw op_list data from Looker
#'
#' Fetches each of the six Looker looks that back the CPO op_list consumption
#' spreadsheet. Results are optionally cached as parquet files so subsequent
#' calls skip the network round-trip.
#'
#' @param token      Character. Bearer token from \code{looker_auth()}.
#'   Defaults to calling \code{looker_auth()} automatically.
#' @param cache_path Character or NULL. Directory for parquet caches. Set to
#'   \code{"data/op_list/"} (relative to project root) to enable caching.
#' @param refresh    Logical. If \code{TRUE}, re-fetch from Looker even when a
#'   cache file exists. Default \code{FALSE}.
#'
#' @return Named list of tibbles: \code{users}, \code{data_sources},
#'   \code{queries}, \code{policies}, \code{sdd_active}, \code{support_cases}.
#' @export
load_op_list_raw <- function(token     = NULL,
                              cache_path = NULL,
                              refresh    = FALSE) {
  if (is.null(token)) token <- looker_auth()

  raw <- lapply(names(.OP_LIST_LOOKS), function(name) {
    look_id    <- .OP_LIST_LOOKS[[name]]
    cache_file <- if (!is.null(cache_path))
      file.path(cache_path, paste0("op_list_", name, ".parquet"))
    else
      NULL

    if (!is.null(cache_file) && file.exists(cache_file) && !refresh) {
      message(glue::glue(".. {name}: loading from cache"))
      return(arrow::read_parquet(cache_file))
    }

    message(glue::glue(".. {name}: fetching look {look_id} from Looker"))
    df <- run_looker_look(look_id, token)

    if (!is.null(cache_file)) {
      dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(df, cache_file)
    }
    df
  })

  names(raw) <- names(.OP_LIST_LOOKS)
  raw
}

#' Build the consumption child list for op_list
#'
#' Reproduces the \strong{Stats}, \strong{Renewals}, and \strong{Low
#' Consumption Customers} worksheets from the CPO op_list consumption
#' spreadsheet as R data frames, and returns them as a named list suitable
#' for storing as \code{op_list$consumption}.
#'
#' Called automatically by \code{\link{enrich_opportunities}} — pass
#' \code{looker = TRUE} there to include live Looker metrics. Can also be
#' called directly to refresh consumption data without re-running the full
#' SFDC pull.
#'
#' SFDC data comes from \code{op_list$orders} (report \code{00OVV000005RTxV2AW}),
#' the same source that backs the \emph{Data} sheet in the CPO spreadsheet.
#'
#' @param op_list Named list from \code{\link{enrich_opportunities}}.
#'   Must contain \code{$orders}.
#' @param raw Named list from \code{\link{load_op_list_raw}}, or \code{NULL}
#'   (default). When \code{NULL}, SFDC-derived stats and the contract base are
#'   still built; Looker columns (\code{Users}, \code{Data Sources},
#'   \code{License Consumption}, etc.) will be \code{NA}.
#' @param low_consumption_threshold Numeric. Fraction below which a customer is
#'   flagged as low consumption. Default \code{0.20} (20\%).
#'
#' @return Named list stored as \code{op_list$consumption}, with elements:
#' \describe{
#'   \item{\code{stats}}{Nested list: \code{by_product_code}, \code{by_mapping},
#'     \code{connectors}, \code{deployment}, \code{top_customers}.}
#'   \item{\code{renewals}}{One row per contract with SFDC fields and, when
#'     \code{raw} is supplied, live Looker metrics.}
#'   \item{\code{low_consumption}}{Subset of \code{renewals} below
#'     \code{low_consumption_threshold}, sorted by consumption ascending.}
#' }
#' @export
op_list_consumption <- function(op_list,
                                raw = NULL) {
  if (is.null(op_list$orders)) {
    stop("`op_list$orders` is NULL. Run enrich_opportunities() first.")
  }

  has_looker <- !is.null(raw)
  message(".. op_list_consumption: building data frames",
          if (!has_looker) " (SFDC-only; pass raw = load_op_list_raw() for Looker metrics)")


  # ── Raw Looker data frames — stored as named child objects ───────────────────
  users           <- if (has_looker) raw$users      else NULL  # look 1746
  policy_metrics  <- if (has_looker) raw$policies   else NULL  # look 1747
  sdd_consumption <- if (has_looker) raw$sdd_active else NULL  # look 1748

  # ── low_consumption ───────────────────────────────────────────────────────────
  # One row per customer. Base from op_list$accounts; Looker metrics joined when
  # available. Users and policy_metrics are filtered to Production instances;
  # where a customer has multiple Production instances the row with the highest
  # seat consumption is used for users, and the highest total policies for metrics.
  account_base <- op_list$accounts |>
    dplyr::select(
      `Account ID`,
      `Account Name`,
      `Contract ARR Rollup`,
      `Is SaaS Customer`,
      `Customer on Unsupported Version`
    )

  if (has_looker) {
    # Production users — one row per customer (highest seat consumption instance)
    users_prod <- users |>
      dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
      dplyr::mutate(.lc = .parse_pct(`Total Contracted Seat Consumption`)) |>
      dplyr::group_by(`Salesforce Accounts Account Name`) |>
      dplyr::slice_max(order_by = .lc, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`) |>
      dplyr::select(-`Immuta Instances Instance Type`, -.lc)

    # Production policy_metrics — one row per customer (highest total policies instance)
    policy_prod <- policy_metrics |>
      dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
      dplyr::mutate(.tot = suppressWarnings(as.numeric(`Active Global Policies: Total`))) |>
      dplyr::group_by(`Salesforce Accounts Account Name`) |>
      dplyr::slice_max(order_by = .tot, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`) |>
      dplyr::select(-`Immuta Instances Instance Type`, -.tot)

    low_consumption <- account_base |>
      dplyr::left_join(users_prod,  by = "Account Name") |>
      dplyr::left_join(policy_prod, by = "Account Name")

  } else {
    low_consumption <- account_base
  }

  message(".. op_list_consumption: complete")

  list(
    # ── Raw Looker data frames ──────────────────────────────────────────────────
    users           = users,           # look 1746 — all instances, all columns
    policy_metrics  = policy_metrics,  # look 1747 — all instances, all columns
    sdd_consumption = sdd_consumption, # look 1748 — all instances, all columns

    # ── Derived ─────────────────────────────────────────────────────────────────
    low_consumption = low_consumption  # one row per customer, accounts + Looker
  )
}
