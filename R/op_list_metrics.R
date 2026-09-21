# Per-account feature metrics derived from the raw Looker looks — the numeric
# inputs behind complexity/consumption scoring (PBAC adoption first).
#
# One row per account, one column per metric, so new metrics are added as
# columns here rather than as new child objects on op_list$consumption. Stored
# as op_list$consumption$metrics by op_list_consumption().

#' Per-account feature metrics for adoption scoring
#'
#' Builds the metrics data frame stored as
#' \code{op_list$consumption$metrics}. Currently one metric:
#'
#' \describe{
#'   \item{\code{platform_count}}{How many distinct platforms the account has
#'     data sources registered on in \strong{Production} — an integer, counting
#'     a platform once when its data-source count is greater than zero.}
#' }
#'
#' Production instances only, and summed across them: several accounts run both
#' an Immuta Managed Cloud and an On Premises Production instance, so a per-row
#' count would report those accounts twice and undercount their platforms (the
#' same reasoning as the user and policy rollups in
#' \code{\link{op_list_consumption}}).
#'
#' The platform columns come back from Looker as character, so they are coerced
#' before summing — summing them raw silently yields nothing.
#'
#' @param data_sources Data frame. The raw data-sources look (1744), i.e.
#'   \code{op_list$consumption$data_sources}. \code{NULL} (a
#'   \code{looker = FALSE} run) returns a zero-row frame of the right shape.
#' @param platform_cols Named character vector mapping short platform name to
#'   its column in \code{data_sources}. Defaults to \code{.PLATFORM_COLS},
#'   which includes the \code{"Other"} catch-all; pass
#'   \code{.PLATFORM_COLS[names(.PLATFORM_COLS) != "Other"]} to count named
#'   platforms only.
#'
#' @return A data frame with \code{Account Name} and \code{platform_count},
#'   one row per account.
#' @examples
#' \dontrun{
#'   op_list_metrics(op_list$consumption$data_sources)
#' }
#' @export
op_list_metrics <- function(data_sources, platform_cols = .PLATFORM_COLS) {
  empty <- data.frame(`Account Name` = character(0),
                      platform_count = integer(0),
                      check.names    = FALSE)

  if (is.null(data_sources) || nrow(data_sources) == 0) return(empty)

  cols  <- platform_cols[platform_cols %in% names(data_sources)]
  short <- names(cols)
  if (length(cols) == 0) return(empty)

  data_sources |>
    dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
    dplyr::select(`Account Name` = `Salesforce Accounts Account Name`,
                  dplyr::all_of(setNames(unname(cols), short))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(short),
                                ~ suppressWarnings(as.numeric(.x)))) |>
    dplyr::group_by(`Account Name`) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(short),
                                   ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(platform_count = rowSums(dplyr::pick(dplyr::all_of(short)) > 0)) |>
    dplyr::select(`Account Name`, platform_count)
}
