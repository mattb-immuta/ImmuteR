#' Convert a date vector to Immuta fiscal quarter labels.
#'
#' Immuta's fiscal year starts in February, so Q1 runs Feb–Apr, Q2 May–Jul,
#' Q3 Aug–Oct, and Q4 Nov–Jan. The fiscal year label is the calendar year in
#' which the fiscal year \emph{ends} (i.e. a date in March 2026 is in Q1-2027).
#'
#' @param dates Date vector (or anything coercible to \code{Date}) to classify.
#' @param fiscal_start_month Integer month number (1–12) on which the fiscal year
#'   begins. Defaults to \code{2} (February).
#'
#' @return Character vector of fiscal quarter labels in \code{"Q#-YYYY"} format,
#'   the same length as \code{dates}.
#'
#' @examples
#' date_to_fiscal_quarter(as.Date(c("2026-03-15", "2026-09-01")))
#' # [1] "Q1-2027" "Q3-2027"
#' @export
date_to_fiscal_quarter <- function(dates, fiscal_start_month = 2) {

  if (!inherits(dates, "Date")) {
    dates <- as.Date(dates)
  }

  calendar_year  <- as.integer(format(dates, "%Y"))
  calendar_month <- as.integer(format(dates, "%m"))

  fiscal_year    <- ifelse(calendar_month >= fiscal_start_month, calendar_year + 1, calendar_year)
  fiscal_month   <- (calendar_month - fiscal_start_month + 12) %% 12 + 1
  fiscal_quarter <- ceiling(fiscal_month / 3)

  paste0("Q", fiscal_quarter, "-", fiscal_year)
}
