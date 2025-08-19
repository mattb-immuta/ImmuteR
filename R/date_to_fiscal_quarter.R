#' convert a close date or contract end date into a fiscal period.
#'
#' @param date_vector a date vector within a given sfdc report.
#' @return a vector of fiscal period assignments in Q#-YYYY format.
#' @examples
#' none
#' @export
#'
date_to_fiscal_quarter <- function(dates, fiscal_start_month = 10) {

  # Ensure the input is a Date object or vector of dates
  if (!inherits(dates, "Date")) {
    dates <- as.Date(dates)
  }

  # Extract the calendar years and months from the date vector
  calendar_year <- as.integer(format(dates, "%Y"))
  calendar_month <- as.integer(format(dates, "%m"))

  # Determine the fiscal year for each date
  fiscal_year <- ifelse(calendar_month >= fiscal_start_month, calendar_year + 1, calendar_year)

  # Calculate the fiscal month number (1-12) for each date
  # The modulo operator ensures the calculation works correctly across year boundaries
  fiscal_month <- (calendar_month - fiscal_start_month + 12) %% 12 + 1

  # Calculate the fiscal quarter number for each date
  fiscal_quarter <- ceiling(fiscal_month / 3)


  # Construct the output string
  paste0("Q", fiscal_quarter,"-", fiscal_year)
}

