#' Merge account data into the renewal table and compute forecast fields.
#'
#' Joins enriched account attributes onto the renewal opportunity table, converts
#' currency columns to numeric, and derives \code{rev_lost_day} and
#' \code{churn_forecast_period} for downstream reporting.
#'
#' @param renewal.table Data frame of renewal opportunities from Salesforce.
#' @param account.table Enriched account table from \code{enrich_account.table()}.
#' @param fiscal.period Character vector of fiscal periods to retain, e.g.
#'   \code{c("Q1-2027", "Q2-2027")}. Pass \code{NULL} to keep all periods.
#'
#' @return The renewal table with account fields joined in and additional columns:
#'   \code{Amount} (numeric), \code{Churn Forecast} (numeric),
#'   \code{rev_lost_day} (Date), and \code{churn_forecast_period} (character).
#' @export
enrich_renewals <- function(renewal.table, account.table, fiscal.period) {

  # Join account attributes not already present in the renewal table
  renewal.table <- merge(
    renewal.table,
    account.table[, c("Account ID", names(account.table)[!names(account.table) %in% names(renewal.table)])],
    by = "Account ID",
    all.x = TRUE
  )

  # Optionally filter to specified fiscal periods
  if (!is.null(fiscal.period)) {
    renewal.table <- renewal.table[renewal.table$`Fiscal Period` %in% fiscal.period, ]
  }

  # Coerce currency strings to numeric
  renewal.table$Amount          <- as.numeric(extract_usd_amount(renewal.table$Amount))
  renewal.table$`Churn Forecast` <- as.numeric(extract_usd_amount(renewal.table$`Churn Forecast`))

  # Derive churn forecast timing fields
  renewal.table$rev_lost_day        <- as.Date(renewal.table$`Current Contract End Date`) + 1
  renewal.table$churn_forecast_period <- date_to_fiscal_quarter(renewal.table$rev_lost_day)

  message(".. renewal data enriched")
  return(renewal.table)
}
