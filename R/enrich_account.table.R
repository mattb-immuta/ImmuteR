#' Join renewal and consumption data onto the account table and derive risk signals.
#'
#' Merges the highest-ARR renewal record per account and idex consumption metrics
#' onto the Salesforce account table. Derives \code{engagement_risk} (based on days
#' since last EBR) and \code{onboarding_risk} (based on adoption phase and tenure).
#'
#' @param account.table Data frame of Salesforce account records.
#' @param renewal.table Enriched renewal table from \code{enrich_renewals()}. The
#'   highest-ARR renewal per account is used for the join.
#' @param consumption.list Named list returned by \code{enrich_consumption()},
#'   expected to contain a \code{metrics_df} element keyed on \code{Account Name}.
#'
#' @return The account table with renewal fields, consumption metrics, and two
#'   new risk signal columns: \code{engagement_risk} and \code{onboarding_risk}
#'   (each \code{"low"}, \code{"moderate"}, or \code{"high"}).
#' @export
enrich_account.table <- function(account.table, renewal.table, consumption.list) {

  # Keep only the largest renewal per account for the join
  renewal.table <- renewal.table %>%
    dplyr::group_by(`Account ID`) %>%
    dplyr::slice_max(order_by = Amount, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(`Account ID`, Priority, `Account Risk`, `Adoption Phase`,
                  `Vitally Days as Customer`, opportunity_url, account_url, consumption_url)

  account.table <- merge(account.table, renewal.table, by = "Account ID", all.x = TRUE)

  # Join idex consumption metrics
  user_metrics <- as.data.frame(consumption.list$metrics_df)
  account.table <- merge(account.table, user_metrics, by = "Account Name", all.x = TRUE) %>%
    dplyr::select(-mean_queries.y, -median_queries.y) %>%
    dplyr::rename(mean_queries = mean_queries.x, median_queries = median_queries.x)

  # Engagement risk: high if no EBR in past 180 days (or never)
  account.table$engagement_risk <- "low"
  account.table$engagement_risk[
    (Sys.Date() - account.table$`Date of last EBR`) > 180 | is.na(account.table$`Date of last EBR`)
  ] <- "high"

  # Onboarding risk: low if in an active adoption phase; high if >2 years without reaching one
  green_logical <- account.table$`Adoption Phase` %in% c("Use Case Pipelining", "MVP Complete", "90% Adoption")
  red_logical   <- !green_logical & account.table$`Vitally Days as Customer` > 365 * 2
  account.table$onboarding_risk <- "moderate"
  account.table$onboarding_risk[green_logical] <- "low"
  account.table$onboarding_risk[red_logical]   <- "high"

  message(".. account data enriched")
  return(account.table)
}
