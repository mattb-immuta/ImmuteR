#' Diff two op_list snapshots and append change reports and tracker data.
#'
#' Compares renewal risk and churn forecast between a freshly-enriched list and
#' a saved prior-run list, appending a \code{renewal_changes} diff table and a
#' \code{customer_ops_tracker} summary to the new list.
#'
#' @param new_list Named list returned by the current run of
#'   \code{enrich_opportunities()}.
#' @param old_list Named list from a previous run, loaded from a saved
#'   \code{.RData} snapshot, used as the comparison baseline.
#'
#' @return \code{new_list} with two additional elements:
#'   \describe{
#'     \item{renewal_changes}{A \code{data.table} of renewals where Account Risk
#'       or Churn Forecast changed since the last run.}
#'     \item{customer_ops_tracker}{A data frame joining renewal and expansion
#'       data for use in \code{output_customer_ops_tracker()}.}
#'   }
#' @export
enrich_list_object <- function(new_list, old_list) {

  # --- Renewal change diff ---------------------------------------------------
  new_list$renewals$`Churn Forecast`[is.na(new_list$renewals$`Churn Forecast`)] <- 0
  old_list$renewals$`Churn Forecast`[is.na(old_list$renewals$`Churn Forecast`)] <- 0

  new_list_renewals <- new_list$renewals %>%
    dplyr::select(`Opportunity ID`, `Opportunity Name`, churn_forecast_period,
                  `Account Risk`, `Churn Forecast`)

  old_list_renewals <- old_list$renewals %>%
    dplyr::select(`Opportunity ID`, churn_forecast_period, `Account Risk`, `Churn Forecast`)

  renewal_change_df <- merge(new_list_renewals, old_list_renewals,
                             by = "Opportunity ID", all.x = TRUE)
  renewal_change_df[is.na(renewal_change_df)] <- FALSE

  renewal_changes_risk     <- renewal_change_df$`Account Risk.x`    != renewal_change_df$`Account Risk.y`
  renewal_changes_forecast <- renewal_change_df$`Churn Forecast.x`  != renewal_change_df$`Churn Forecast.y`
  renewal_changes          <- renewal_changes_risk | renewal_changes_forecast

  message(paste("Renewal changes:", sum(renewal_changes_risk), "to Account Risk,",
                sum(renewal_changes_forecast), "to Churn Forecast,",
                sum(renewal_changes), "total"))

  new_list[["renewal_changes"]] <- data.table::data.table(
    `Opportunity Name` = renewal_change_df$`Opportunity Name`[renewal_changes],
    `Fiscal Period`    = renewal_change_df$churn_forecast_period[renewal_changes],
    `New Account Risk` = renewal_change_df$`Account Risk.x`[renewal_changes],
    `Old Account Risk` = renewal_change_df$`Account Risk.y`[renewal_changes],
    `New Forecast`     = renewal_change_df$`Churn Forecast.x`[renewal_changes],
    `Old Forecast`     = renewal_change_df$`Churn Forecast.y`[renewal_changes]
  )
  new_list[["renewal_changes"]]$forecast_change <-
    new_list[["renewal_changes"]]$`New Forecast` - new_list[["renewal_changes"]]$`Old Forecast`

  message("renewal_changes appended to list")

  # --- Customer ops tracker build --------------------------------------------
  temp.expansions <- new_list$expansions[, c("Account ID", "Amount")] %>%
    dplyr::group_by(`Account ID`) %>%
    dplyr::summarise(total_amount = sum(Amount), active_expansions = dplyr::n(),
                     .groups = "drop")

  temp <- new_list$renewals %>%
    dplyr::select(`Account ID`, `Opportunity Name`, Region, `Opportunity Owner`,
                  `Customer Success Manager`, `Fiscal Period`, churn_forecast_period,
                  `Close Date`, Amount, `Churn Forecast`,
                  `Next Step`,
                  `Account Risk`, `Adoption Phase`)

  new_list[["customer_ops_tracker"]] <- merge(temp, temp.expansions,
                                              by = "Account ID", all.x = TRUE)
  message("customer_ops_tracker appended to list")

  return(new_list)
}
