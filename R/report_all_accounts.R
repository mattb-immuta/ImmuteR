#' Build a summary table of all accounts with renewal churn quarters.
#'
#' Returns a data frame with one row per account, sorted alphabetically, showing
#' each account's name, contract ARR, and a concatenated string of churn quarters
#' from any active renewals ordered by contract end date.
#'
#' @param op_list Named list returned by \code{enrich_opportunities()}, expected
#'   to contain \code{accounts} and \code{renewals} elements.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{Account Name}{Salesforce account name.}
#'     \item{Contract ARR Rollup}{Numeric contract ARR.}
#'     \item{Customer Success Manager}{Assigned CSM.}
#'     \item{Account Owner}{Assigned account owner.}
#'     \item{Field CTO}{Assigned field CTO.}
#'     \item{Region}{Sales region.}
#'     \item{Months as Customer}{Whole months since the earliest contract
#'       start (\code{months_as_customer}); \code{NA} if unknown.}
#'     \item{Churn Quarters}{Comma-separated churn quarters for open renewals,
#'       ordered by contract end date. \code{NA} if no open renewals.}
#'   }
#'
#' @examples
#' \dontrun{
#'   report_all_accounts(op_list)
#' }
#' @export
report_all_accounts <- function(op_list) {

  accounts <- op_list$accounts[, c("Account ID", "Account Name", "Contract ARR Rollup",
                                    "Customer Success Manager", "Account Owner",
                                    "Field CTO", "Region")]
  accounts$`Months as Customer` <- if (is.null(op_list$accounts$months_as_customer)) NA_integer_ else
    op_list$accounts$months_as_customer
  renewals <- op_list$renewals

  # For each account, collect churn quarters ordered by contract end date
  churn_quarters <- tapply(
    seq_len(nrow(renewals)),
    renewals$`Account ID`,
    function(idx) {
      r <- renewals[idx, ]
      r <- r[order(as.Date(r$`Current Contract End Date`)), ]
      quarters <- r$`Churn Quarter`
      quarters <- quarters[!is.na(quarters) & quarters != ""]
      if (length(quarters) == 0) return(NA_character_)
      paste(quarters, collapse = ", ")
    }
  )

  churn_df <- data.frame(
    `Account ID`     = names(churn_quarters),
    `Churn Quarters` = as.character(churn_quarters),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )

  output <- merge(accounts, churn_df, by = "Account ID", all.x = TRUE)
  output <- output[order(output$`Account Name`), ]
  output <- output[, c("Account Name", "Contract ARR Rollup",
                       "Customer Success Manager", "Account Owner",
                       "Field CTO", "Region", "Months as Customer", "Churn Quarters")]
  rownames(output) <- NULL

  return(output)
}
