#' Build a structured account summary from op_list.
#'
#' Returns a named list with account metadata, open renewal and expansion
#' opportunities, and architecture details including SaaS status and active
#' contracted products.
#'
#' @param account_name Character. The exact Salesforce account name to summarize.
#' @param op_list Named list returned by \code{enrich_opportunities()}, expected
#'   to contain \code{accounts}, \code{renewals}, \code{expansions}, and
#'   \code{active_orders} elements.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{account}{Named list of key account fields: \code{account_name},
#'       \code{contract_arr}, \code{csm}, \code{account_owner}, \code{field_cto},
#'       \code{region}.}
#'     \item{opportunities}{List with two data frames: \code{renewals} (Opportunity
#'       Name, Amount, Current Contract End Date, Churn Quarter) and
#'       \code{expansions} (Opportunity Name, Opportunity Owner, Amount,
#'       Fiscal Period).}
#'     \item{architecture}{List with \code{is_saas} (logical), \code{unsupported_version}
#'       (logical, only meaningful when \code{is_saas} is \code{FALSE}), and
#'       \code{products} (data frame of active product names and quantities).}
#'   }
#'
#' @examples
#' \dontrun{
#'   summary <- output_account_summary("Cigna", op_list)
#'   summary$account$contract_arr
#'   summary$opportunities$renewals
#'   summary$architecture$products
#' }
#' @export
output_account_summary <- function(account_name, op_list) {

  # --- Account ---------------------------------------------------------------
  acct <- op_list$accounts[op_list$accounts$`Account Name` == account_name, ]

  if (nrow(acct) == 0) stop("Account '", account_name, "' not found in op_list$accounts.")

  acct <- acct[1, ]  # guard against duplicates
  acct_id <- acct$`Account ID`

  account <- list(
    account_name  = acct$`Account Name`,
    contract_arr  = acct$`Contract ARR Rollup`,
    csm           = acct$`Customer Success Manager`,
    account_owner = acct$`Account Owner`,
    field_cto     = acct$`Field CTO`,
    region        = acct$Region
  )

  # --- Opportunities ---------------------------------------------------------
  # Filter by Account ID (primary key — always present) rather than Account Name
  acct_renewals <- op_list$renewals[op_list$renewals$`Account ID` == acct_id, ]
  renewals <- acct_renewals[order(as.Date(acct_renewals$`Current Contract End Date`)), ]
  renewals <- renewals[, c("Opportunity Name", "Amount",
                           "Current Contract End Date", "Churn Quarter")]

  acct_expansions <- op_list$expansions[op_list$expansions$`Account ID` == acct_id, ]
  expansions <- acct_expansions[order(as.Date(acct_expansions$`Close Date`)), ]
  expansions <- expansions[, c("Opportunity Name", "Opportunity Owner",
                               "Amount", "Fiscal Period")]

  # --- Architecture ----------------------------------------------------------
  is_saas <- isTRUE(acct$`Is SaaS Customer` %in% c(TRUE, "Yes", "TRUE", "true", "yes"))

  unsupported_version <- if (!is_saas) {
    isTRUE(acct$`Customer on Unsupported Version` %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
  } else {
    NA  # not applicable for SaaS customers
  }

  orders <- op_list$active_orders %>%
    dplyr::filter(`Account Name: Account Name` == account_name)

  if (nrow(orders) == 0) {
    products <- data.frame(`Product Name` = character(0),
                           Quantity       = numeric(0),
                           check.names    = FALSE)
  } else {
    products <- orders %>%
      dplyr::select(`Product: Product Name`, Quantity) %>%
      dplyr::rename(`Product Name` = `Product: Product Name`) %>%
      dplyr::arrange(`Product Name`)
  }

  # --- Assemble --------------------------------------------------------------
  list(
    account = account,
    opportunities = list(
      renewals   = renewals,
      expansions = expansions
    ),
    architecture = list(
      is_saas             = is_saas,
      unsupported_version = unsupported_version,
      products            = products
    )
  )
}
