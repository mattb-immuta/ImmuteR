#' Generate a structured account summary from an op_list.
#'
#' Extracts account metadata, open renewals, open expansions, and adoption/
#' consumption metrics for a single named account into a named list suitable
#' for display or reporting.
#'
#' @param account_name Character. The exact Salesforce account name to summarize.
#' @param op_list Named list returned by \code{enrich_opportunities()}, expected
#'   to contain \code{accounts}, \code{renewals}, and \code{expansions} elements.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{account}{Account name string.}
#'     \item{metadata}{Transposed data frame of key account fields (region, ARR,
#'       owner, CSM, field CTO, org chart and running estimate URLs).}
#'     \item{renewals}{Data frame of open renewal opportunities with Amount,
#'       Close Date, and Account Risk.}
#'     \item{expansions}{Data frame of open expansion opportunities with Amount,
#'       Close Date, and Stage.}
#'     \item{adoption}{Transposed data frame of adoption phase, tenure, user
#'       counts, consumption rates, and policy metrics.}
#'     \item{Performance-Stability}{Data frame of version, integration, SaaS,
#'       and Retool link fields.}
#'   }
#'
#' @examples
#' \dontrun{
#'   summarize_account("Cigna", op_list)
#' }
#' @export
summarize_account <- function(account_name, op_list) {

  output <- list()

  a_data <- op_list$accounts %>%
    dplyr::filter(`Account Name` == account_name)

  output[["account"]] <- a_data$`Account Name`

  output[["metadata"]] <- a_data %>%
    dplyr::select(Region, `Contract ARR Rollup`, `Account Owner`,
                  `Customer Success Manager`, `Field CTO`,
                  org_chart_url, running_estimate_url) %>%
    t()
  output$metadata[2, 1] <- scales::dollar(as.numeric(output$metadata[2, 1]))

  output[["renewals"]] <- op_list$renewals %>%
    dplyr::filter(`Account Name` == account_name) %>%
    dplyr::select(`Opportunity Name`, Amount, `Close Date`, `Account Risk`) %>%
    dplyr::mutate(Amount = scales::dollar(Amount))

  output[["expansions"]] <- op_list$expansions %>%
    dplyr::filter(`Account Name` == account_name) %>%
    dplyr::select(`Opportunity Name`, Amount, `Close Date`, Stage) %>%
    dplyr::mutate(Amount = scales::dollar(Amount))

  output[["adoption"]] <- a_data %>%
    dplyr::select(`Adoption Phase`, `Vitally Days as Customer`,
                  licensed_users, max_reg_users, mean_consumption, max_consumption,
                  mean_queries, global_policies_staged,
                  use_case_rollout_url, consumption_url) %>%
    t()

  output[["Performance-Stability"]] <- a_data %>%
    dplyr::select(`Customer on Unsupported Version`, `Tech Integrations`,
                  `Is SaaS Customer`, retool_url) %>%
    t() %>%
    as.data.frame()

  return(output)
}
