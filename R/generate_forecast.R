#' Monte Carlo churn forecast by fiscal quarter.
#'
#' For each quarter in \code{churn.quarters}, simulates \code{n} churn outcomes
#' by treating "Expected to Churn" and "At Risk Renewal" accounts as Bernoulli
#' random variables with churn probabilities \code{p[1]} and \code{p[2]}
#' respectively. "Closed Lost" and "Verbally Confirmed" accounts are treated as
#' certain churn (static component).
#'
#' @param op.table Named list containing a \code{renewals} data frame with at
#'   least columns \code{Churn Quarter}, \code{Churn Forecast}, \code{Amount},
#'   and \code{Account Risk}.
#' @param p Numeric vector of length 2 giving churn probabilities for
#'   \code{"Expected to Churn"} and \code{"At Risk Renewal"} accounts
#'   respectively. Defaults to \code{c(0.6, 0.1)}.
#' @param churn.quarters Character vector of fiscal quarter labels to forecast,
#'   e.g. \code{c("Q4-2026", "Q1-2027")}.
#' @param n Integer number of Monte Carlo simulations. Defaults to \code{1000}.
#'
#' @return Named list with one element per quarter in \code{churn.quarters}.
#'   Each element is a numeric vector of length \code{n} containing simulated
#'   total churn for that quarter.
#'
#' @examples
#' \dontrun{
#'   sims <- generate_forecast(op_list, churn.quarters = c("Q4-2026", "Q1-2027"))
#'   hist(sims[["Q1-2027"]], main = "Q1-2027 Churn Forecast")
#' }
#' @export
generate_forecast <- function(op.table,
                              p = c(0.6, 0.1),
                              churn.quarters = c("Q4-2026", "Q1-2027"),
                              n = 1000) {

  output.list <- list()

  for (i in churn.quarters) {
    temp.renewals <- op.table$renewals %>%
      dplyr::filter(`Churn Quarter` == i) %>%
      dplyr::select(`Account Name`, `Churn Forecast`, Amount, `Account Risk`, `Churn Quarter`)

    # Certain churn: Closed Lost or Verbally Confirmed
    static_churn <- sum(
      (as.numeric(temp.renewals$`Account Risk` == "Closed Lost") +
       as.numeric(temp.renewals$`Account Risk` == "Verbally Confirmed")) *
      temp.renewals$`Churn Forecast`
    )

    # Probabilistic churn: "Expected to Churn" accounts
    expected_churn_pos <- temp.renewals$`Churn Forecast`[
      temp.renewals$`Account Risk` == "Expected to Churn" & temp.renewals$`Churn Forecast` > 0
    ]
    m <- length(expected_churn_pos)
    expected_churn_sim <- rowSums(
      matrix(expected_churn_pos, nrow = n, ncol = m, byrow = TRUE) *
      matrix(stats::rbinom(n * m, 1, p[1]), nrow = n, ncol = m)
    )

    # Probabilistic churn: "At Risk Renewal" accounts
    at_risk_pos <- temp.renewals$`Churn Forecast`[
      temp.renewals$`Account Risk` == "At Risk Renewal" & temp.renewals$`Churn Forecast` > 0
    ]
    m <- length(at_risk_pos)
    at_risk_sim <- rowSums(
      matrix(at_risk_pos, nrow = n, ncol = m, byrow = TRUE) *
      matrix(stats::rbinom(n * m, 1, p[2]), nrow = n, ncol = m)
    )

    output.list[[i]] <- static_churn + expected_churn_sim + at_risk_sim
  }

  return(output.list)
}
