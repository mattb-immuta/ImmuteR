#' Load forecast and prioritization constants.
#'
#' Returns a list of constants used across enrichment and reporting functions,
#' including ARR tier breakpoints and the company churn plan by fiscal period.
#'
#' @return A named list with:
#'   \describe{
#'     \item{arr_index}{Numeric vector of ARR tier breakpoints (0, 150k, 500k).}
#'     \item{churn_plan}{Data frame of planned churn by fiscal period.}
#'   }
#' @export
load_constants <- function() {
  c <- list()

  c$arr_index <- c(0, 150000, 500000)

  c$churn_plan <- data.frame(
    fiscal.period = c("Q1-2027", "Q2-2027","Q3-2027","Q4-2027","Q1-2028", "Q2-2028","Q3-2028","Q4-2028"),
    company_plan  = c(1919761,505930,813808,1413967,
                      650000,650000,800000, 1100000)
  )

  return(c)
}
