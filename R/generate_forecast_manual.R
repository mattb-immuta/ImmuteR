#' Monte Carlo churn forecast from hand-entered amounts.
#'
#' The same simulation as \code{\link{generate_forecast}}, but driven by
#' vectors you supply instead of \code{op_list$renewals} — for what-if
#' scenarios, or for a quarter where the Salesforce fields aren't filled in yet.
#'
#' Each value in \code{expected_churn} churns independently with probability
#' \code{p[1]}, each value in \code{at_risk} with probability \code{p[2]}, and
#' \code{static} is added to every simulation as certain churn (the
#' Closed Lost / Verbally Confirmed component in \code{generate_forecast()}).
#'
#' @param expected_churn Numeric vector of ARR amounts for "Expected to Churn"
#'   accounts. Names, if given, are kept in the returned \code{inputs}.
#' @param at_risk Numeric vector of ARR amounts for "At Risk Renewal"
#'   accounts. Defaults to none.
#' @param static Numeric. Certain churn added to every simulation. Defaults to
#'   \code{0}.
#' @param p Numeric length 2: churn probabilities for \code{expected_churn} and
#'   \code{at_risk}. Defaults to \code{c(0.6, 0.1)}, matching
#'   \code{generate_forecast()}.
#' @param n Integer number of simulations. Defaults to \code{10000}.
#' @param seed Optional integer passed to \code{set.seed()} for a reproducible
#'   run.
#' @param label Character. Scenario name used in the plot legend and title.
#' @param plot Logical. Build the interactive ECDF plot (needs \pkg{plotly}
#'   and \pkg{htmlwidgets}). Defaults to \code{TRUE}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{simulations}}{Named list (one element, \code{label}) holding
#'     the \code{n} simulated churn totals — the same shape
#'     \code{generate_forecast()} returns, so the two can be combined.}
#'   \item{\code{summary}}{One-row data frame: expected value, mean, sd, min,
#'     P10, P25, P50, P75, P90, max, and the probability of churning
#'     anything beyond \code{static}.}
#'   \item{\code{inputs}}{The values and probabilities used.}
#'   \item{\code{plot}}{A \code{plotly} empirical CDF with dashed P10 / P50 /
#'     P90 markers (click a point to pin its value and percentile), or
#'     \code{NULL} when \code{plot = FALSE}.}
#' }
#'
#' @examples
#' \dontrun{
#'   res <- generate_forecast_manual(
#'     expected_churn = c(Acme = 250000, Globex = 120000),
#'     at_risk        = c(400000, 180000, 95000),
#'     static         = 26501,
#'     label          = "Q4-2027 what-if",
#'     seed           = 1
#'   )
#'   res$summary
#'   res$plot
#'
#'   # Overlay on a Salesforce-driven quarter
#'   sf <- generate_forecast(op_list, churn.quarters = "Q4-2027")
#'   ImmuteR:::.forecast_ecdf_plot(c(sf, res$simulations))
#' }
#' @export
generate_forecast_manual <- function(expected_churn,
                                     at_risk = numeric(0),
                                     static  = 0,
                                     p       = c(0.6, 0.1),
                                     n       = 10000,
                                     seed    = NULL,
                                     label   = "Manual scenario",
                                     plot    = TRUE) {

  # ── Validate ────────────────────────────────────────────────────────────────
  clean <- function(x, arg) {
    if (is.null(x)) return(numeric(0))
    if (!is.numeric(x)) stop("`", arg, "` must be numeric.", call. = FALSE)
    if (anyNA(x)) {
      warning("Dropping ", sum(is.na(x)), " NA value(s) from `", arg, "`.",
              call. = FALSE)
      x <- x[!is.na(x)]
    }
    if (any(x < 0)) stop("`", arg, "` can't contain negative amounts.",
                         call. = FALSE)
    x
  }
  expected_churn <- clean(expected_churn, "expected_churn")
  at_risk        <- clean(at_risk, "at_risk")

  if (!is.numeric(static) || length(static) != 1 || is.na(static)) {
    stop("`static` must be a single number.", call. = FALSE)
  }
  if (!is.numeric(p) || length(p) != 2 || anyNA(p) || any(p < 0 | p > 1)) {
    stop("`p` must be two probabilities between 0 and 1.", call. = FALSE)
  }
  n <- as.integer(n)
  if (is.na(n) || n < 1) stop("`n` must be a positive integer.", call. = FALSE)
  if (length(expected_churn) == 0 && length(at_risk) == 0) {
    warning("No expected_churn or at_risk values — every simulation equals ",
            "`static`.", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)

  # ── Simulate ────────────────────────────────────────────────────────────────
  sim <- static +
    .forecast_bernoulli_sum(expected_churn, p[1], n) +
    .forecast_bernoulli_sum(at_risk,        p[2], n)

  sims <- stats::setNames(list(sim), label)

  q <- stats::quantile(sim, c(0.10, 0.25, 0.50, 0.75, 0.90), names = FALSE)
  summary <- data.frame(
    scenario       = label,
    expected_value = static + p[1] * sum(expected_churn) + p[2] * sum(at_risk),
    mean           = mean(sim),
    sd             = stats::sd(sim),
    min            = min(sim),
    p10            = q[1],
    p25            = q[2],
    p50            = q[3],
    p75            = q[4],
    p90            = q[5],
    max            = max(sim),
    prob_above_static = mean(sim > static),
    stringsAsFactors = FALSE
  )

  message(label, ": P10 ", fmt_usd(q[1]), " | P50 ", fmt_usd(q[3]),
          " | P90 ", fmt_usd(q[5]), " | mean ", fmt_usd(mean(sim)),
          "  (", format(n, big.mark = ","), " sims)")

  # ── Plot ────────────────────────────────────────────────────────────────────
  fig <- NULL
  if (plot) {
    fig <- .forecast_ecdf_plot(sims, title = paste0("Simulated Churn: ", label))

    # Dashed P10 / P50 / P90 markers, labelled along the top edge.
    marks <- c(P10 = q[1], P50 = q[3], P90 = q[5])
    fig <- plotly::layout(
      fig,
      shapes = lapply(marks, function(v) list(
        type = "line", x0 = v, x1 = v, y0 = 0, y1 = 1, yref = "paper",
        line = list(dash = "dash", width = 1, color = "#888888")
      )),
      annotations = unname(Map(function(nm, v) list(
        x = v, y = 1.02, yref = "paper", xanchor = "center", yanchor = "bottom",
        text = paste0(nm, " ", fmt_usd(v)), showarrow = FALSE,
        font = list(size = 11, color = "#555555")
      ), names(marks), marks))
    )
  }

  list(
    simulations = sims,
    summary     = summary,
    inputs      = list(expected_churn = expected_churn, at_risk = at_risk,
                       static = static, p = p, n = n),
    plot        = fig
  )
}


# Sum of independent Bernoulli draws: each amount in `values` churns with
# probability `prob`. Returns a length-n vector (zeros when `values` is empty).
.forecast_bernoulli_sum <- function(values, prob, n) {
  m <- length(values)
  if (m == 0) return(numeric(n))
  draws <- matrix(stats::rbinom(n * m, 1, prob), nrow = n, ncol = m)
  as.vector(draws %*% values)
}
