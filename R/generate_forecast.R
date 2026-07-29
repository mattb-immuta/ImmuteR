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
#' @param interactive_plot Logical. If \code{TRUE}, also builds an interactive
#'   \pkg{plotly} plot of the empirical CDF of simulated churn for each quarter
#'   in \code{churn.quarters}. Clicking a point on the plot drops an annotation
#'   showing the exact churn value and percentile at that point. Requires the
#'   \pkg{plotly} and \pkg{htmlwidgets} packages. Defaults to \code{FALSE}.
#'
#' @return If \code{interactive_plot = FALSE} (the default), a named list with
#'   one element per quarter in \code{churn.quarters}, where each element is a
#'   numeric vector of length \code{n} containing simulated total churn for
#'   that quarter. If \code{interactive_plot = TRUE}, a named list with two
#'   elements: \code{simulations} (the list described above) and \code{plot}
#'   (a \code{plotly} htmlwidget showing the empirical CDF per quarter).
#'
#' @examples
#' \dontrun{
#'   sims <- generate_forecast(op_list, churn.quarters = c("Q4-2026", "Q1-2027"))
#'   hist(sims[["Q1-2027"]], main = "Q1-2027 Churn Forecast")
#'
#'   # Interactive empirical CDF, click a point to see its percentile
#'   result <- generate_forecast(
#'     op_list,
#'     churn.quarters = c("Q4-2026", "Q1-2027"),
#'     interactive_plot = TRUE
#'   )
#'   result$plot
#' }
#' @export
generate_forecast <- function(op.table,
                              p = c(0.6, 0.1),
                              churn.quarters = c("Q1-2028", "Q2-2028"),
                              n = 1000,
                              interactive_plot = FALSE) {

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
    at_risk_pos <- temp.renewals$`Amount`[
      temp.renewals$`Account Risk` == "At Risk Renewal" & temp.renewals$`Amount` > 0
    ]
    m <- length(at_risk_pos)
    at_risk_sim <- rowSums(
      matrix(at_risk_pos, nrow = n, ncol = m, byrow = TRUE) *
      matrix(stats::rbinom(n * m, 1, p[2]), nrow = n, ncol = m)
    )

    output.list[[i]] <- static_churn + expected_churn_sim + at_risk_sim
  }

  if (!interactive_plot) {
    return(output.list)
  }

  if (!requireNamespace("plotly", quietly = TRUE) ||
      !requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop(
      "Packages 'plotly' and 'htmlwidgets' are required for interactive_plot = TRUE. ",
      "Install them with install.packages(c('plotly', 'htmlwidgets')).",
      call. = FALSE
    )
  }

  ecdf.df <- do.call(rbind, lapply(names(output.list), function(q) {
    x <- sort(output.list[[q]])
    data.frame(
      Quarter = q,
      Value = x,
      Percentile = seq_along(x) / length(x)
    )
  }))

  fig <- plotly::plot_ly(
    data = ecdf.df,
    x = ~Value,
    y = ~Percentile,
    color = ~Quarter,
    type = "scatter",
    mode = "lines",
    line = list(shape = "hv"),
    hovertemplate = paste0(
      "Quarter: %{fullData.name}",
      "<br>Churn: $%{x:,.0f}",
      "<br>Percentile: %{y:.1%}",
      "<extra></extra>"
    )
  ) %>%
    plotly::layout(
      title = "Empirical CDF of Simulated Churn by Fiscal Quarter",
      xaxis = list(title = "Simulated Total Churn ($)", tickformat = "$,.0f"),
      yaxis = list(title = "Cumulative Probability (Percentile)", tickformat = ".0%"),
      hovermode = "closest"
    )

  # Click-to-inspect: dropping a point on the ECDF pins an annotation with
  # the exact churn value and percentile at that click location.
  fig <- htmlwidgets::onRender(fig, "
    function(el) {
      el.on('plotly_click', function(data) {
        var pt = data.points[0];
        var text = pt.data.name +
          '<br>Value: $' + pt.x.toLocaleString(undefined, {maximumFractionDigits: 0}) +
          '<br>Percentile: ' + (pt.y * 100).toFixed(1) + '%';
        var existing = (el.layout.annotations || []).filter(function(a) {
          return a.name !== 'ecdf-click';
        });
        Plotly.relayout(el, {
          annotations: existing.concat([{
            name: 'ecdf-click',
            x: pt.x,
            y: pt.y,
            xref: 'x',
            yref: 'y',
            text: text,
            showarrow: true,
            arrowhead: 4,
            ax: 40,
            ay: -40,
            bgcolor: 'white',
            bordercolor: pt.fullData.line.color
          }])
        });
      });
    }
  ")

  return(list(simulations = output.list, plot = fig))
}
