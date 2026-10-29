#' Shape the new logo (New Business) opportunities.
#'
#' New logo opportunities are on prospects, which usually aren't in the
#' customer account report, so unlike \code{enrich_expansions()} no account
#' fields are joined on. Everything shown (Region, Owner, Stage, Next Step)
#' already comes from the opportunity report. Filters to the given fiscal
#' periods, deduplicates on Opportunity ID, and derives the same numeric
#' fields expansions get.
#'
#' @param new_logo.table Data frame of \code{Opportunity Record Type ==
#'   "New Business"} rows from the Salesforce opportunity report.
#' @param fiscal.period Character vector of fiscal periods to retain, e.g.
#'   \code{c("Q3-2027", "Q4-2027")}. \code{NULL} keeps every period.
#'
#' @return The table with \code{Amount} (numeric), \code{Probability (\%)}
#'   (numeric 0-1), \code{expected_revenue}, \code{opportunity_type}
#'   (\code{"new_logo"}) and \code{days_until_close} added.
#' @export
enrich_new_logos <- function(new_logo.table, fiscal.period = NULL) {

  nl <- as.data.frame(new_logo.table, check.names = FALSE, stringsAsFactors = FALSE)

  if (!is.null(fiscal.period)) nl <- nl[as.character(nl$`Fiscal Period`) %in% fiscal.period, , drop = FALSE]
  if ("Opportunity ID" %in% names(nl)) nl <- nl[!duplicated(nl$`Opportunity ID`), , drop = FALSE]

  nl$Amount <- as.numeric(extract_usd_amount(nl$Amount))
  nl$`Probability (%)` <- if ("Probability (%)" %in% names(nl))
    suppressWarnings(as.numeric(sub("%", "", nl$`Probability (%)`)) / 100) else rep(NA_real_, nrow(nl))
  nl$expected_revenue <- nl$Amount * nl$`Probability (%)`
  nl$opportunity_type <- rep("new_logo", nrow(nl))
  nl$days_until_close <- if ("Close Date" %in% names(nl))
    as.numeric(as.Date(nl$`Close Date`) - Sys.Date()) else rep(NA_real_, nrow(nl))

  rownames(nl) <- NULL
  message(".. new logo data enriched (", nrow(nl), " opportunit", if (nrow(nl) == 1) "y" else "ies", ")")
  nl
}
