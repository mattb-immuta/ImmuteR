#' Merge account data into the expansion table and compute derived fields.
#'
#' Joins enriched account attributes onto expansion opportunities, filters to
#' the specified fiscal periods, deduplicates on Opportunity ID, and derives
#' expected revenue, deal size classification, and consumption risk flag.
#'
#' @param expansion.table Data frame of expansion opportunities from Salesforce.
#' @param account.table Enriched account table from \code{enrich_account.table()}.
#' @param fiscal.period Character vector of fiscal periods to retain, e.g.
#'   \code{c("Q1-2027", "Q2-2027")}.
#'
#' @return The expansion table with account fields joined in and additional columns:
#'   \code{Amount} (numeric), \code{Probability (\%)} (numeric 0-1),
#'   \code{expected_revenue}, \code{expansion_type}, \code{days_until_close},
#'   and \code{consumption_risk} (logical).
#' @export
enrich_expansions <- function(expansion.table, account.table, fiscal.period) {

  expansion.table <- expansion.table %>%
    dplyr::select(-`Account Risk`, -`Adoption Phase`)

  expansion.table <- merge(
    expansion.table,
    account.table[, c("Account ID", names(account.table)[!names(account.table) %in% names(expansion.table)])],
    by = "Account ID",
    all.x = TRUE
  )

  expansion.table <- expansion.table[expansion.table$`Fiscal Period` %in% fiscal.period, ]
  expansion.table <- expansion.table[!duplicated(expansion.table$`Opportunity ID`), ]

  expansion.table$Amount             <- as.numeric(extract_usd_amount(expansion.table$Amount))
  expansion.table$`Probability (%)`  <- as.numeric(sub("%", "", expansion.table$`Probability (%)`))/100
  expansion.table$expected_revenue   <- expansion.table$Amount * expansion.table$`Probability (%)`
  expansion.table$expansion_type     <- "expansion"
  expansion.table$days_until_close   <- as.numeric(as.Date(expansion.table$`Close Date`) - Sys.Date())
  expansion.table$consumption_risk   <- expansion.table$days_until_close < 120 & expansion.table$consumption < 0.6

  message(".. expansion data enriched")
  return(expansion.table)
}
