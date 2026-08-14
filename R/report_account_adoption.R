#' Build a summary table of every account's adoption profile.
#'
#' Returns one row per account with region, lifecycle stage, per-module
#' adoption flags (PBAC, Request, Comply, Agentic Governance), and whether
#' the account is SaaS-hosted. These all come straight off
#' \code{op_list$accounts} (the Salesforce account report pulled in
#' \code{enrich_opportunities()}) — this function just selects and sorts them.
#'
#' @param op_list Named list returned by \code{enrich_opportunities()} (or
#'   loaded from a saved snapshot in \code{data/}), expected to contain an
#'   \code{accounts} element.
#'
#' @return A data frame with columns \code{Account Name}, \code{Region},
#'   \code{Lifecycle Stage}, \code{PBAC Adoption}, \code{Request Adoption},
#'   \code{Comply Adoption}, \code{Agentic Governance Adoption}, and
#'   \code{Is SaaS Customer}, sorted alphabetically by account name. Any of
#'   these columns missing from \code{op_list$accounts} (e.g. an older
#'   snapshot predating a field) is filled with \code{NA} rather than erroring.
#'
#' @examples
#' \dontrun{
#'   report_account_adoption(op_list)
#' }
#' @export
report_account_adoption <- function(op_list) {

  cols <- c("Account Name", "Region", "Lifecycle Stage",
            "PBAC Adoption", "Request Adoption", "Comply Adoption",
            "Agentic Governance Adoption", "Is SaaS Customer")

  accounts <- op_list$accounts
  if (is.null(accounts)) {
    stop("`op_list$accounts` is NULL. Run enrich_opportunities() first.")
  }

  missing_cols <- setdiff(cols, names(accounts))
  for (col in missing_cols) accounts[[col]] <- NA

  output <- accounts[, cols]
  output <- output[order(output$`Account Name`), ]
  rownames(output) <- NULL

  output
}
