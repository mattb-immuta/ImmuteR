#' Push the all-accounts adoption summary to a Google Sheet.
#'
#' Builds the table from \code{\link{report_account_adoption}} (account name,
#' region, lifecycle stage, PBAC/Request/Comply/Agentic Governance adoption,
#' and SaaS-customer flag) and writes it out with \code{googlesheets4}.
#'
#' @param op_list Named list returned by \code{enrich_opportunities()} (or
#'   loaded from a saved snapshot in \code{data/}).
#' @param ss Character or \code{NULL}. URL/ID of an existing Google Sheet to
#'   write into. If \code{NULL} (default), a brand-new spreadsheet named
#'   \code{"Account Adoption Summary - <today>"} is created and shared with
#'   \code{matt.benigni@immuta.com}.
#' @param sheet Character. Tab name to write. Defaults to \code{"Accounts"}.
#'
#' @return Invisibly, the spreadsheet ID/URL that was written to.
#' @examples
#' \dontrun{
#'   output_gsheet_account_adoption(op_list)
#' }
#' @export
output_gsheet_account_adoption <- function(op_list, ss = NULL, sheet = "Accounts") {

  df <- report_account_adoption(op_list)

  if (is.null(ss)) {
    ss_out <- googlesheets4::gs4_create(
      paste0("Account Adoption Summary - ", Sys.Date()),
      sheets = list(Accounts = df)
    )
    message("Created new Google Sheet: ", ss_out)
    return(invisible(ss_out))
  }

  googlesheets4::sheet_write(df, ss = ss, sheet = sheet)
  message("Wrote '", sheet, "' tab to: ", ss)
  invisible(ss)
}
