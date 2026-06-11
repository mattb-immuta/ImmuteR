#' Generate per-account markdown files for Asana task descriptions.
#'
#' For each account in \code{op_list}, writes a \code{.md} file containing an
#' account overview and a summary of open renewal opportunities. Files are named
#' by account name (spaces replaced with underscores) and written to
#' \code{output_dir}.
#'
#' @param op_list Named list returned by \code{enrich_opportunities()}, expected
#'   to contain \code{accounts} and \code{renewals} elements.
#' @param output_dir Character. Path to the directory where \code{.md} files
#'   will be written. Created automatically if it does not exist. Defaults to
#'   \code{"output/account_briefs/"}.
#'
#' @return Invisibly returns a character vector of the file paths written.
#'
#' @examples
#' \dontrun{
#'   output_markdown_accounts(op_list)
#' }
#' @export
output_markdown_accounts <- function(op_list, output_dir = "output/account_briefs/") {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  accounts  <- op_list$accounts
  renewals  <- op_list$renewals

  paths <- character(nrow(accounts))

  for (i in seq_len(nrow(accounts))) {

    acct <- accounts[i, ]
    acct_name <- acct$`Account Name`

    # Renewals for this account, sorted soonest first
    acct_renewals <- renewals %>%
      dplyr::filter(`Account Name` == acct_name) %>%
      dplyr::arrange(`Close Date`)

    lines <- c(
      paste0("# ", acct_name),
      paste0("*Last updated: ", Sys.Date(), "*"),
      "",

      # --- Overview -----------------------------------------------------------
      "## Account Overview",
      "",
      build_field("Region",        acct$Region),
      build_field("ARR",           fmt_usd(acct$`Contract ARR Rollup`)),
      build_field("Account Owner", acct$`Account Owner`),
      build_field("Field CTO",     acct$`Field CTO`),
      build_field("CSM",           acct$`Customer Success Manager`),
      build_field("Adoption Phase", acct$`Adoption Phase`),
      build_field("Days as Customer", fmt_days(acct$`Vitally Days as Customer`)),
      build_field("Engagement Risk",  fmt_risk(acct$engagement_risk)),
      build_field("Onboarding Risk",  fmt_risk(acct$onboarding_risk)),
      ""
    )

    # --- Architecture ---------------------------------------------------------
    lines <- c(lines, "## Architecture", "")

    saas_val <- acct$`Is SaaS Customer`
    saas_label <- if (!is.na(saas_val) && saas_val %in% c(TRUE, "Yes", "TRUE", "true", "yes")) {
      "Yes"
    } else if (!is.na(saas_val) && saas_val %in% c(FALSE, "No", "FALSE", "false", "no")) {
      "No"
    } else {
      "*—*"
    }
    lines <- c(lines, build_field("SaaS Customer", saas_label))

    lines <- c(lines, build_field("Databricks Connector", fmt_bool(acct$Databricks)))
    lines <- c(lines, build_field("Snowflake Connector",  fmt_bool(acct$Snowflake)))

    other_integrations <- acct$`Tech Integrations`
    lines <- c(lines,
      build_field("Other Tech Integrations",
                  if (is.na(other_integrations) || other_integrations == "") "*None listed*"
                  else other_integrations),
      ""
    )

    # --- Renewals -------------------------------------------------------------
    lines <- c(lines, "## Open Renewals", "")

    if (nrow(acct_renewals) == 0) {
      lines <- c(lines, "*No open renewals.*", "")
    } else {
      for (j in seq_len(nrow(acct_renewals))) {
        r <- acct_renewals[j, ]
        lines <- c(
          lines,
          paste0("### ", r$`Opportunity Name`),
          "",
          build_field("Close Date",     format(as.Date(r$`Close Date`), "%B %d, %Y")),
          build_field("Amount",         fmt_usd(r$Amount)),
          build_field("Churn Forecast", fmt_usd(r$`Churn Forecast`)),
          build_field("Account Risk",   fmt_risk(r$`Account Risk`)),
          build_field("Fiscal Period",  r$`Fiscal Period`),
          build_field("Next Step",      r$`Next Step`),
          ""
        )
      }
    }

    # Write file
    safe_name <- gsub("[^A-Za-z0-9_\\-]", "_", acct_name)
    path <- file.path(output_dir, paste0(safe_name, ".md"))
    writeLines(lines, path)
    paths[i] <- path
  }

  message(paste(".. wrote", nrow(accounts), "account markdown files to", output_dir))
  invisible(paths)
}


# Internal helpers (build_field, fmt_usd, fmt_days, fmt_bool, fmt_risk)
# are defined in R/utils_internal.R
