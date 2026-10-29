#' Next Step snitch report across renewals, expansions and new logos
#'
#' One list of every open deal whose \code{Next Step} is stale or missing,
#' grouped by quarter and then by region, naming the person responsible:
#' \itemize{
#'   \item \strong{Renewals} and \strong{expansions}: the CSM
#'     (\code{Customer Success Manager}).
#'   \item \strong{New logos}: the account executive (\code{Opportunity Owner}).
#' }
#'
#' Same rule as \code{\link{report_next_step_snitch}}: \code{Next Step} must
#' start with an \code{MM/DD} date no older than the deal allows.
#' \itemize{
#'   \item Renewals reuse \code{report_next_step_snitch()} directly, so the two
#'     reports always agree: At Risk / Expected to Churn within
#'     \code{at_risk_max_days}, Expecting to Renew within \code{renew_max_days},
#'     other risk levels not checked. Quarter = \code{Churn Quarter}.
#'   \item Expansions and new logos: open (stage not "Closed"), within
#'     \code{pipeline_max_days}. Quarter = \code{Fiscal Period}, or the fiscal
#'     quarter of \code{Close Date} when that is blank. These are already
#'     limited to the \code{fiscal.period} passed to
#'     \code{enrich_opportunities()}.
#' }
#'
#' @param op_list Named list from \code{enrich_opportunities()}; uses
#'   \code{renewals}, \code{expansions} and \code{new_logos}.
#' @param additional_quarters,quarters,as_of,at_risk_max_days,renew_max_days,future_tolerance_days
#'   As in \code{\link{report_next_step_snitch}}. Quarters in scope default to
#'   the current and next fiscal quarter, for all three deal types.
#' @param pipeline_max_days Maximum Next Step age in days for expansions and
#'   new logos. Default \code{14}.
#' @param regions Character vector of regions to include, for all three deal
#'   types. Default all five; \code{NULL} turns the filter off.
#' @param output_path Character. Markdown report written here (Quarter →
#'   Region → table). \code{NULL} skips the file.
#'
#' @return Invisibly, a data frame with one row per flagged deal, ordered by
#'   quarter, region, deal type, then most stale: \code{Quarter},
#'   \code{Region}, \code{Deal Type} (Renewal / Expansion / New Logo),
#'   \code{Owner Role} (CSM / AE), \code{Owner}, \code{Account Name},
#'   \code{Opportunity Name}, \code{Amount}, \code{Status} (risk for renewals,
#'   stage otherwise), \code{Next Step Date}, \code{Days Since Update},
#'   \code{Allowed Days}, \code{Issue}, \code{Next Step}, \code{opportunity_url}.
#' @examples
#' \dontrun{
#'   all <- report_next_step_snitch_all(op_list)
#'   report_next_step_snitch_all(op_list, additional_quarters = "Q1-2028")
#' }
#' @export
report_next_step_snitch_all <- function(op_list,
                                        additional_quarters   = character(0),
                                        quarters              = NULL,
                                        as_of                 = Sys.Date(),
                                        at_risk_max_days      = 7,
                                        renew_max_days        = 14,
                                        pipeline_max_days     = 14,
                                        future_tolerance_days = 7,
                                        regions               = c("APJ", "EMEA", "US - E", "US - W", "Public Sector"),
                                        output_path           = "output/next_step_snitch_all.md") {

  as_of <- as.Date(as_of)
  if (is.null(quarters)) {
    current  <- date_to_fiscal_quarter(as_of)
    quarters <- c(current, .snitch_next_quarter(current))
  }
  quarters <- unique(c(quarters, additional_quarters))

  # Renewals: the existing check, verbatim (its console summary is suppressed
  # because this report prints its own).
  sn  <- suppressMessages(report_next_step_snitch(
    op_list, quarters = quarters, as_of = as_of,
    at_risk_max_days = at_risk_max_days, renew_max_days = renew_max_days,
    future_tolerance_days = future_tolerance_days, include_qbr = FALSE, regions = regions))
  ren <- sn$next_steps
  ren_out <- data.frame(
    Quarter            = ren$`Churn Quarter`,
    Region             = ren$Region,
    `Deal Type`        = rep("Renewal", nrow(ren)),
    `Owner Role`       = rep("CSM", nrow(ren)),
    Owner              = ren$`Customer Success Manager`,
    `Account Name`     = ren$`Account Name`,
    `Opportunity Name` = ren$`Opportunity Name`,
    Amount             = ren$Amount,
    Status             = ren$`Account Risk`,
    `Next Step Date`   = ren$`Next Step Date`,
    `Days Since Update`= ren$`Days Since Update`,
    `Allowed Days`     = ren$`Allowed Days`,
    Issue              = ren$Issue,
    `Next Step`        = ren$`Next Step`,
    opportunity_url    = ren$opportunity_url,
    check.names = FALSE, stringsAsFactors = FALSE)

  in_regions <- function(df) {
    if (is.null(regions) || !is.data.frame(df) || !"Region" %in% names(df)) return(df)
    df[trimws(as.character(df$Region)) %in% regions, , drop = FALSE]
  }
  exp_out <- .snitch_pipeline(in_regions(op_list$expansions), "Expansion", "CSM", "Customer Success Manager",
                              quarters, as_of, pipeline_max_days, future_tolerance_days)
  nl_out  <- .snitch_pipeline(in_regions(op_list$new_logos),  "New Logo",  "AE",  "Opportunity Owner",
                              quarters, as_of, pipeline_max_days, future_tolerance_days)
  checked <- c(Renewal = attr(sn, "checked") %||% NA,Expansion = attr(exp_out, "checked"),
               `New Logo` = attr(nl_out, "checked"))

  out <- rbind(ren_out, exp_out, nl_out)
  out$Region[is.na(out$Region) | trimws(out$Region) %in% c("", "-")] <- "(no region)"
  out$Owner[is.na(out$Owner) | trimws(out$Owner) %in% c("", "-")]    <- NA_character_

  # Quarter (chronological), Region, Renewal > Expansion > New Logo, most stale first.
  qkey  <- .snitch_quarter_key(out$Quarter)
  stale <- ifelse(is.na(out$`Days Since Update`), Inf, out$`Days Since Update`)
  out   <- out[order(qkey, out$Region, match(out$`Deal Type`, c("Renewal", "Expansion", "New Logo")),
                     -stale), , drop = FALSE]
  rownames(out) <- NULL

  .snitch_all_message(out, quarters, as_of, checked)
  if (!is.null(output_path)) .snitch_all_markdown(out, quarters, as_of, checked, output_path)
  invisible(out)
}


# ---- Internal helpers ----------------------------------------------------------

#' Stale-Next-Step check for an expansion / new logo table.
#' @noRd
.snitch_pipeline <- function(df, deal_type, role, owner_col, quarters, as_of, max_days, tol) {
  cols <- c("Quarter", "Region", "Deal Type", "Owner Role", "Owner", "Account Name",
            "Opportunity Name", "Amount", "Status", "Next Step Date", "Days Since Update",
            "Allowed Days", "Issue", "Next Step", "opportunity_url")
  empty <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                         check.names = FALSE, stringsAsFactors = FALSE)
  empty$Amount <- numeric(0); empty$`Next Step Date` <- as.Date(character(0))
  empty$`Days Since Update` <- integer(0); empty$`Allowed Days` <- numeric(0)
  attr(empty, "checked") <- 0L
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty)

  col <- function(nm) if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))

  fp <- trimws(as.character(col("Fiscal Period")))
  cd <- suppressWarnings(as.Date(col("Close Date")))
  qtr <- ifelse(grepl("^Q[1-4]-\\d{4}$", fp), fp,
                ifelse(is.na(cd), NA_character_, date_to_fiscal_quarter(cd)))
  stage <- as.character(col("Stage"))
  keep  <- qtr %in% quarters & !grepl("closed", stage, ignore.case = TRUE)
  df <- df[keep, , drop = FALSE]; qtr <- qtr[keep]; stage <- stage[keep]
  if (nrow(df) == 0) return(empty)

  next_step <- as.character(col("Next Step"))
  lead  <- .snitch_lead_date(next_step, as_of, tol)
  age   <- as.integer(as_of - lead)
  blank <- is.na(next_step) | trimws(next_step) %in% c("", "-")
  issue <- ifelse(blank, "No next step",
           ifelse(is.na(lead), "No MM/DD date at start",
           ifelse(age > max_days, "Out of date", NA_character_)))

  out <- data.frame(
    Quarter            = qtr,
    Region             = as.character(col("Region")),
    `Deal Type`        = deal_type,
    `Owner Role`       = role,
    Owner              = trimws(as.character(col(owner_col))),
    `Account Name`     = as.character(col("Account Name")),
    `Opportunity Name` = as.character(col("Opportunity Name")),
    Amount             = suppressWarnings(as.numeric(col("Amount"))),
    Status             = stage,
    `Next Step Date`   = lead,
    `Days Since Update`= age,
    `Allowed Days`     = rep(max_days, nrow(df)),
    Issue              = issue,
    `Next Step`        = ifelse(blank, NA_character_, next_step),
    opportunity_url    = as.character(col("opportunity_url")),
    check.names = FALSE, stringsAsFactors = FALSE)
  checked <- nrow(out)
  out <- out[!is.na(out$Issue), , drop = FALSE]
  attr(out, "checked") <- checked
  out
}

#' Sortable number for "Q3-2027" (year * 10 + quarter); unknown sorts last.
#' @noRd
.snitch_quarter_key <- function(q) {
  ok <- !is.na(q) & grepl("^Q[1-4]-\\d{4}$", q)
  key <- rep(Inf, length(q))
  key[ok] <- as.integer(sub("^Q[1-4]-", "", q[ok])) * 10 + as.integer(substr(q[ok], 2, 2))
  key
}

#' Console summary: Quarter -> Region -> one line per owner.
#' @noRd
.snitch_all_message <- function(out, quarters, as_of, checked) {
  hdr <- sprintf("Next Step snitch, all deals (%s; as of %s): %d deal(s) need an update%s.",
                 paste(quarters, collapse = ", "), format(as_of, "%Y-%m-%d"), nrow(out),
                 paste0(" [checked: ", paste(names(checked), checked, sep = " ", collapse = ", "), "]"))
  if (nrow(out) == 0) { message(hdr); return(invisible(NULL)) }
  lines <- hdr
  for (q in unique(out$Quarter)) {
    qd <- out[out$Quarter %in% q, , drop = FALSE]
    lines <- c(lines, sprintf("%s (%d)", ifelse(is.na(q), "(no quarter)", q), nrow(qd)))
    for (r in unique(qd$Region)) {
      rd <- qd[qd$Region == r, , drop = FALSE]
      lines <- c(lines, sprintf("  %s (%d)", r, nrow(rd)))
      who <- paste0(ifelse(is.na(rd$Owner), paste0("(no ", rd$`Owner Role`, ")"), rd$Owner),
                    " [", rd$`Owner Role`, "]")
      for (w in unique(who)) {
        wd <- rd[who == w, , drop = FALSE]
        lines <- c(lines, sprintf("    %s: %s", w,
                   paste0(wd$`Account Name`, " (", wd$`Deal Type`, ")", collapse = ", ")))
      }
    }
  }
  message(paste(lines, collapse = "\n"))
  invisible(NULL)
}

#' Markdown file: ## Quarter -> ### Region -> table.
#' @noRd
.snitch_all_markdown <- function(out, quarters, as_of, checked, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cell <- function(x) gsub("\\|", "\\\\|", gsub("[\r\n]+", " ", safe_val(x)))
  why <- ifelse(out$Issue == "Out of date",
                sprintf("%s (%d days; due every %d)", format(out$`Next Step Date`, "%m/%d"),
                        as.integer(out$`Days Since Update`), as.integer(out$`Allowed Days`)),
                out$Issue)
  opp <- ifelse(!is.na(out$opportunity_url) & nzchar(out$opportunity_url),
                sprintf("[%s](%s)", cell(out$`Opportunity Name`), out$opportunity_url),
                cell(out$`Opportunity Name`))

  md <- c(sprintf("# Next Step snitch — all deals (as of %s)", format(as_of, "%Y-%m-%d")), "",
          sprintf("Quarters: %s. Owner is the CSM for renewals and expansions, the AE for new logos.",
                  paste(quarters, collapse = ", ")),
          sprintf("Checked: %s. **%d need an update.**",
                  paste(names(checked), checked, sep = " ", collapse = " · "), nrow(out)), "")
  if (nrow(out) == 0) md <- c(md, "Nothing stale.")

  for (q in unique(out$Quarter)) {
    qi <- which(out$Quarter %in% q)
    md <- c(md, sprintf("## %s (%d)", ifelse(is.na(q), "No quarter", q), length(qi)), "")
    for (r in unique(out$Region[qi])) {
      ri <- qi[out$Region[qi] == r]
      md <- c(md, sprintf("### %s (%d)", r, length(ri)), "",
              "| Type | Owner | Account | Opportunity | Amount | Status | Issue |",
              "|---|---|---|---|---|---|---|",
              vapply(ri, function(i) sprintf("| %s | %s (%s) | %s | %s | %s | %s | %s |",
                out$`Deal Type`[i], cell(out$Owner[i]), out$`Owner Role`[i],
                cell(out$`Account Name`[i]), opp[i], fmt_usd(out$Amount[i]),
                cell(out$Status[i]), why[i]), character(1)), "")
    }
  }
  writeLines(md, path, useBytes = FALSE)
  message(".. wrote ", path)
}
