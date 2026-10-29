#' Snitch report: stale renewal Next Steps, SaaS migration notes, and overdue
#' QBR/EBRs.
#'
#' Three checks, returned together:
#' \enumerate{
#'   \item \strong{next_steps}: open renewals whose Next Step is out of date.
#'   \item \strong{saas_notes}: SaaS migrations due in the quarters in scope
#'     whose pipeline notes are older than \code{saas_max_days} (default 14 —
#'     bi-weekly).
#'   \item \strong{no_recent_qbr}: accounts with no QBR or EBR in the last
#'     \code{qbr_max_months} months (default 5).
#' }
#'
#' Looks at open renewals in the current churn quarter and the next one (plus
#' any extra quarters you name) and checks the date that leads the
#' \code{Next Step} field. CSMs are expected to start that field with the date
#' of the update in \code{MM/DD} form, e.g. \code{"9/29 - Save attempted..."}.
#'
#' A renewal is flagged when the lead date is older than its risk tier allows,
#' or when there is no lead date at all:
#' \itemize{
#'   \item \strong{At Risk Renewal} and \strong{Expected to Churn}: older than
#'     \code{at_risk_max_days} (default 7 days).
#'   \item \strong{Expecting to Renew}: older than \code{renew_max_days}
#'     (default 14 days).
#' }
#' Renewals with any other Account Risk (e.g. Verbally Confirmed, Closed Won)
#' are not checked.
#'
#' The \code{MM/DD} has no year, so it is read as the most recent such date on
#' or before \code{as_of} (a few days into the future are tolerated, for an
#' update written ahead of a meeting). "12/11" read on 6 October 2026 is
#' therefore 11 December 2025, about ten months stale.
#'
#' The SaaS migration check reads \code{op_list$saas_pipeline}: migrations
#' whose \code{Estimated SaaS Migration Period} is in the same quarters as the
#' renewal check (or \code{saas_quarters}), skipping ones the customer declined
#' (stage starting "0") and ones whose notes say "assumed churn" / "will churn"
#' (the SaaS Migration board's own churn rule). \code{SaaS Migration Pipeline
#' Notes} must start with an \code{MM/DD} date, read the same way as Next Step,
#' no more than \code{saas_max_days} old.
#'
#' The QBR/EBR check takes the more recent of \code{Date Last QBR} and
#' \code{Date of last EBR} on \code{op_list$accounts}. By default it covers the
#' accounts with a renewal checked above (open, in the quarters in scope, in a
#' checked risk tier); set \code{qbr_all_accounts = TRUE} to check every
#' account. An account with neither date is flagged too.
#'
#' @param op_list Named list from \code{enrich_opportunities()}; uses
#'   \code{op_list$renewals}.
#' @param additional_quarters Character vector of extra churn quarters to
#'   include, in \code{"Q#-YYYY"} form (e.g. \code{c("Q1-2028")}).
#' @param quarters Character vector that replaces the default current + next
#'   quarter entirely. Leave \code{NULL} for the default.
#' @param as_of Date the staleness is measured against. Defaults to today.
#' @param at_risk_max_days Maximum age in days for At Risk / Expected to Churn.
#' @param renew_max_days Maximum age in days for Expecting to Renew.
#' @param future_tolerance_days How far past \code{as_of} a lead date may be
#'   and still count as this year rather than last year.
#' @param saas_max_days Maximum age in days for SaaS migration notes. Default
#'   \code{14} (bi-weekly).
#' @param saas_quarters Character vector of estimated migration quarters to
#'   check. \code{NULL} (default) uses the same quarters as the renewal check.
#' @param qbr_max_months An account is flagged when its latest QBR/EBR is more
#'   than this many months before \code{as_of}.
#' @param qbr_all_accounts \code{TRUE} checks QBR/EBR recency for every account
#'   in \code{op_list$accounts} rather than only the in-scope renewals.
#' @param regions Character vector of regions to include, matched against
#'   \code{Region} on each check. Default all five:
#'   \code{c("APJ", "EMEA", "US - E", "US - W", "Public Sector")}. Rows with
#'   a blank region are left out; \code{NULL} turns the filter off.
#' @param include_qbr Logical. Run the QBR/EBR check. Default \code{FALSE}:
#'   \code{no_recent_qbr} comes back with its columns but no rows, and no QBR
#'   summary is printed. Set \code{TRUE} to include it (and pass
#'   \code{include_qbr = TRUE} to \code{push_slack_snitch()} /
#'   \code{output_dust_snitch()} to message it).
#'
#' @return A list with three data frames, each sorted by CSM:
#'
#'   \code{next_steps}: one row per renewal that needs its Next Step updated,
#'   at-risk first, then most stale. Columns:
#'   \code{Customer Success Manager}, \code{Account Name},
#'   \code{Opportunity Name}, \code{Churn Quarter}, \code{Account Risk},
#'   \code{Amount}, \code{Next Step Date}, \code{Days Since Update},
#'   \code{Allowed Days}, \code{Issue}, \code{Next Step}, \code{Region},
#'   \code{Opportunity Owner}, \code{opportunity_url}.
#'
#'   \code{saas_notes}: one row per in-scope SaaS migration whose notes need
#'   an update, most stale first. Columns: \code{Customer Success Manager},
#'   \code{Account Name}, \code{Estimated SaaS Migration Period},
#'   \code{SaaS Migration Pipeline Stage}, \code{Est. Date to Move Out of Stage},
#'   \code{Notes Date}, \code{Days Since Update}, \code{Allowed Days},
#'   \code{Issue}, \code{SaaS Migration Pipeline Notes}, \code{Region},
#'   \code{account_url}.
#'
#'   \code{no_recent_qbr}: one row per account with no QBR/EBR in the window,
#'   most overdue first. Columns: \code{Customer Success Manager},
#'   \code{Account Name}, \code{Region}, \code{Contract ARR},
#'   \code{Last QBR}, \code{Last EBR}, \code{Last Review},
#'   \code{Days Since Review}, \code{Next QBR}, \code{Next EBR},
#'   \code{Issue}, \code{account_url}.
#'
#'   Attributes \code{quarters}, \code{as_of}, \code{checked} (renewals
#'   examined), \code{saas_quarters}, \code{saas_checked} (migrations
#'   examined), \code{qbr_cutoff} and \code{qbr_checked} (accounts examined)
#'   are attached to the list, and a by-CSM summary of both is printed.
#'
#' @examples
#' \dontrun{
#'   snitch <- report_next_step_snitch(op_list)
#'   snitch <- report_next_step_snitch(op_list, additional_quarters = "Q1-2028")
#'   snitch$next_steps$`Account Name`
#'   snitch$saas_notes$`Account Name`
#'   snitch$no_recent_qbr$`Account Name`
#'   report_next_step_snitch(op_list, include_qbr = TRUE, qbr_all_accounts = TRUE)$no_recent_qbr
#' }
#' @export
report_next_step_snitch <- function(op_list,
                                    additional_quarters   = character(0),
                                    quarters              = NULL,
                                    as_of                 = Sys.Date(),
                                    at_risk_max_days      = 7,
                                    renew_max_days        = 14,
                                    future_tolerance_days = 7,
                                    saas_max_days         = 14,
                                    saas_quarters         = NULL,
                                    qbr_max_months        = 5,
                                    qbr_all_accounts      = FALSE,
                                    include_qbr           = FALSE,
                                    regions               = c("APJ", "EMEA", "US - E", "US - W", "Public Sector")) {

  ren <- op_list$renewals
  if (is.null(ren) || !is.data.frame(ren) || nrow(ren) == 0) {
    stop("op_list$renewals is missing or empty.")
  }
  need <- c("Account Name", "Opportunity Name", "Churn Quarter", "Account Risk", "Next Step")
  miss <- setdiff(need, names(ren))
  if (length(miss)) stop("op_list$renewals is missing column(s): ", paste(miss, collapse = ", "))

  as_of <- as.Date(as_of)

  # Region scope ----------------------------------------------------------------
  # Applied to all three checks. Rows with no region fall outside any list.
  in_regions <- function(df) {
    if (is.null(regions) || !is.data.frame(df) || !"Region" %in% names(df)) return(df)
    chk <- attr(df, "checked")
    df  <- df[trimws(as.character(df$Region)) %in% regions, , drop = FALSE]
    attr(df, "checked") <- chk
    df
  }
  ren <- in_regions(ren)

  # Quarters in scope ---------------------------------------------------------
  if (is.null(quarters)) {
    current  <- date_to_fiscal_quarter(as_of)
    quarters <- c(current, .snitch_next_quarter(current))
  }
  quarters <- unique(c(quarters, additional_quarters))
  bad_q <- quarters[!grepl("^Q[1-4]-\\d{4}$", quarters)]
  if (length(bad_q)) stop("Quarters must look like 'Q3-2027': ", paste(bad_q, collapse = ", "))

  # Open renewals in those quarters, in a risk tier we check ------------------
  risk  <- trimws(as.character(ren$`Account Risk`))
  tier  <- ifelse(grepl("at risk|expected to churn", risk, ignore.case = TRUE), "at_risk",
           ifelse(grepl("expecting to renew", risk, ignore.case = TRUE), "renew", NA_character_))
  stage <- if ("Stage" %in% names(ren)) as.character(ren$Stage) else rep("", nrow(ren))
  open  <- !grepl("closed", stage, ignore.case = TRUE) & !grepl("closed", risk, ignore.case = TRUE)

  keep <- trimws(as.character(ren$`Churn Quarter`)) %in% quarters & open & !is.na(tier)
  ren  <- ren[keep, , drop = FALSE]
  tier <- tier[keep]
  scope_accounts <- unique(trimws(as.character(ren$`Account Name`)))

  # Lead date and age ---------------------------------------------------------
  next_step <- as.character(ren$`Next Step`)
  lead      <- .snitch_lead_date(next_step, as_of, future_tolerance_days)
  age       <- as.integer(as_of - lead)
  allowed   <- ifelse(tier == "at_risk", at_risk_max_days, renew_max_days)

  blank <- is.na(next_step) | trimws(next_step) %in% c("", "-")
  issue <- ifelse(blank, "No next step",
           ifelse(is.na(lead), "No MM/DD date at start",
           ifelse(age > allowed, "Out of date", NA_character_)))

  col <- function(nm) if (nm %in% names(ren)) ren[[nm]] else rep(NA, nrow(ren))
  out <- data.frame(
    # Salesforce sends "-" for an unassigned CSM; make it NA so it groups as "(no CSM)".
    `Customer Success Manager` = { v <- trimws(as.character(col("Customer Success Manager")))
                                   ifelse(is.na(v) | v %in% c("", "-"), NA_character_, v) },
    `Account Name`             = as.character(ren$`Account Name`),
    `Opportunity Name`         = as.character(ren$`Opportunity Name`),
    `Churn Quarter`            = as.character(ren$`Churn Quarter`),
    `Account Risk`             = as.character(ren$`Account Risk`),
    Amount                     = suppressWarnings(as.numeric(col("Amount"))),
    `Next Step Date`           = lead,
    `Days Since Update`        = age,
    `Allowed Days`             = allowed,
    Issue                      = issue,
    `Next Step`                = ifelse(blank, NA_character_, next_step),
    Region                     = as.character(col("Region")),
    `Opportunity Owner`        = as.character(col("Opportunity Owner")),
    opportunity_url            = as.character(col("opportunity_url")),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  checked <- nrow(out)
  tier    <- tier[!is.na(out$Issue)]
  out     <- out[!is.na(out$Issue), , drop = FALSE]

  # CSM, then at-risk before expecting-to-renew, then most stale first (no
  # date at all counts as most stale).
  stale_key <- ifelse(is.na(out$`Days Since Update`), Inf, out$`Days Since Update`)
  out <- out[order(out$`Customer Success Manager`, tier != "at_risk", -stale_key), , drop = FALSE]
  rownames(out) <- NULL

  # SaaS migration notes --------------------------------------------------------
  saas_q <- unique(if (is.null(saas_quarters)) quarters else saas_quarters)
  saas <- .snitch_saas(op_list$saas_pipeline, op_list$accounts, saas_q, as_of,
                       saas_max_days, future_tolerance_days)
  saas <- in_regions(saas)

  # QBR / EBR recency ---------------------------------------------------------
  qbr_cutoff <- seq(as_of, length.out = 2, by = paste0("-", qbr_max_months, " months"))[2]
  qbr <- .snitch_qbr(op_list$accounts,
                     if (isTRUE(qbr_all_accounts)) NULL else scope_accounts,
                     as_of, qbr_cutoff)
  # Off by default: keep the columns (and the "checked" count) but drop the rows,
  # so downstream senders see "no QBR issues" rather than a missing element.
  qbr <- in_regions(qbr)
  if (!isTRUE(include_qbr)) {
    chk <- attr(qbr, "checked")
    qbr <- qbr[0, , drop = FALSE]
    attr(qbr, "checked") <- chk
  }

  res <- list(next_steps = out, saas_notes = saas, no_recent_qbr = qbr)
  attr(res, "quarters")    <- quarters
  attr(res, "as_of")       <- as_of
  attr(res, "checked")     <- checked
  attr(res, "saas_quarters") <- saas_q
  attr(res, "saas_checked")  <- attr(saas, "checked")
  attr(res, "qbr_cutoff")  <- qbr_cutoff
  attr(res, "qbr_checked") <- attr(qbr, "checked")

  .snitch_message(out, quarters, as_of, checked)
  .snitch_saas_message(saas, saas_q, saas_max_days, attr(saas, "checked"))
  if (isTRUE(include_qbr)) {
    .snitch_qbr_message(qbr, qbr_max_months, qbr_cutoff, attr(qbr, "checked"),
                        if (isTRUE(qbr_all_accounts)) "all accounts" else "accounts with an in-scope renewal")
  }
  res
}


# ---- Internal helpers ----------------------------------------------------------

#' "Q4-2027" -> "Q1-2028".
#' @noRd
.snitch_next_quarter <- function(q) {
  n <- as.integer(sub("^Q([1-4])-\\d{4}$", "\\1", q))
  y <- as.integer(sub("^Q[1-4]-(\\d{4})$", "\\1", q))
  ifelse(n == 4, paste0("Q1-", y + 1), paste0("Q", n + 1, "-", y))
}

#' Date from the leading MM/DD (optionally /YY or /YYYY) of each string.
#' Without a year, the most recent such date not more than `tol` days after
#' `as_of`. NA when the string does not start with a valid date.
#' @noRd
.snitch_lead_date <- function(x, as_of, tol = 7) {
  x  <- trimws(ifelse(is.na(x), "", as.character(x)))
  re <- "^(\\d{1,2})/(\\d{1,2})(?:/(\\d{2}|\\d{4}))?(?!\\d)"
  m  <- regmatches(x, regexec(re, x, perl = TRUE))
  days <- vapply(m, function(g) {
    if (length(g) == 0) return(NA_real_)
    mo <- as.integer(g[2]); dy <- as.integer(g[3]); yr <- g[4]
    mk <- function(y) suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mo, dy)))
    if (!is.na(yr) && nzchar(yr)) {
      y <- as.integer(yr); if (y < 100) y <- 2000L + y
      return(as.numeric(mk(y)))
    }
    y <- as.integer(format(as_of, "%Y"))
    d <- mk(y)
    if (!is.na(d) && d > as_of + tol) d <- mk(y - 1L)
    as.numeric(d)
  }, numeric(1))
  as.Date(days, origin = "1970-01-01")
}

#' Console summary: one line per CSM.
#' @noRd
.snitch_message <- function(out, quarters, as_of, checked) {
  hdr <- sprintf("Next Step snitch report (%s; as of %s): %d of %d renewals need an update.",
                 paste(quarters, collapse = ", "), format(as_of, "%Y-%m-%d"), nrow(out), checked)
  if (nrow(out) == 0) { message(hdr); return(invisible(NULL)) }
  csm <- ifelse(is.na(out$`Customer Success Manager`) | out$`Customer Success Manager` == "",
                "(no CSM)", out$`Customer Success Manager`)
  groups <- split(out$`Account Name`, csm)
  lines  <- vapply(names(groups), function(nm) {
    sprintf("  %s (%d): %s", nm, length(groups[[nm]]), paste(unique(groups[[nm]]), collapse = ", "))
  }, character(1))
  message(paste(c(hdr, unname(lines)), collapse = "\n"))
  invisible(NULL)
}

#' Accounts whose latest QBR/EBR is before `cutoff` (or missing).
#' `keep_names` limits the check to those account names; NULL checks every account.
#' @noRd
.snitch_qbr <- function(accounts, keep_names, as_of, cutoff) {
  cols <- c("Customer Success Manager", "Account Name", "Region", "Contract ARR",
            "Last QBR", "Last EBR", "Last Review", "Days Since Review",
            "Next QBR", "Next EBR", "Issue", "account_url")
  empty <- function(n_checked) {
    e <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                       check.names = FALSE, stringsAsFactors = FALSE)
    attr(e, "checked") <- n_checked
    e
  }
  if (is.null(accounts) || !is.data.frame(accounts) || nrow(accounts) == 0 ||
      !"Account Name" %in% names(accounts)) {
    warning("op_list$accounts is missing; QBR/EBR check skipped.")
    return(empty(0L))
  }
  a <- accounts
  if (!is.null(keep_names)) a <- a[trimws(as.character(a$`Account Name`)) %in% keep_names, , drop = FALSE]
  if (nrow(a) == 0) return(empty(0L))

  col  <- function(nm) if (nm %in% names(a)) a[[nm]] else rep(NA, nrow(a))
  dt   <- function(nm) suppressWarnings(as.Date(col(nm)))
  qbr  <- dt("Date Last QBR")
  ebr  <- dt("Date of last EBR")
  last <- pmax(qbr, ebr, na.rm = TRUE)                      # NA only when both are NA
  csm  <- trimws(as.character(col("Customer Success Manager")))
  csm  <- ifelse(is.na(csm) | csm %in% c("", "-"), NA_character_, csm)

  out <- data.frame(
    `Customer Success Manager` = csm,
    `Account Name`             = as.character(a$`Account Name`),
    Region                     = as.character(col("Region")),
    `Contract ARR`             = suppressWarnings(as.numeric(col("Contract ARR Rollup"))),
    `Last QBR`                 = qbr,
    `Last EBR`                 = ebr,
    `Last Review`              = last,
    `Days Since Review`        = as.integer(as_of - last),
    `Next QBR`                 = dt("Date Next QBR"),
    `Next EBR`                 = dt("Date of next EBR"),
    Issue                      = ifelse(is.na(last), "No QBR/EBR on record",
                                 ifelse(last < cutoff, "No QBR/EBR since cutoff", NA_character_)),
    account_url                = as.character(col("account_url")),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  checked <- nrow(out)
  out <- out[!is.na(out$Issue), , drop = FALSE]
  overdue <- ifelse(is.na(out$`Days Since Review`), Inf, out$`Days Since Review`)
  out <- out[order(out$`Customer Success Manager`, -overdue), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "checked") <- checked
  out
}

#' Console summary for the QBR/EBR check.
#' @noRd
.snitch_qbr_message <- function(qbr, months, cutoff, checked, scope) {
  hdr <- sprintf("QBR/EBR check (%s; none since %s, %d months): %d of %d accounts overdue.",
                 scope, format(cutoff, "%Y-%m-%d"), months, nrow(qbr), checked)
  if (nrow(qbr) == 0) { message(hdr); return(invisible(NULL)) }
  csm <- ifelse(is.na(qbr$`Customer Success Manager`), "(no CSM)", qbr$`Customer Success Manager`)
  groups <- split(qbr$`Account Name`, csm)
  lines  <- vapply(names(groups), function(nm) {
    sprintf("  %s (%d): %s", nm, length(groups[[nm]]), paste(unique(groups[[nm]]), collapse = ", "))
  }, character(1))
  message(paste(c(hdr, unname(lines)), collapse = "\n"))
  invisible(NULL)
}


#' In-scope SaaS migrations whose notes are stale or undated.
#' @noRd
.snitch_saas <- function(sp, accounts, quarters, as_of, max_days, tol) {
  cols <- c("Customer Success Manager", "Account Name", "Estimated SaaS Migration Period",
            "SaaS Migration Pipeline Stage", "Est. Date to Move Out of Stage",
            "Notes Date", "Days Since Update", "Allowed Days", "Issue",
            "SaaS Migration Pipeline Notes", "Region", "account_url")
  empty <- function(n_checked) {
    e <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                       check.names = FALSE, stringsAsFactors = FALSE)
    attr(e, "checked") <- n_checked
    e
  }
  need <- c("Account Name", "Estimated SaaS Migration Period", "SaaS Migration Pipeline Notes")
  if (is.null(sp) || !is.data.frame(sp) || nrow(sp) == 0 || !all(need %in% names(sp))) {
    warning("op_list$saas_pipeline is missing or lacks ", paste(need, collapse = " / "),
            "; SaaS notes check skipped.", call. = FALSE)
    return(empty(0L))
  }

  col   <- function(nm) if (nm %in% names(sp)) sp[[nm]] else rep(NA, nrow(sp))
  stage <- trimws(as.character(col("SaaS Migration Pipeline Stage")))
  notes <- as.character(sp$`SaaS Migration Pipeline Notes`)
  churn <- grepl("(assumed|will)\\s+churn", ifelse(is.na(notes), "", notes), ignore.case = TRUE, perl = TRUE)
  declined <- grepl("^0\\b|declined", ifelse(is.na(stage), "", stage), ignore.case = TRUE, perl = TRUE)

  keep <- trimws(as.character(sp$`Estimated SaaS Migration Period`)) %in% quarters &
          !churn & !declined
  sp <- sp[keep, , drop = FALSE]; notes <- notes[keep]; stage <- stage[keep]
  if (nrow(sp) == 0) return(empty(0L))

  lead  <- .snitch_lead_date(notes, as_of, tol)
  age   <- as.integer(as_of - lead)
  blank <- is.na(notes) | trimws(notes) %in% c("", "-")
  issue <- ifelse(blank, "No notes",
           ifelse(is.na(lead), "No MM/DD date at start",
           ifelse(age > max_days, "Out of date", NA_character_)))

  # The SaaS report has Region but no CSM; both CSM and link come from accounts.
  idx <- if (!is.null(accounts) && "Account Name" %in% names(accounts))
    match(trimws(as.character(sp$`Account Name`)), trimws(as.character(accounts$`Account Name`)))
  else rep(NA_integer_, nrow(sp))
  acol <- function(nm) if (!is.null(accounts) && nm %in% names(accounts))
    as.character(accounts[[nm]])[idx] else rep(NA_character_, nrow(sp))
  csm <- trimws(acol("Customer Success Manager"))
  csm <- ifelse(is.na(csm) | csm %in% c("", "-"), NA_character_, csm)

  out <- data.frame(
    `Customer Success Manager`        = csm,
    `Account Name`                    = as.character(sp$`Account Name`),
    `Estimated SaaS Migration Period` = as.character(sp$`Estimated SaaS Migration Period`),
    `SaaS Migration Pipeline Stage`   = stage,
    `Est. Date to Move Out of Stage`  = suppressWarnings(as.Date(col("Est. Date to Move Out of Stage (SM)"))),
    `Notes Date`                      = lead,
    `Days Since Update`               = age,
    `Allowed Days`                    = max_days,
    Issue                             = issue,
    `SaaS Migration Pipeline Notes`   = ifelse(blank, NA_character_, notes),
    Region                            = as.character(col("Region")),
    account_url                       = acol("account_url"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  checked <- nrow(out)
  out <- out[!is.na(out$Issue), , drop = FALSE]
  stale <- ifelse(is.na(out$`Days Since Update`), Inf, out$`Days Since Update`)
  out <- out[order(out$`Customer Success Manager`, -stale), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "checked") <- checked
  out
}

#' Console summary for the SaaS notes check.
#' @noRd
.snitch_saas_message <- function(saas, quarters, max_days, checked) {
  hdr <- sprintf("SaaS migration notes (%s; older than %d days): %d of %d migrations need an update.",
                 paste(quarters, collapse = ", "), max_days, nrow(saas), checked)
  if (nrow(saas) == 0) { message(hdr); return(invisible(NULL)) }
  csm <- ifelse(is.na(saas$`Customer Success Manager`), "(no CSM)", saas$`Customer Success Manager`)
  groups <- split(saas$`Account Name`, csm)
  lines  <- vapply(names(groups), function(nm) {
    sprintf("  %s (%d): %s", nm, length(groups[[nm]]), paste(unique(groups[[nm]]), collapse = ", "))
  }, character(1))
  message(paste(c(hdr, unname(lines)), collapse = "\n"))
  invisible(NULL)
}
