#' Agentic proof-of-value (PoV) projects, matched to accounts
#'
#' Runs the Salesforce agentic PoV project report (default
#' \code{00OVV000007QLj72AG}) and returns one row per project with its status,
#' opportunity stage, expected end date and notes, matched to
#' \code{op_list$accounts}. Attached by \code{\link{enrich_opportunities}} as
#' \code{op_list$agentic_pov} and shown on the Agentic Engagement Overview
#' (dashboard tab and \code{agentic_owner_summary.html}).
#'
#' The report has no account column: projects are named
#' \code{"PoV - <Account Name>"}, so the account is read from the project name
#' (anything after a leading "PoV" / "POC" and a dash or colon) and matched to
#' \code{accounts$`Account Name`} exactly (case-insensitive), then with company
#' suffixes and punctuation ignored (\code{.pb_normalize_name()}), then on a
#' unique prefix either way (so "PoV - Bupa" finds "Bupa - Australia"). If the
#' report does carry an Account Name column, that wins. Projects that match no
#' account are kept with \code{matched = FALSE} rather than dropped.
#'
#' @param sf_agentic_pov_report Character. Salesforce report ID.
#' @param report Optional data frame to use instead of running the report.
#' @param accounts Optional \code{op_list$accounts}, for the account match.
#'
#' @return A data frame, one row per project: \code{project}, \code{pov_account}
#'   (the name read from the project), \code{account} and \code{account_id} (the
#'   matched \code{op_list$accounts} row, \code{NA} if none), \code{matched},
#'   \code{status}, \code{stage}, \code{expected_end} (ISO date string) and
#'   \code{notes}.
#'
#' @examples
#' \dontrun{
#'   op_list_agentic_pov(accounts = op_list$accounts)
#' }
#' @export
op_list_agentic_pov <- function(sf_agentic_pov_report = "00OVV000007QLj72AG",
                                report   = NULL,
                                accounts = NULL) {
  rpt <- if (is.null(report)) .sf_run_report(sf_agentic_pov_report) else report
  rpt <- as.data.frame(rpt, check.names = FALSE, stringsAsFactors = FALSE)
  names(rpt) <- trimws(gsub("↑|↓", "", names(rpt)))

  pick <- function(exact, pattern = NULL) {
    hit <- names(rpt)[tolower(names(rpt)) %in% tolower(exact)]
    if (!length(hit) && !is.null(pattern)) hit <- grep(pattern, names(rpt), ignore.case = TRUE, perl = TRUE, value = TRUE)
    if (length(hit)) hit[1] else NA_character_
  }
  col <- list(
    project = pick(c("Project: Project Name", "Project Name", "Project"), "project.*name"),
    account = pick(c("Account Name", "Account: Account Name", "Project: Account Name",
                     "Opportunity: Account Name")),
    status  = pick(c("Status", "Project: Status", "Project Status"), "(^|: )status$"),
    stage   = pick(c("Opportunity Stage", "Opportunity: Stage", "Stage"), "stage"),
    end     = pick(c("Expected End Date", "Project: Expected End Date"), "end date"),
    notes   = pick(c("Project Notes (n)", "Project Notes", "Project: Project Notes", "Notes"), "notes")
  )
  if (is.na(col$project)) {
    stop("Agentic PoV report ", sf_agentic_pov_report, " has no project name column. Columns: ",
         paste(names(rpt), collapse = ", "), call. = FALSE)
  }
  if (nrow(rpt) == 0) {
    return(data.frame(project = character(0), pov_account = character(0), account = character(0),
                      account_id = character(0), matched = logical(0), status = character(0),
                      stage = character(0), expected_end = character(0), notes = character(0),
                      stringsAsFactors = FALSE))
  }
  get <- function(k) {
    if (is.na(col[[k]])) return(rep(NA_character_, nrow(rpt)))
    v <- trimws(as.character(rpt[[col[[k]]]]))
    v[is.na(v) | v %in% c("", "-", "—")] <- NA_character_
    v
  }

  project <- get("project")
  from_name <- trimws(sub("^\\s*(pov|poc|proof of value)\\s*[-–—:]+\\s*", "", project, ignore.case = TRUE))
  acct_col  <- get("account")
  pov_account <- ifelse(is.na(acct_col), from_name, acct_col)

  # ── Match to op_list$accounts ─────────────────────────────────────────────
  account <- account_id <- rep(NA_character_, length(project))
  if (is.data.frame(accounts) && nrow(accounts) && "Account Name" %in% names(accounts)) {
    an  <- as.character(accounts$`Account Name`)
    ids <- if ("Account ID" %in% names(accounts)) as.character(accounts$`Account ID`) else rep(NA_character_, length(an))
    idx <- match(tolower(trimws(pov_account)), tolower(trimws(an)))
    nn  <- .pb_normalize_name(an)
    miss <- is.na(idx) & !is.na(pov_account)
    idx[miss] <- match(.pb_normalize_name(pov_account[miss]), nn)
    for (i in which(is.na(idx) & !is.na(pov_account))) {
      p <- .pb_normalize_name(pov_account[i])
      if (nchar(p) < 4) next   # too short to trust a prefix match
      hit <- which(startsWith(nn, p) | vapply(nn, function(a) nchar(a) >= 4 && startsWith(p, a), logical(1)))
      if (length(hit) == 1) idx[i] <- hit
    }
    account    <- an[idx]
    account_id <- ids[idx]
  }

  end <- .ao_date(get("end"))
  out <- data.frame(
    project      = project,
    pov_account  = pov_account,
    account      = account,
    account_id   = account_id,
    matched      = !is.na(account),
    status       = get("status"),
    stage        = get("stage"),
    expected_end = ifelse(is.na(end), NA_character_, format(end, "%Y-%m-%d")),
    notes        = get("notes"),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$project), , drop = FALSE]
  rownames(out) <- NULL
  if (any(!out$matched)) {
    warning("Agentic PoV: no account match for ", paste(unique(out$pov_account[!out$matched]), collapse = ", "),
            " (kept with matched = FALSE).", call. = FALSE)
  }
  message("  agentic PoV: ", nrow(out), " project(s), ", sum(out$matched), " matched to accounts")
  out
}


#' Flag accounts with an agentic PoV: accounts$OBO_revenue_TGT
#'
#' \code{TRUE} for every account with at least one project on the agentic PoV
#' report (\code{op_list$agentic_pov}, matched by \code{op_list_agentic_pov()}),
#' \code{FALSE} for every other account: these are the OBO session revenue
#' targets. Matched on Account ID (15 characters), then account name. When the
#' PoV pull failed (\code{pov} is \code{NULL}) the column is \code{NA}, unknown
#' rather than "no PoV".
#'
#' @param accounts \code{op_list$accounts}.
#' @param pov \code{op_list$agentic_pov}, or \code{NULL}.
#' @return \code{accounts} with a logical \code{OBO_revenue_TGT} column.
#' @noRd
.accounts_add_obo_target <- function(accounts, pov) {
  if (!is.data.frame(accounts) || !nrow(accounts)) return(accounts)
  if (!is.data.frame(pov)) {
    accounts$OBO_revenue_TGT <- NA
    return(accounts)
  }
  pov <- pov[!is.na(pov$matched) & pov$matched, , drop = FALSE]
  id15 <- function(x) substr(trimws(as.character(x)), 1, 15)
  by_id <- if ("Account ID" %in% names(accounts)) {
    id15(accounts$`Account ID`) %in% stats::na.omit(id15(pov$account_id))
  } else rep(FALSE, nrow(accounts))
  by_nm <- tolower(trimws(accounts$`Account Name`)) %in% stats::na.omit(tolower(trimws(pov$account)))
  accounts$OBO_revenue_TGT <- by_id | by_nm
  message("  OBO_revenue_TGT: ", sum(accounts$OBO_revenue_TGT), " of ", nrow(accounts),
          " accounts have an agentic PoV")
  accounts
}
