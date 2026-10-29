#' Interactive agentic owner summary (HTML)
#'
#' Runs the Salesforce agentic profile report (default
#' \code{00OVV000007LHjN2AW}, the same report \code{\link{agentic_snitch}}
#' reads) and writes a self-contained, filterable HTML page that answers three
#' questions:
#' \enumerate{
#'   \item For how many accounts do we know the agentic owner
#'     (\code{Agentic Owner - Customer})?
#'   \item For how many have we captured their vision
#'     (\code{Customer Agentic Vision})?
#'   \item Where do we stand on the conversation with them
#'     (\code{Conversation Status}, \code{Conversation Date})?
#' }
#'
#' The page is the dashboard's Adoption > Agentic Engagement Overview tab on
#' its own: the panel, counting rules and script are shared with
#' \code{\link{report_all_accounts_html}} (R/agentic_overview_assets.R), so the
#' two cannot drift. Two tabs: "Coverage" (headline numbers, the coverage
#' funnel, Conversation Status, Evaluation / Deployment Status, then one
#' expandable card per account with every field) and "Removed for churn" (the
#' accounts left off because of \code{exclude_risk}). Region / CSM pills, gap
#' chips, clickable bars, "Incomplete" facets and a search box filter it.
#'
#' Conversation Status values are shown as Salesforce has them. Each is placed
#' on a stage by keyword so the page can count "held" conversations and order
#' the chart: held (held, completed, done, met, had), scheduled (scheduled,
#' booked, planned), in progress (requested, outreach, contacted, pending,
#' progress, initiated), not started (not started, none, no), else other.
#'
#' The per-account rows are built by the internal \code{.ao_build()}, which
#' \code{\link{report_all_accounts_html}} also uses for its Adoption > Agentic
#' Profile tab, so the two pages always count the same accounts the same way.
#' Besides the fields above it carries \code{Agentic Architecture},
#' \code{Evaluation Status}, \code{Deployment Status} and
#' \code{Next Steps/Follow Up} (shown on the dashboard tab).
#'
#' @param sf_report Character. Salesforce report ID.
#' @param op_list Optional named list from \code{\link{enrich_opportunities}}.
#'   When given, Contract ARR, region, CSM (where the report lacks them) and
#'   the product adoption rating are joined on Account ID, then name. If it
#'   carries \code{op_list$agentic_profile} (the raw report, pulled by
#'   \code{enrich_opportunities()}), that is used instead of running the
#'   report again.
#' @param output_path Where to write the HTML page.
#' @param report Optional data frame to use instead of running the report.
#' @param exclude_risk Character vector of Account Risk values (exact,
#'   case-insensitive). Accounts with any renewal at one of these are left off
#'   the page. Default \code{c("Verbally Confirmed", "Will Churn")}: those
#'   customers are leaving, so their agentic profile isn't worth chasing.
#'   Read from the report's \code{Account Risk} column if it has one, and from
#'   \code{op_list$renewals} / \code{op_list$forecast}, so pass
#'   \code{op_list}. \code{character(0)} turns it off.
#' @param pov_report Character. Agentic PoV project report
#'   (\code{\link{op_list_agentic_pov}}), pulled only when \code{op_list}
#'   has no \code{agentic_pov}. \code{NULL} skips it.
#' @param open Logical. Open the page in the browser when done.
#'
#' @return Invisibly, a list: \code{path} (the HTML file), \code{data}
#'   (one row per account, as embedded in the page) and \code{removed} (the
#'   accounts dropped by \code{exclude_risk}, with their \code{risk} and
#'   earliest renewal \code{close} date; shown on the page's "Removed for
#'   churn" tab).
#'
#' @examples
#' \dontrun{
#'   salesforcer::sf_auth()
#'   report_agentic_owner_html(op_list = op_list)
#' }
#' @export
report_agentic_owner_html <- function(sf_report   = "00OVV000007LHjN2AW",
                                      op_list     = NULL,
                                      output_path = "output/agentic_owner_summary.html",
                                      report      = NULL,
                                      exclude_risk = c("Verbally Confirmed", "Will Churn"),
                                      pov_report  = "00OVV000007QLj72AG",
                                      open        = interactive()) {

  rpt <- if (!is.null(report)) {
    report
  } else if (is.list(op_list) && is.data.frame(op_list$agentic_profile) && nrow(op_list$agentic_profile)) {
    op_list$agentic_profile
  } else {
    .sf_run_report(sf_report)
  }
  built   <- .ao_build(rpt, op_list = op_list, exclude_risk = exclude_risk, sf_report = sf_report)
  acct    <- built$data
  removed <- built$removed

  # ── Page ──────────────────────────────────────────────────────────────────
  html <- .ao_fill(.AO_TEMPLATE, "{{AGX_CSS}}", .AGX_CSS)
  html <- .ao_fill(html, "{{AGX_PANEL}}", .AGX_PANEL)
  html <- .ao_fill(html, "{{AGX_JS}}", .AGX_JS)
  html <- .ao_fill(html, "{{UPDATED}}", html_escape(
    if (is.list(op_list)) .op_list_updated(op_list) else paste("Pulled", format(Sys.Date(), "%d %b %Y"))))
  html <- .ao_fill(html, "{{REPORT}}", html_escape(sf_report))
  # Agentic PoV projects: from op_list when the run pulled them, else pulled here.
  povs <- if (is.list(op_list) && is.data.frame(op_list$agentic_pov)) op_list$agentic_pov else
    if (is.null(pov_report) || !nzchar(pov_report)) NULL else tryCatch(op_list_agentic_pov(pov_report, accounts = if (is.list(op_list)) op_list$accounts else NULL),
             error = function(e) { warning("Agentic PoV report skipped: ", conditionMessage(e), call. = FALSE); NULL })
  html <- .ao_fill(html, "{{AGENTIC}}", .agx_json(built, exclude_risk, povs = povs,
                                                   delta = if (is.list(op_list)) op_list$changes_detail$agentic else NULL))

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, output_path, useBytes = TRUE)
  message("agentic owner summary: ", nrow(acct), " account(s) -> ", output_path)
  if (isTRUE(open)) utils::browseURL(normalizePath(output_path))
  invisible(list(path = output_path, data = acct, removed = removed))
}


# ── Internal ──────────────────────────────────────────────────────────────────

# The agentic profile report -> one row per account. Shared by
# report_agentic_owner_html() and the Adoption > Agentic Engagement Overview tab of
# report_all_accounts_html(). Returns list(data, removed): `removed` holds the
# accounts dropped by exclude_risk, with their risk value and renewal close.
.ao_build <- function(rpt, op_list = NULL,
                      exclude_risk = c("Verbally Confirmed", "Will Churn"),
                      sf_report = "00OVV000007LHjN2AW") {
  rpt <- as.data.frame(rpt, check.names = FALSE, stringsAsFactors = FALSE)
  names(rpt) <- trimws(gsub("\u2191|\u2193", "", names(rpt)))
  if (nrow(rpt) == 0) stop("Report ", sf_report, " returned no rows.", call. = FALSE)

  # ── Columns, by name or by keyword ─────────────────────────────────────────
  pick <- function(exact, pattern = NULL) {
    hit <- names(rpt)[tolower(names(rpt)) %in% tolower(exact)]
    if (!length(hit) && !is.null(pattern)) hit <- grep(pattern, names(rpt), ignore.case = TRUE, perl = TRUE, value = TRUE)
    if (length(hit)) hit[1] else NA_character_
  }
  col <- list(
    name   = pick(c("Account Name", "Account: Account Name", "Account")),
    id     = pick(c("Account ID", "Account ID 18", "Account: Account ID", "Account 18 Digit ID")),
    csm    = pick(c("Customer Success Manager", "CSM", "Account: Customer Success Manager",
                    "Customer Success Manager: Full Name")),
    region = pick(c("Region", "Account: Region")),
    owner  = pick(c("Agentic Owner - Customer", "Agentic Owner"), "agentic owner(?!.*title)"),
    title  = pick(c("Title", "Agentic Owner - Customer: Title"), "owner.*title"),
    vision = pick(c("Customer Agentic Vision"), "vision"),
    architecture = pick(c("Agentic Architecture"), "architecture"),
    status = pick(c("Conversation Status"), "conversation status"),
    date   = pick(c("Conversation Date"), "conversation date"),
    eval   = pick(c("Evaluation Status"), "evaluation"),
    deploy = pick(c("Deployment Status"), "deploy"),
    next_steps = pick(c("Next Steps/Follow Up", "Next Steps / Follow Up", "Next Steps/Follow-Up",
                        "Next Steps", "Follow Up"), "next step|follow.?up")
  )
  if (is.na(col$name)) {
    stop("Report ", sf_report, " has no Account Name column. Columns: ",
         paste(names(rpt), collapse = ", "), call. = FALSE)
  }
  need    <- c("owner", "vision", "status", "date")
  missing <- need[is.na(unlist(col[need]))]
  if (length(missing)) {
    warning("Report ", sf_report, " has no column for: ", paste(missing, collapse = ", "),
            ". Columns: ", paste(names(rpt), collapse = ", "), call. = FALSE)
  }

  # ── One row per account (first non-blank value per field) ─────────────────
  blank <- function(x) {
    v <- trimws(as.character(x))
    is.na(x) | v %in% c("", "-", "\u2014", "\u2013", "NA", "N/A")
  }
  get <- function(df, k) if (is.na(col[[k]])) rep(NA, nrow(df)) else df[[col[[k]]]]
  key <- if (!is.na(col$id)) {
    ifelse(blank(rpt[[col$id]]), tolower(trimws(rpt[[col$name]])), substr(trimws(rpt[[col$id]]), 1, 15))
  } else tolower(trimws(rpt[[col$name]]))
  first_filled <- function(x) {
    if (inherits(x, c("Date", "POSIXt"))) x <- format(as.Date(x), "%Y-%m-%d")
    x <- x[!blank(x)]
    # Prefer a real value over a "TBD..." placeholder on another row.
    x <- x[order(grepl("^\\s*tbd", x, ignore.case = TRUE))]
    if (length(x)) as.character(x[1]) else NA_character_
  }
  acct <- do.call(rbind, lapply(split(rpt, key), function(g) {
    data.frame(
      name   = first_filled(get(g, "name")),
      id     = first_filled(get(g, "id")),
      csm    = first_filled(get(g, "csm")),
      region = first_filled(get(g, "region")),
      owner  = first_filled(get(g, "owner")),
      title  = first_filled(get(g, "title")),
      vision = first_filled(get(g, "vision")),
      architecture = first_filled(get(g, "architecture")),
      status = first_filled(get(g, "status")),
      date   = first_filled(get(g, "date")),
      eval   = first_filled(get(g, "eval")),
      deploy = first_filled(get(g, "deploy")),
      next_steps = first_filled(get(g, "next_steps")),
      stringsAsFactors = FALSE
    )
  }))
  rownames(acct) <- NULL
  acct$date <- format(.ao_date(acct$date), "%Y-%m-%d")
  # Evaluating = TRUE when Evaluation Status is Active or Completed (or a checkbox
  # TRUE); every other status, and blank, is FALSE. The Agentic Engagement
  # Overview facets on it. Same rule as agxEvaluating() in agentic_overview_assets.R.
  acct$evaluating <- grepl("^(active|completed?|true)$", tolower(trimws(as.character(acct$eval))))

  # ── Drop accounts whose renewal is already lost ───────────────────────────
  # Account Risk comes from the report itself when it has the column, and from
  # every renewal in op_list$renewals / op_list$forecast (Account ID, then
  # name). An account is dropped if any of those carries an excluded value.
  # Each hit is kept (with its risk value and renewal close date) so the page
  # can list the removed accounts on their own tab.
  acct$risk  <- NA_character_
  acct$close <- NA_character_
  out <- rep(FALSE, nrow(acct))
  if (length(exclude_risk)) {
    ex   <- tolower(trimws(exclude_risk))
    hit  <- function(x) tolower(trimws(as.character(x))) %in% ex
    drops <- list()
    add_drops <- function(df, id_col, nm_col, risk_col, close_col = NA) {
      df <- df[hit(df[[risk_col]]), , drop = FALSE]
      if (!nrow(df)) return(NULL)
      data.frame(
        id    = if (!is.na(id_col)) substr(trimws(as.character(df[[id_col]])), 1, 15) else NA_character_,
        nm    = if (!is.na(nm_col)) tolower(trimws(as.character(df[[nm_col]]))) else NA_character_,
        risk  = trimws(as.character(df[[risk_col]])),
        close = if (!is.na(close_col)) format(.ao_date(df[[close_col]]), "%Y-%m-%d") else NA_character_,
        stringsAsFactors = FALSE
      )
    }
    risk_col <- pick(c("Account Risk", "Renewal Risk", "Opportunity: Account Risk"))
    if (!is.na(risk_col)) {
      drops[["report"]] <- add_drops(rpt, col$id, col$name, risk_col,
                                     pick(c("Close Date", "Opportunity: Close Date")))
    }
    for (tb in c("renewals", "forecast")) {
      rn <- if (is.list(op_list)) op_list[[tb]] else NULL
      if (!is.data.frame(rn) || !"Account Risk" %in% names(rn)) next
      nc <- function(f) if (f %in% names(rn)) f else NA_character_
      drops[[tb]] <- add_drops(rn, nc("Account ID"), nc("Account Name"), "Account Risk", nc("Close Date"))
    }
    if (is.na(risk_col) && !is.list(op_list)) {
      warning("No Account Risk in the report and no op_list given, so nothing was excluded ",
              "for exclude_risk. Pass op_list = op_list.", call. = FALSE)
    }
    drops <- do.call(rbind, drops)
    if (!is.null(drops) && nrow(drops)) {
      id15 <- substr(trimws(acct$id), 1, 15)
      nm   <- tolower(trimws(acct$name))
      for (i in seq_len(nrow(acct))) {
        m <- drops[(!is.na(drops$id) & !is.na(id15[i]) & drops$id == id15[i]) |
                   (!is.na(drops$nm) & drops$nm == nm[i]), , drop = FALSE]
        if (!nrow(m)) next
        out[i] <- TRUE
        acct$risk[i]  <- paste(unique(m$risk), collapse = ", ")
        cl <- sort(stats::na.omit(m$close))
        if (length(cl)) acct$close[i] <- cl[1]
      }
    }
    if (any(out)) {
      message("agentic owner summary: excluded ", sum(out), " account(s) with Account Risk ",
              paste(exclude_risk, collapse = " / "), " (listed on the 'Removed for churn' tab).")
    }
  }

  # ── Optional op_list context ──────────────────────────────────────────────
  acct$arr <- NA_real_
  acct$adoption <- NA_character_
  # The matching op_list$accounts name, so the dashboard can open that
  # account's card (the report's spelling may differ from the account report's).
  acct$account <- NA_character_
  # Customer age, for the account cards (op_list$accounts, op_list_customer_tenure()).
  acct$months_as_customer <- NA_real_
  acct$customer_since     <- NA_character_
  oa <- if (is.list(op_list)) op_list$accounts else NULL
  if (is.data.frame(oa) && nrow(oa)) {
    idx <- rep(NA_integer_, nrow(acct))
    if ("Account ID" %in% names(oa)) {
      idx <- match(substr(trimws(acct$id), 1, 15), substr(trimws(as.character(oa$`Account ID`)), 1, 15),
                   incomparables = NA)
    }
    miss <- is.na(idx)
    if (any(miss) && "Account Name" %in% names(oa)) {
      idx[miss] <- match(tolower(trimws(acct$name[miss])), tolower(trimws(oa$`Account Name`)))
    }
    oc <- function(f) if (f %in% names(oa)) as.character(oa[[f]])[idx] else rep(NA_character_, nrow(acct))
    acct$account <- oc("Account Name")
    acct$months_as_customer <- suppressWarnings(as.numeric(oc("months_as_customer")))
    acct$customer_since     <- oc("customer_since")
    acct$arr <- suppressWarnings(as.numeric(extract_usd_amount(oc("Contract ARR Rollup"))))
    acct$region[is.na(acct$region)] <- oc("Region")[is.na(acct$region)]
    acct$csm[is.na(acct$csm)]       <- oc("Customer Success Manager")[is.na(acct$csm)]
    r <- tolower(trimws(oc("product_adoption_rating")))
    acct$adoption <- ifelse(grepl("^(green|amber|yellow|red|black)", r),
                            sub("^yellow$", "amber", sub("^(green|amber|yellow|red|black).*", "\\1", r)),
                            NA_character_)
  }
  acct$url <- ifelse(is.na(acct$id), NA_character_,
                     paste0("https://immuta.lightning.force.com/lightning/r/Account/", acct$id, "/view"))
  ord     <- order(tolower(acct$name))
  removed <- acct[ord[out[ord]], , drop = FALSE]
  acct    <- acct[ord[!out[ord]], setdiff(names(acct), c("risk", "close")), drop = FALSE]
  rownames(acct) <- NULL; rownames(removed) <- NULL
  list(data = acct, removed = removed)
}

# ISO or US-style date strings -> Date.
.ao_date <- function(x) {
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  us <- is.na(d) & !is.na(x)
  d[us] <- suppressWarnings(as.Date(x[us], format = "%m/%d/%Y"))
  d
}

# Literal placeholder substitution (no regex, no backslash handling).
.ao_fill <- function(tpl, key, val) {
  parts <- strsplit(tpl, key, fixed = TRUE)[[1]]
  paste(parts, collapse = val)
}

# The standalone page: a thin host around the shared Agentic Engagement
# Overview (R/agentic_overview_assets.R) — the same panel, rules and script as
# the dashboard tab. This file only supplies the page chrome (top bar, search,
# region / CSM pills), the base styles the panel borrows from the dashboard,
# and the globals / helpers the shared script expects.
.AO_TEMPLATE <- r"---(<!doctype html>
<html lang="en"><head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Agentic Engagement Overview</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap" rel="stylesheet">
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
  /* Tokens and base classes copied from report_all_accounts_html() so the
     shared panel looks the same here as on the dashboard. */
  :root {
    --im-black: #0D0D0D; --im-lime: #C8F135; --im-lime-dk: #A8CC18; --im-lime-lt: #F5FCD8; --im-white: #FFFFFF;
    --amber: #F59E0B; --amber-lt: #FEF3C7; --red: #EF4444; --red-lt: #FEE2E2;
    --text-1: #111111; --text-2: #555555; --text-3: #999999; --border: #E5E5E5;
    --surface: #F8F8F6; --surface-2: #F0F0EE;
    --radius: 10px; --radius-sm: 6px;
    --shadow: 0 1px 3px rgba(0,0,0,.06), 0 4px 12px rgba(0,0,0,.06); --shadow-lg: 0 8px 28px rgba(0,0,0,.12);
    --font: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }
  body { font-family: var(--font); font-size: 14px; line-height: 1.5; color: var(--text-1); background: var(--surface); }
  [hidden] { display: none !important; }
  .ao-top { background: var(--im-black); color: #fff; padding: 14px 24px; display: flex; align-items: center; gap: 16px; flex-wrap: wrap;
            position: sticky; top: 0; z-index: 10; }
  .ao-top h1 { font-size: 18px; font-weight: 800; letter-spacing: -.01em; }
  .ao-top .meta { font-size: 12px; color: #A8A8A8; }
  .ao-top .meta b { color: var(--im-lime); font-weight: 600; }
  .ao-top .ao-right { margin-left: auto; display: flex; align-items: center; gap: 12px; }
  .ao-top input { font: inherit; font-size: 13px; padding: 7px 12px; border-radius: var(--radius-sm); border: 1px solid #333;
                  background: #1C1C1C; color: #fff; min-width: 260px; }
  .count-badge { font-size: 12px; font-weight: 600; color: var(--im-lime); white-space: nowrap; }
  .ao-wrap { max-width: 1440px; margin: 0 auto; }
  .flat-filters { background: var(--im-white); border-bottom: 1px solid var(--border); padding: 10px 24px;
                  display: flex; align-items: center; gap: 6px; flex-wrap: wrap; }
  .flat-filters-csm { padding-top: 6px; border-top: 1px solid var(--surface-2); }
  .flat-pill { padding: 5px 12px; border-radius: var(--radius-sm); border: 1px solid var(--border); background: transparent;
               font-size: 12.5px; font-family: var(--font); color: var(--text-2); cursor: pointer; white-space: nowrap; }
  .flat-pill:hover { background: var(--im-lime-lt); border-color: #C8E870; color: var(--text-1); }
  .flat-pill.active { background: var(--im-black); color: var(--im-lime); border-color: var(--im-black); font-weight: 600; }
  .card { background: var(--im-white); border-radius: var(--radius); box-shadow: var(--shadow); margin-bottom: 8px; overflow: hidden;
          border: 1px solid var(--border); transition: box-shadow .18s, border-color .18s; }
  .card:hover { box-shadow: var(--shadow-lg); border-color: #BBBBBB; }
  .card.open { border-color: var(--im-black); }
  .card-header { display: flex; align-items: center; gap: 14px; padding: 14px 18px; cursor: pointer; user-select: none; transition: background .12s; }
  .card-header:hover { background: var(--surface); }
  .card-name { font-size: 14.5px; font-weight: 600; color: var(--text-1); flex: 1; min-width: 0; white-space: nowrap;
               overflow: hidden; text-overflow: ellipsis; letter-spacing: -.15px; }
  .card-arr { font-size: 14px; font-weight: 700; color: var(--text-1); white-space: nowrap; letter-spacing: -.2px; }
  .expand-icon { font-size: 10px; color: var(--text-3); transition: transform .2s; flex-shrink: 0; }
  .card.open .expand-icon { transform: rotate(180deg); }
  .card-body { display: none; padding: 0 18px 18px; border-top: 1px solid var(--border); }
  .card.open .card-body { display: block; }
  .cc-link { display: inline-flex; align-items: center; gap: 3px; font-size: 11.5px; font-weight: 700; font-family: var(--font);
             color: var(--im-black); text-decoration: none; white-space: nowrap; flex-shrink: 0; padding: 5px 12px;
             background: var(--im-white); border: 1.5px solid var(--im-black); border-radius: 4px; transition: background .14s, color .14s; }
  .cc-link:hover { background: var(--im-black); color: var(--im-lime); }
  .field-label { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 1px; color: var(--text-3); margin-bottom: 3px; }
  .field-value { font-size: 13.5px; color: var(--text-1); font-weight: 500; }
  .em-dash { color: var(--border); }
  .tabs { display: flex; border-bottom: 1px solid var(--border); margin: 14px 0; }
  .tab-btn { padding: 9px 16px; border: none; background: none; font-size: 13px; font-weight: 500; font-family: var(--font);
             color: var(--text-3); cursor: pointer; border-bottom: 2px solid transparent; margin-bottom: -1px; transition: color .12s; }
  .tab-btn:hover { color: var(--text-1); }
  .tab-btn.active { color: var(--text-1); border-bottom-color: var(--im-lime); font-weight: 600; }
  .flat-table { width: 100%; border-collapse: collapse; font-size: 13px; margin-top: 16px; }
  .flat-table th { text-align: left; font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 1px; color: var(--text-3);
                   padding: 9px 12px; border-bottom: 1.5px solid var(--border); background: var(--im-white); white-space: nowrap; }
  .flat-table td { padding: 10px 12px; border-bottom: 1px solid var(--surface); color: var(--text-1); vertical-align: middle; }
  .flat-table tr:hover td { background: var(--im-lime-lt); }
  .flat-table tr:last-child td { border-bottom: none; }
  .opp-link { color: var(--text-1); text-decoration: none; font-weight: 500; }
  .opp-link:hover { text-decoration: underline; text-decoration-color: var(--im-lime); color: var(--im-black); }
  .cov-num { text-align: right; white-space: nowrap; }
  .cov-body { padding: 0 18px 8px; }
  @media (max-width: 700px) { .ao-top input { min-width: 0; width: 100%; } .ao-top .ao-right { margin-left: 0; width: 100%; } }
{{AGX_CSS}}
</style></head>
<body>
<div class="ao-top">
  <h1>Agentic Engagement Overview</h1>
  <div class="meta">{{UPDATED}} &middot; Salesforce report <b>{{REPORT}}</b></div>
  <div class="ao-right">
    <input id="search" type="search" placeholder="Search account, owner, vision, next steps..." oninput="searchTerm = this.value.trim(); render()">
    <div class="count-badge" id="count-badge"></div>
  </div>
</div>
<div class="ao-wrap">
  <div class="flat-filters" id="ao-region-filters"></div>
  <div class="flat-filters flat-filters-csm" id="ao-csm-filters"></div>
{{AGX_PANEL}}
</div>

<script>
const AGENTIC_DATA = {{AGENTIC}};
const ACCOUNTS = [];
const AGX_STANDALONE = true;
let activeRegion = "all", activeCSM = "all", searchTerm = "";
const PAGES = { adoption: { active: "agentic" } };
function switchPage() {}
function goToAccount() {}

// Same helpers as the dashboard (report_all_accounts_html()).
function esc(s) {
  if (!s) return "";
  return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}
function adoptBlank(v) {
  if (v === null || v === undefined) return true;
  const s = String(v).trim();
  return !s || s === "-" || s === "—" || s === "null";
}
function covNum(v) {
  const n = Number(v);
  return (v === null || v === undefined || v === "" || !isFinite(n)) ? null : n;
}
function ratingColour(v) {
  if (adoptBlank(v)) return "";
  const m = String(v).trim().toLowerCase().match(/^(green|amber|yellow|red|black)/);
  if (!m) return "";
  return m[1] === "yellow" ? "amber" : m[1];
}
function setCount(id, n) { const el = document.getElementById(id); if (el) el.textContent = n; }
function toggleCard(header) { header.parentElement.classList.toggle("open"); }
function setRegion(r) { activeRegion = r; render(); }
function setCSM(c) { activeCSM = c; render(); }

{{AGX_JS}}

// Region / CSM pills from the agentic rows (counted and removed alike).
function aoPills(id, values, active, fn, allLabel) {
  const el = document.getElementById(id);
  const vals = [...new Set(values.filter(v => !adoptBlank(v)))].sort();
  el.innerHTML = [`<button class="flat-pill${active === "all" ? " active" : ""}" data-v="all">${allLabel}</button>`]
    .concat(vals.map(v => `<button class="flat-pill${active === v ? " active" : ""}" data-v="${esc(v)}">${esc(v)}</button>`)).join("");
  el.querySelectorAll(".flat-pill").forEach(b => b.onclick = () => fn(b.dataset.v));
}
function render() {
  const all = AG_ROWS.concat(AG_REMOVED);
  aoPills("ao-region-filters", all.map(a => a.region), activeRegion, setRegion, "All Regions");
  aoPills("ao-csm-filters", all.map(a => agxBlank(a.csm) ? "Unassigned" : String(a.csm).trim()), activeCSM, setCSM, "All CSMs");
  renderAgentic();
}
render();
</script>
</body></html>)---"
