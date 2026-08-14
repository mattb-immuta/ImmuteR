#' Generate an HTML dashboard of all account summaries.
#'
#' Produces a self-contained, single-file HTML page listing every account in
#' \code{op_list} in alphabetical order with full detail cards.  The page
#' includes:
#' \itemize{
#'   \item A search box for quick account lookup by name.
#'   \item Region filter pills to narrow the visible set.
#'   \item A-Z alphabet jump bar for instant scrolling.
#'   \item Expandable detail cards showing Account Overview, Architecture,
#'         Open Renewals, and Expansion Pipeline for each account.
#' }
#'
#' @param op_list Named list returned by \code{enrich_opportunities()}, expected
#'   to contain \code{accounts}, \code{renewals}, \code{expansions}, and
#'   \code{active_orders} elements.
#' @param output_path Character. Path where the HTML file will be written.
#'   Parent directories are created automatically.
#'   Defaults to \code{"output/immuta_accounts.html"}.
#'
#' @return Invisibly returns \code{output_path}.
#'
#' @examples
#' \dontrun{
#'   report_all_accounts_html(op_list)
#'   report_all_accounts_html(op_list, "reports/accounts.html")
#' }
#' @export
report_all_accounts_html <- function(op_list,
                                     output_path = "output/immuta_accounts.html") {

  # Return the first candidate column present on `row` (a one-row data frame
  # slice), or NULL if none of the candidates exist. Lets us tolerate slightly
  # different Salesforce field labels (e.g. "Next Step" vs "Next Steps")
  # without erroring when a report hasn't been refreshed with the new field yet.
  .first_col <- function(row, candidates) {
    for (cn in candidates) {
      if (cn %in% names(row)) return(row[[cn]])
    }
    NULL
  }

  # Salesforce reports emit a literal "-" for an unset field rather than a
  # true blank/NA. Treat that as missing so it renders as an em-dash instead
  # of a stray hyphen.
  .blank_dash <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    if (!is.na(x) && trimws(as.character(x)) == "-") return(NA)
    x
  }

  # Ensure output directory exists
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  accounts     <- op_list$accounts
  renewals     <- op_list$renewals
  expansions   <- op_list$expansions
  active_orders <- op_list$active_orders

  # Consumption + support lookups, so the Overview tab can carry the same three
  # blocks the PNG health summary does. Both are optional — an SFDC-only
  # op_list still renders, with em-dashes where the figures would be.
  consumption <- op_list$consumption$low_consumption
  support     <- op_list$support

  # Row lookup by account name (consumption) / account ID (support).
  .lookup_row <- function(df, key_col, key) {
    if (is.null(df) || !is.data.frame(df) || !key_col %in% names(df)) return(NULL)
    hit <- df[!is.na(df[[key_col]]) & df[[key_col]] == key, , drop = FALSE]
    if (nrow(hit) == 0) NULL else hit[1, ]
  }

  .num_or_na <- function(row, col) {
    if (is.null(row) || !col %in% names(row)) return(NA_real_)
    suppressWarnings(as.numeric(row[[col]][1]))
  }

  # Comma-grouped integer, em-dash when missing — matches .tile_int().
  .int_or_dash <- function(x) {
    if (length(x) == 0 || is.na(x)) "—"
    else format(round(x), big.mark = ",", trim = TRUE)
  }

  accounts_sorted <- accounts[order(accounts$`Account Name`), ]

  # Build per-account JSON data ------------------------------------------------
  account_records <- lapply(seq_len(nrow(accounts_sorted)), function(i) {

    acct    <- accounts_sorted[i, ]
    acct_id <- acct$`Account ID`
    aname   <- acct$`Account Name`

    # --- Renewals ---
    ren_list <- list()
    if (!is.null(renewals) && is.data.frame(renewals) && nrow(renewals) > 0) {
      acct_ren <- renewals[!is.na(renewals$`Account ID`) &
                           renewals$`Account ID` == acct_id, ]
      if (nrow(acct_ren) > 0) {
        acct_ren <- acct_ren[order(as.Date(acct_ren$`Current Contract End Date`)), ]
        ren_list <- lapply(seq_len(nrow(acct_ren)), function(j) {
          r <- acct_ren[j, ]
          list(
            name          = safe_val(r[["Opportunity Name"]]),
            url           = safe_val(r[["opportunity_url"]]),
            amount        = fmt_usd(r[["Amount"]]),
            amount_raw    = suppressWarnings(as.numeric(r[["Amount"]])),
            end_date      = fmt_date(r[["Current Contract End Date"]]),
            end_date_raw  = tryCatch(format(as.Date(r[["Current Contract End Date"]]), "%Y-%m-%d"), error = function(e) ""),
            churn_quarter = safe_val(r[["Churn Quarter"]]),
            risk          = safe_val(r[["Account Risk"]]),
            next_steps    = safe_val(.first_col(r, c("Next Step", "Next Steps")))
          )
        })
      }
      # churn summary for card header chip
      qs <- acct_ren$`Churn Quarter`
    } else {
      acct_ren <- data.frame()
      qs       <- character(0)
    }

    # --- Expansions ---
    exp_list <- list()
    if (!is.null(expansions) && is.data.frame(expansions) && nrow(expansions) > 0) {
      acct_exp <- expansions[!is.na(expansions$`Account ID`) &
                             expansions$`Account ID` == acct_id, ]
      if (nrow(acct_exp) > 0) {
        acct_exp <- acct_exp[order(as.Date(acct_exp$`Close Date`)), ]
        exp_list <- lapply(seq_len(nrow(acct_exp)), function(j) {
          e <- acct_exp[j, ]
          list(
            name             = safe_val(e[["Opportunity Name"]]),
            url              = safe_val(e[["opportunity_url"]]),
            owner            = safe_val(e[["Opportunity Owner"]]),
            amount           = fmt_usd(e[["Amount"]]),
            amount_raw       = suppressWarnings(as.numeric(e[["Amount"]])),
            fiscal_period    = safe_val(e[["Fiscal Period"]]),
            close_date       = fmt_date(e[["Close Date"]]),
            close_date_raw   = tryCatch(format(as.Date(e[["Close Date"]]), "%Y-%m-%d"), error = function(e) ""),
            next_steps       = safe_val(.first_col(e, c("Next Step", "Next Steps")))
          )
        })
      }
    }

    # --- Products ---
    prod_list <- list()
    if (!is.null(active_orders) &&
        is.data.frame(active_orders) &&
        nrow(active_orders) > 0 &&
        "Account Name: Account Name" %in% names(active_orders)) {
      ord <- active_orders[
        !is.na(active_orders$`Account Name: Account Name`) &
        active_orders$`Account Name: Account Name` == aname, ]
      if (!is.null(ord) && nrow(ord) > 0) {
        prods <- ord[order(ord$`Product: Product Name`), ]
        prod_list <- lapply(seq_len(nrow(prods)), function(j) {
          list(
            name     = safe_val(prods[j, "Product: Product Name"]),
            quantity = safe_val(prods[j, "Quantity"])
          )
        })
      }
    }

    # --- SaaS / version ---
    is_saas <- isTRUE(acct$`Is SaaS Customer` %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
    unsupported <- if (!is_saas) {
      isTRUE(acct$`Customer on Unsupported Version` %in%
               c(TRUE, "Yes", "TRUE", "true", "yes"))
    } else NA

    # --- Churn quarters summary (for card header chip) ---
    # qs was populated inside the renewals block above
    qs <- qs[!is.na(qs) & qs != ""]
    churn_summary <- if (length(qs) == 0) "" else paste(qs, collapse = ", ")

    # --- Consumption + support rows for the health-summary blocks ---
    c_row <- .lookup_row(consumption, "Account Name", aname)
    s_row <- .lookup_row(support,     "Account Name", aname)

    reg_raw <- .num_or_na(c_row, "Registered Users")
    lic_raw <- .num_or_na(c_row, "Licensed Users")
    rat_raw <- .num_or_na(c_row, "Consumption Ratio")
    if (is.na(rat_raw) && !is.na(reg_raw) && !is.na(lic_raw) && lic_raw > 0) {
      rat_raw <- reg_raw / lic_raw
    }

    list(
      id            = gsub("[^A-Za-z0-9]", "_", aname),
      name          = aname,
      account_id    = acct_id,
      customer_central_url = {
        cc <- acct$customer_central_url
        if (!is.null(cc) && !is.na(cc) && nchar(as.character(cc)) > 0) as.character(cc) else ""
      },
      region        = safe_val(acct$Region),
      arr           = fmt_usd(acct$`Contract ARR Rollup`),
      arr_raw       = suppressWarnings(as.numeric(acct$`Contract ARR Rollup`)),
      csm           = safe_val(acct$`Customer Success Manager`),
      account_owner = safe_val(acct$`Account Owner`),
      field_cto        = safe_val(acct$`Field CTO`),
      consumption_link = {
        curl <- acct$consumption_url
        if (!is.null(curl) && !is.na(curl) && nchar(as.character(curl)) > 0) {
          paste0('<a class="consumption-link" href="', html_escape(as.character(curl)),
                 '" target="_blank" rel="noopener"',
                 ' onclick="event.stopPropagation()">Current Consumption &#8599;</a>')
        } else ""
      },
      is_saas       = is_saas,
      unsupported   = isTRUE(unsupported),
      churn_summary = churn_summary,
      lifecycle_stage = safe_val(.blank_dash(.first_col(acct, c("Lifecycle Stage")))),
      pbac_adoption   = safe_val(.blank_dash(.first_col(acct, c("PBAC Adoption")))),
      comply_adoption = safe_val(.blank_dash(.first_col(acct, c("Comply Adoption")))),
      request_adoption = safe_val(.blank_dash(.first_col(acct, c("Request Adoption")))),
      agentic_governance_adoption = safe_val(.blank_dash(.first_col(acct, c("Agentic Governance Adoption")))),

      # --- Health-summary blocks, mirroring generate_slide_tile() ---
      registered_users = .int_or_dash(reg_raw),
      licensed_users   = .int_or_dash(lic_raw),
      consumption_ratio = if (is.na(rat_raw)) "—" else paste0(round(rat_raw * 1000) / 10, "%"),
      consumption_ratio_raw = rat_raw,
      immuta_version   = safe_val(.blank_dash(
        if (!is.null(c_row)) c_row[["Immuta Version"]] else NA)),
      inc_90d          = .int_or_dash(.num_or_na(s_row, "cases_90d")),
      inc_360d         = .int_or_dash(.num_or_na(s_row, "cases_360d")),
      custx_90d        = .int_or_dash(.num_or_na(s_row, "escalations_90d")),
      custx_open       = .int_or_dash(.num_or_na(s_row, "escalations_open")),
      custx_90d_raw    = .num_or_na(s_row, "escalations_90d"),
      custx_open_raw   = .num_or_na(s_row, "escalations_open"),
      last_qbr         = fmt_date(.first_col(acct, c("Date Last QBR"))),
      next_qbr         = fmt_date(.first_col(acct, c("Date Next QBR"))),
      qbr_note         = safe_val(.blank_dash(.first_col(acct, c("QBR Note")))),

      renewals      = ren_list,
      expansions    = exp_list,
      products      = prod_list
    )
  })

  # Emit JSON ------------------------------------------------------------------
  json_data <- .to_json(account_records)

  # Regions for filter pills ---------------------------------------------------
  regions <- sort(unique(accounts$Region))
  regions <- regions[!is.na(regions) & regions != ""]

  # CSMs for filter pills ------------------------------------------------------
  csms <- sort(unique(accounts$`Customer Success Manager`))
  csms <- csms[!is.na(csms) & csms != ""]

  # Lifecycle stages for the Self-Managed tab's filter pills -------------------
  # (scoped to self-managed accounts only, since that's the only tab that uses it)
  self_managed_is_saas <- vapply(accounts$`Is SaaS Customer`, function(v) {
    isTRUE(v %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
  }, logical(1))
  lifecycle_stages <- sort(unique(accounts$`Lifecycle Stage`[!self_managed_is_saas]))
  lifecycle_stages <- lifecycle_stages[!is.na(lifecycle_stages) &
                                       !lifecycle_stages %in% c("", "-")]

  # Snapshot dates for the diff-date picker (newest first) --------------------
  snap_files <- list.files(dirname(output_path),  # look relative to output, fall back to data/
                            pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
  if (length(snap_files) == 0)
    snap_files <- list.files("data/", pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
  snap_dates <- sort(gsub("opportunities_(.+)\\.RData", "\\1", snap_files), decreasing = TRUE)

  # Changes diff JSON ----------------------------------------------------------
  changes_json     <- .build_changes_json(op_list)
  consumption_json <- .build_consumption_json(op_list)

  # Build HTML -----------------------------------------------------------------
  html <- .rah_html(json_data, regions, csms, snap_dates, changes_json, consumption_json,
                    lifecycle_stages)

  writeLines(html, output_path, useBytes = FALSE)
  message(".. wrote account HTML dashboard to ", output_path)
  invisible(output_path)
}


# ---- Internal: minimal JSON serialiser (no jsonlite dependency) -------------

.to_json <- function(x) {
  if (is.null(x)) return("null")

  # ── logical ──────────────────────────────────────────────────────────────────
  if (is.logical(x) && length(x) == 1)
    return(if (is.na(x)) "null" else if (isTRUE(x)) "true" else "false")
  if (is.logical(x))
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))

  # ── numeric — use | not || so vectors don't error ────────────────────────────
  if (is.numeric(x) && length(x) == 1)
    return(if (is.na(x) | is.nan(x) | is.infinite(x)) "null" else as.character(x))
  if (is.numeric(x))
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))

  # ── character ─────────────────────────────────────────────────────────────────
  if (is.character(x) && length(x) == 1) {
    if (is.na(x)) return("null")
    x <- gsub("\\\\", "\\\\\\\\", x)
    x <- gsub('"',  '\\\\"',  x)
    x <- gsub('\n', '\\\\n', x)
    x <- gsub('\r', '\\\\r', x)
    x <- gsub('\t', '\\\\t', x)
    return(paste0('"', x, '"'))
  }
  if (is.character(x))
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))

  # ── data.frame — serialize as array of row objects ───────────────────────────
  # Must come before is.list() since data frames are also lists.
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      row <- as.list(x[i, , drop = FALSE])
      # as.list on a 1-row data frame gives 1-element lists per column; unwrap them
      row <- lapply(row, function(v) if (is.list(v)) v[[1L]] else v)
      pairs <- mapply(function(k, v) paste0('"', k, '":', .to_json(v)),
                      names(row), row, SIMPLIFY = TRUE)
      paste0("{", paste(pairs, collapse = ","), "}")
    })
    return(paste0("[", paste(rows, collapse = ","), "]"))
  }

  # ── list ──────────────────────────────────────────────────────────────────────
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms)) {
      pairs <- mapply(function(k, v) paste0('"', k, '":', .to_json(v)),
                      nms, x, SIMPLIFY = TRUE)
      return(paste0("{", paste(pairs, collapse = ","), "}"))
    } else {
      return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))
    }
  }

  '"?"'
}


# ---- Internal: convert a data frame to a list of row objects ---------------

.df_to_rows <- function(df, cols = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(list())
  if (!is.null(cols)) df <- df[, intersect(cols, names(df)), drop = FALSE]
  if (ncol(df) == 0)  return(list())
  lapply(seq_len(nrow(df)), function(i) {
    row <- lapply(names(df), function(nm) {
      v <- df[[nm]][i]
      if (is.factor(v))        return(as.character(v))
      if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
      if (is.logical(v))       return(isTRUE(v))
      if (is.numeric(v))       return(if (is.na(v)) NULL else as.numeric(v))
      s <- tryCatch(as.character(v), error = function(e) "")
      if (length(s) == 0 || is.na(s) || s == "NA") return(NULL)
      s
    })
    names(row) <- names(df)
    row
  })
}


# ---- Internal: build the changes JSON blob from op_list --------------------

.build_changes_json <- function(op_list) {
  cd <- op_list[["changes_detail"]]
  cs <- op_list[["changes"]]          # summary data frame

  if (is.null(cd)) {
    return(.to_json(list(has_data = FALSE, diff_date = "", summary = list())))
  }

  diff_date_str <- tryCatch(
    format(as.Date(cd$diff_date), "%Y-%m-%d"),
    error = function(e) as.character(cd$diff_date)
  )

  .to_json(list(
    has_data      = TRUE,
    diff_date     = diff_date_str,
    summary       = .df_to_rows(cs),
    accounts      = list(
      added         = .df_to_rows(cd$accounts$added,
                        c("Account ID", "Account Name", "Region",
                          "Contract ARR Rollup", "Customer Success Manager")),
      removed       = .df_to_rows(cd$accounts$removed,
                        c("Account ID", "Account Name", "Region",
                          "Contract ARR Rollup", "Customer Success Manager")),
      field_changes = .df_to_rows(cd$accounts$field_changes,
                        c("Account Name",
                          "Contract ARR Rollup.new",      "Contract ARR Rollup.old",
                          "Customer Success Manager.new", "Customer Success Manager.old",
                          "Account Owner.new",            "Account Owner.old",
                          "Field CTO.new",                "Field CTO.old",
                          "Region.new",                   "Region.old",
                          "Adoption Phase.new",           "Adoption Phase.old"))
    ),
    renewals      = list(
      added         = .df_to_rows(cd$renewals$added,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Close Date", "Account Risk")),
      removed       = .df_to_rows(cd$renewals$removed,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Close Date", "Account Risk")),
      field_changes = .df_to_rows(cd$renewals$field_changes,
                        c("Opportunity Name", "Account Name",
                          "Account Risk.new",    "Account Risk.old",
                          "Churn Forecast.new",  "Churn Forecast.old",
                          "Amount.new",          "Amount.old",
                          "Close Date.new",      "Close Date.old",
                          "risk_changed", "forecast_changed",
                          "amount_changed", "date_changed", "forecast_delta"))
    ),
    expansions    = list(
      added         = .df_to_rows(cd$expansions$added,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Fiscal Period", "Close Date", "Opportunity Owner")),
      removed       = .df_to_rows(cd$expansions$removed,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Fiscal Period", "Close Date", "Opportunity Owner")),
      field_changes = .df_to_rows(cd$expansions$field_changes,
                        c("Opportunity Name", "Account Name",
                          "Amount.new", "Amount.old",
                          "Fiscal Period.new", "Fiscal Period.old",
                          "amount_changed", "period_changed"))
    ),
    csm_table     = list(
      added         = .df_to_rows(cd$csm_table$added),
      removed       = .df_to_rows(cd$csm_table$removed),
      reassignments = .df_to_rows(cd$csm_table$reassignments)
    ),
    saas_pipeline = list(
      added         = .df_to_rows(cd$saas_pipeline$added),
      removed       = .df_to_rows(cd$saas_pipeline$removed),
      field_changes = .df_to_rows(cd$saas_pipeline$field_changes)
    )
  ))
}


.build_consumption_json <- function(op_list) {
  con <- op_list$consumption
  cd  <- op_list[["changes_detail"]]

  has_data <- !is.null(con) && !is.null(con$low_consumption) && nrow(con$low_consumption) > 0

  diff_date_str <- if (!is.null(cd)) {
    tryCatch(format(as.Date(cd$diff_date), "%Y-%m-%d"),
             error = function(e) as.character(cd$diff_date))
  } else {
    ""
  }

  rows_df <- if (has_data) con$low_consumption else NULL

  # Merge in the per-account "Changes in ..." deltas (every account present in
  # both snapshots, not just the ones flagged in cd$consumption$field_changes)
  # so the main Consumption tab table can show them as ordinary columns.
  deltas <- cd$consumption$all_deltas
  if (!is.null(rows_df) && !is.null(deltas) && nrow(deltas) > 0) {
    key <- if ("Account ID" %in% names(deltas)) "Account ID" else "Account Name"
    if (key %in% names(rows_df)) {
      rows_df <- merge(rows_df, deltas, by = key, all.x = TRUE)
    }
  }

  .to_json(list(
    has_data  = has_data,
    rows      = if (!is.null(rows_df)) .df_to_rows(rows_df) else list(),
    diff_date = diff_date_str,
    changes   = list(
      field_changes = .df_to_rows(cd$consumption$field_changes)
    )
  ))
}

# ---- Internal: full HTML page template -------------------------------------

.rah_html <- function(json_data, regions, csms = character(0), snap_dates = character(0), changes_json = "null", consumption_json = "null", lifecycle_stages = character(0)) {

  region_pills <- paste(
    vapply(regions, function(r) {
      re <- html_escape(r)
      paste0('<button class="pill" data-region="', re, '">', re, '</button>')
    }, character(1)),
    collapse = "\n    "
  )

  csm_pills <- paste(
    vapply(csms, function(cm) {
      ce <- html_escape(cm)
      paste0('<button class="pill" data-csm="', ce, '">', ce, '</button>')
    }, character(1)),
    collapse = "\n    "
  )

  sm_lifecycle_pills <- paste(
    vapply(lifecycle_stages, function(l) {
      le <- html_escape(l)
      paste0('<button class="flat-pill" data-sm-lifecycle="', le,
             '" onclick="setSMLifecycle(\'', le, '\')">', le, '</button>')
    }, character(1)),
    collapse = "\n    "
  )

  tmpl <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Immuta | Customer Accounts</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap" rel="stylesheet">
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

  :root {
    /* ── Immuta brand palette (from immuta.com) ── */
    --im-black:    #0D0D0D;   /* near-black — topbar / hero bg */
    --im-lime:     #C8F135;   /* neon chartreuse — THE brand accent */
    --im-lime-dk:  #A8CC18;   /* darker lime for hover */
    --im-lime-lt:  #F5FCD8;   /* pale lime tint — soft backgrounds */
    --im-white:    #FFFFFF;

    /* ── Semantic colors ── */
    --amber:       #F59E0B;
    --amber-lt:    #FEF3C7;
    --red:         #EF4444;
    --red-lt:      #FEE2E2;

    /* ── Neutrals ── */
    --text-1:      #111111;   /* primary text */
    --text-2:      #555555;   /* secondary */
    --text-3:      #999999;   /* muted / labels */
    --border:      #E5E5E5;
    --surface:     #F8F8F6;   /* warm off-white page bg */
    --surface-2:   #F0F0EE;   /* slightly deeper for hover */

    /* ── Tokens ── */
    --radius:      10px;
    --radius-sm:   6px;
    --shadow:      0 1px 3px rgba(0,0,0,.06), 0 4px 12px rgba(0,0,0,.06);
    --shadow-lg:   0 8px 28px rgba(0,0,0,.12);
    --sidebar:     264px;
    --topbar-h:    60px;
    --tabs-h:      48px;
    --font:        "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }

  body {
    font-family: var(--font);
    font-size: 14px;
    line-height: 1.5;
    color: var(--text-1);
    background: var(--surface);
    display: flex;
    flex-direction: column;
    min-height: 100vh;
    -webkit-font-smoothing: antialiased;
  }

  /* ── Top bar ──────────────────────────────────────────────────────────── */
  .topbar {
    position: sticky;
    top: 0;
    z-index: 100;
    background: var(--im-black);
    color: var(--im-white);
    padding: 0 24px;
    height: var(--topbar-h);
    display: flex;
    align-items: center;
    gap: 20px;
  }

  .topbar-logo {
    display: flex;
    align-items: center;
    gap: 14px;
    text-decoration: none;
    flex-shrink: 0;
  }
  .topbar-logo svg { display: block; flex-shrink: 0; }
  .topbar-section {
    font-size: 13px;
    font-weight: 400;
    color: rgba(255,255,255,.4);
    padding-left: 14px;
    border-left: 1px solid rgba(255,255,255,.15);
  }

  .search-wrap {
    flex: 1;
    max-width: 420px;
    position: relative;
    margin-left: 4px;
  }
  .search-wrap input {
    width: 100%;
    padding: 8px 14px 8px 38px;
    border: 1px solid rgba(255,255,255,.12);
    border-radius: var(--radius-sm);
    font-size: 13px;
    font-family: var(--font);
    background: rgba(255,255,255,.06);
    color: var(--im-white);
    outline: none;
    transition: background .15s, border-color .15s;
  }
  .search-wrap input::placeholder { color: rgba(255,255,255,.3); }
  .search-wrap input:focus {
    background: rgba(255,255,255,.1);
    border-color: var(--im-lime);
  }
  .search-icon {
    position: absolute;
    left: 11px;
    top: 50%;
    transform: translateY(-50%);
    color: rgba(255,255,255,.3);
    font-size: 14px;
    pointer-events: none;
  }
  .count-badge {
    font-size: 12px;
    color: rgba(255,255,255,.38);
    white-space: nowrap;
    margin-left: auto;
  }

  /* ── Layout ───────────────────────────────────────────────────────────── */
  .layout { display: flex; flex: 1; }

  /* ── Sidebar ──────────────────────────────────────────────────────────── */
  .sidebar {
    width: var(--sidebar);
    flex-shrink: 0;
    background: var(--im-white);
    border-right: 1px solid var(--border);
    position: sticky;
    top: calc(var(--topbar-h) + var(--tabs-h));
    height: calc(100vh - var(--topbar-h) - var(--tabs-h));
    overflow-y: auto;
    padding: 20px 0 40px;
    scrollbar-width: thin;
    scrollbar-color: var(--border) transparent;
  }
  .sidebar-section { margin-bottom: 28px; }
  .sidebar-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    color: var(--text-3);
    padding: 0 18px 10px;
  }
  .region-filters {
    display: flex;
    flex-direction: column;
    gap: 1px;
    padding: 0 10px;
  }
  .pill {
    text-align: left;
    padding: 7px 10px;
    border-radius: var(--radius-sm);
    border: none;
    background: transparent;
    font-size: 13px;
    font-family: var(--font);
    color: var(--text-2);
    cursor: pointer;
    transition: background .12s, color .12s;
  }
  .pill:hover { background: var(--im-lime-lt); color: var(--text-1); }
  .pill.active {
    background: var(--im-black);
    color: var(--im-lime);
    font-weight: 600;
  }

  .alpha-jump {
    display: flex;
    flex-wrap: wrap;
    gap: 2px;
    padding: 0 14px;
  }
  .alpha-jump a {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 25px;
    height: 25px;
    border-radius: 4px;
    font-size: 11.5px;
    font-weight: 600;
    color: var(--text-2);
    text-decoration: none;
    transition: background .12s, color .12s;
  }
  .alpha-jump a:hover { background: var(--im-lime); color: var(--im-black); }
  .alpha-jump a.has-accounts { color: var(--text-1); font-weight: 700; }
  .alpha-jump a.disabled { color: var(--border); pointer-events: none; }

  /* ── Main content ─────────────────────────────────────────────────────── */
  .main {
    flex: 1;
    padding: 24px 28px 48px;
    min-width: 0;
  }
  .letter-group { margin-bottom: 6px; }
  .letter-heading {
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 1.4px;
    text-transform: uppercase;
    color: var(--text-3);
    padding: 14px 0 8px;
    border-bottom: 1px solid var(--border);
    margin-bottom: 12px;
  }

  /* ── Account card ─────────────────────────────────────────────────────── */
  .card {
    background: var(--im-white);
    border-radius: var(--radius);
    box-shadow: var(--shadow);
    margin-bottom: 8px;
    overflow: hidden;
    border: 1px solid var(--border);
    transition: box-shadow .18s, border-color .18s;
  }
  .card:hover { box-shadow: var(--shadow-lg); border-color: #BBBBBB; }
  .card.open  { border-color: var(--im-black); }

  .card-header {
    display: flex;
    align-items: center;
    gap: 14px;
    padding: 14px 18px;
    cursor: pointer;
    user-select: none;
    transition: background .12s;
  }
  .card-header:hover { background: var(--surface); }

  .card-name {
    font-size: 14.5px;
    font-weight: 600;
    color: var(--text-1);
    flex: 1;
    min-width: 0;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    letter-spacing: -.15px;
  }
  .card-arr {
    font-size: 14px;
    font-weight: 700;
    color: var(--text-1);
    white-space: nowrap;
    letter-spacing: -.2px;
  }

  /* Consumption link — lime pill matching "Book a Demo" CTA style */
  .consumption-link {
    display: inline-flex;
    align-items: center;
    gap: 3px;
    font-size: 11.5px;
    font-weight: 700;
    font-family: var(--font);
    color: var(--im-black);
    text-decoration: none;
    white-space: nowrap;
    flex-shrink: 0;
    padding: 5px 12px;
    background: var(--im-lime);
    border-radius: 4px;
    transition: background .14s;
  }
  .consumption-link:hover { background: var(--im-lime-dk); }

  /* Customer Central link — dark outline style to distinguish from lime CTA */
  .cc-link {
    display: inline-flex;
    align-items: center;
    gap: 3px;
    font-size: 11.5px;
    font-weight: 700;
    font-family: var(--font);
    color: var(--im-black);
    text-decoration: none;
    white-space: nowrap;
    flex-shrink: 0;
    padding: 5px 12px;
    background: var(--im-white);
    border: 1.5px solid var(--im-black);
    border-radius: 4px;
    transition: background .14s, color .14s;
  }
  .cc-link:hover { background: var(--im-black); color: var(--im-lime); }

  .card-meta {
    display: flex;
    align-items: center;
    gap: 8px;
    flex-shrink: 0;
  }
  .tag {
    font-size: 10.5px;
    font-weight: 600;
    padding: 3px 9px;
    border-radius: 4px;
    white-space: nowrap;
    letter-spacing: .1px;
    border: 1px solid transparent;
  }
  .tag-region   { background: #F0F0EE; color: var(--text-2); border-color: var(--border); }
  .tag-saas     { background: var(--im-lime-lt); color: #5A7A00; border-color: #C8E870; }
  .tag-sm-sup   { background: var(--amber-lt);   color: #92400E; border-color: #FCD34D; }
  .tag-sm-unsup { background: var(--red-lt);      color: #991B1B; border-color: #FCA5A5; }
  .tag-churn    { background: var(--amber-lt);   color: #92400E; border-color: #FCD34D; }
  .tag-renew    { background: var(--red-lt);     color: #991B1B; border-color: #FCA5A5; }

  .expand-icon {
    font-size: 10px;
    color: var(--text-3);
    transition: transform .2s;
    flex-shrink: 0;
  }
  .card.open .expand-icon { transform: rotate(180deg); }

  .card-body {
    display: none;
    padding: 0 18px 18px;
    border-top: 1px solid var(--border);
  }
  .card.open .card-body { display: block; }

  /* ── Tabs ─────────────────────────────────────────────────────────────── */
  .tabs {
    display: flex;
    border-bottom: 1px solid var(--border);
    margin: 14px 0;
  }
  .tab-btn {
    padding: 9px 16px;
    border: none;
    background: none;
    font-size: 13px;
    font-weight: 500;
    font-family: var(--font);
    color: var(--text-3);
    cursor: pointer;
    border-bottom: 2px solid transparent;
    margin-bottom: -1px;
    transition: color .12s;
  }
  .tab-btn:hover { color: var(--text-1); }
  .tab-btn.active {
    color: var(--text-1);
    border-bottom-color: var(--im-lime);
    font-weight: 600;
  }
  .tab-pane { display: none; }
  .tab-pane.active { display: block; }

  /* ── Field grid ───────────────────────────────────────────────────────── */
  .field-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
    gap: 16px;
    padding: 6px 0;
  }
  /* Overview tab: three fixed columns, each stacking its fields top-to-bottom.
     Commercial on the left, account team in the middle, adoption on the right.
     Deliberately not auto-fill — the grouping is the point, so the columns must
     not reflow into an arbitrary number of tracks as the card widens. */
  .overview-cols {
    display: grid;
    grid-template-columns:
      minmax(130px, .75fr)   /* commercial   */
      minmax(150px, .9fr)    /* account team */
      minmax(180px, 1.1fr)   /* adoption     */
      minmax(170px, 1fr)     /* performance  */
      minmax(180px, 1.1fr);  /* engagement   */
    gap: 22px;
    padding: 6px 0;
    align-items: start;
  }
  .overview-col {
    display: flex;
    flex-direction: column;
    gap: 13px;
    min-width: 0;
  }
  /* Section heading above the three health-summary columns, so the Overview
     tab reads with the same structure as the PNG health summary. */
  .ov-head {
    font-size: 11px;
    font-weight: 700;
    letter-spacing: .6px;
    text-transform: uppercase;
    color: var(--text-1);
    padding-bottom: 5px;
    border-bottom: 2px solid var(--im-lime);
    margin-bottom: 2px;
  }
  /* Compact label:value pairs inside the health-summary columns. */
  .ov-pair {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    gap: 10px;
    font-size: 12.5px;
  }
  .ov-pair-k { color: var(--text-3); white-space: nowrap; }
  .ov-pair-v { color: var(--text-1); font-weight: 600; text-align: right; }
  .ov-pair-v.bad { color: #991B1B; }
  .ov-note {
    font-size: 12px;
    color: var(--text-2);
    line-height: 1.45;
  }
  /* Adoption rows carry the full picklist definition under the stoplight, so
     the criteria are readable without hovering for the tooltip. */
  .ov-adopt {
    display: flex;
    flex-direction: column;
    gap: 3px;
  }
  .ov-adopt-top {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    gap: 10px;
    font-size: 12.5px;
  }
  .ov-adopt-def {
    font-size: 10.5px;
    line-height: 1.4;
    color: var(--text-3);
  }
  @media (max-width: 1500px) {
    .overview-cols { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  }
  @media (max-width: 900px) {
    .overview-cols { grid-template-columns: 1fr; gap: 18px; }
  }
  .field-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    margin-bottom: 3px;
  }
  .field-value {
    font-size: 13.5px;
    color: var(--text-1);
    font-weight: 500;
  }
  .field-value.arr-value {
    font-size: 20px;
    font-weight: 800;
    color: var(--text-1);
    letter-spacing: -.5px;
  }
  .em-dash { color: var(--border); }

  /* ── Data table ───────────────────────────────────────────────────────── */
  .data-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 13px;
  }
  .data-table th {
    text-align: left;
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    padding: 7px 12px;
    border-bottom: 1.5px solid var(--border);
    background: var(--surface);
  }
  .data-table td {
    padding: 9px 12px;
    border-bottom: 1px solid var(--surface);
    color: var(--text-1);
    vertical-align: top;
  }
  .data-table tr:last-child td { border-bottom: none; }
  .data-table tr:hover td { background: var(--im-lime-lt); }
  .empty-msg {
    font-size: 13px;
    color: var(--text-3);
    font-style: italic;
    padding: 10px 2px;
  }

  /* ── Architecture badges ──────────────────────────────────────────────── */
  .arch-badge {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    padding: 5px 12px;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 600;
    margin: 3px;
  }

  /* ── Adoption stoplights ─────────────────────────────────────────────── */
  .stoplight-badge {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    font-size: 13px;
    font-weight: 600;
  }
  .stoplight-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    display: inline-block;
    flex-shrink: 0;
    border: 1px solid rgba(0,0,0,.15);
  }
  .stoplight-green  { background: #22C55E; }
  .stoplight-amber  { background: #F59E0B; }
  .stoplight-yellow { background: #F59E0B; }
  .stoplight-red    { background: #EF4444; }
  .stoplight-black  { background: #111111; }
  .arch-yes  { background: var(--im-lime); color: var(--im-black); }
  .arch-no   { background: var(--surface); color: var(--text-3); border: 1px solid var(--border); }
  .arch-warn { background: var(--red-lt);  color: #991B1B; border: 1px solid #FCA5A5; }

  /* ── Diff-date picker ────────────────────────────────────────────────────── */
  .diff-date-wrap {
    display: flex;
    align-items: center;
    gap: 7px;
    flex-shrink: 0;
  }
  .diff-date-label {
    font-size: 11.5px;
    color: rgba(255,255,255,.4);
    white-space: nowrap;
  }
  .diff-date-outer {
    position: relative;
    display: inline-flex;
    align-items: center;
  }
  .diff-date-select {
    appearance: none;
    -webkit-appearance: none;
    background: rgba(255,255,255,.08);
    color: var(--im-white);
    border: 1px solid rgba(255,255,255,.12);
    border-radius: var(--radius-sm);
    padding: 6px 28px 6px 10px;
    font-size: 12.5px;
    font-family: var(--font);
    cursor: pointer;
    outline: none;
    transition: border-color .15s, background .15s;
  }
  .diff-date-select:focus { border-color: var(--im-lime); background: rgba(255,255,255,.12); }
  .diff-date-select option { background: #1a1a1a; color: var(--im-white); }
  .diff-date-chevron {
    position: absolute;
    right: 8px;
    pointer-events: none;
    color: rgba(255,255,255,.35);
    font-size: 10px;
    line-height: 1;
  }

  /* ── Changes tab ─────────────────────────────────────────────────────────── */
  .changes-header {
    background: var(--im-white);
    border-bottom: 1px solid var(--border);
    padding: 16px 28px;
    display: flex;
    align-items: center;
    gap: 32px;
    flex-wrap: wrap;
  }
  .changes-date-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    margin-bottom: 3px;
  }
  .changes-date-val {
    font-size: 20px;
    font-weight: 800;
    color: var(--text-1);
    letter-spacing: -.4px;
  }
  .changes-body {
    flex: 1;
    overflow-y: auto;
    padding: 20px 28px 48px;
  }
  .change-section {
    background: var(--im-white);
    border: 1px solid var(--border);
    border-radius: var(--radius);
    margin-bottom: 10px;
    overflow: hidden;
  }
  .change-section-hdr {
    padding: 13px 18px;
    display: flex;
    align-items: center;
    gap: 8px;
    cursor: pointer;
    user-select: none;
    transition: background .12s;
  }
  .change-section-hdr:hover { background: var(--surface); }
  .change-section-title { font-size: 13.5px; font-weight: 600; flex: 1; }
  .change-section-chevron { font-size: 11px; color: var(--text-3); flex-shrink: 0; }
  .chg-badge {
    font-size: 10.5px;
    font-weight: 700;
    padding: 2px 9px;
    border-radius: 4px;
    white-space: nowrap;
  }
  .chg-added   { background: var(--im-lime-lt); color: #5A7A00; }
  .chg-removed { background: var(--red-lt);     color: #991B1B; }
  .chg-changed { background: var(--amber-lt);   color: #92400E; }
  .change-section-body { border-top: 1px solid var(--border); }
  .chg-group {
    padding: 14px 18px;
    border-bottom: 1px solid var(--surface-2);
  }
  .chg-group:last-child { border-bottom: none; }
  .chg-group-title { margin-bottom: 10px; }
  .chg-old  { color: var(--text-3); }
  .chg-new  { font-weight: 600; color: var(--text-1); }
  .chg-null { color: var(--border); font-style: italic; }

  /* ── Refresh button ──────────────────────────────────────────────────────── */
  .refresh-btn {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 7px 15px;
    background: var(--im-lime);
    color: var(--im-black);
    border: none;
    border-radius: var(--radius-sm);
    font-size: 12.5px;
    font-weight: 700;
    font-family: var(--font);
    cursor: pointer;
    white-space: nowrap;
    flex-shrink: 0;
    transition: background .14s, opacity .14s;
  }
  .refresh-btn:hover:not(:disabled) { background: var(--im-lime-dk); }
  .refresh-btn:disabled { opacity: .55; cursor: wait; }

  /* ── Modal ────────────────────────────────────────────────────────────────── */
  .modal-overlay {
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,.45);
    z-index: 999;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .modal-box {
    background: var(--im-white);
    border-radius: var(--radius);
    padding: 28px 32px;
    max-width: 440px;
    width: 90%;
    box-shadow: var(--shadow-lg);
  }
  .modal-title { font-size: 15px; font-weight: 700; margin-bottom: 10px; color: var(--text-1); }
  .modal-body  { font-size: 13.5px; color: var(--text-2); line-height: 1.55; margin-bottom: 4px; }
  .modal-code {
    display: block;
    background: var(--surface);
    border: 1px solid var(--border);
    border-radius: var(--radius-sm);
    padding: 10px 14px;
    font-family: "Menlo", "Consolas", monospace;
    font-size: 13px;
    color: var(--text-1);
    margin: 10px 0 4px;
    cursor: pointer;
    user-select: all;
    transition: background .12s;
  }
  .modal-code:hover { background: var(--im-lime-lt); }
  .modal-hint  { font-size: 11.5px; color: var(--text-3); margin-bottom: 16px; }
  .modal-close {
    padding: 8px 20px;
    background: var(--im-black);
    color: var(--im-lime);
    border: none;
    border-radius: var(--radius-sm);
    font-size: 13px;
    font-weight: 600;
    font-family: var(--font);
    cursor: pointer;
  }
  .modal-close:hover { opacity: .85; }

  /* ── No results ───────────────────────────────────────────────────────── */
  #no-results {
    display: none;
    text-align: center;
    padding: 80px 20px;
    color: var(--text-3);
    font-size: 16px;
    font-weight: 500;
  }

  /* ── Page-level tab nav ───────────────────────────────────────────────── */
  .page-tabs {
    background: var(--im-white);
    border-bottom: 1px solid var(--border);
    display: flex;
    align-items: stretch;
    padding: 0 24px;
    position: sticky;
    top: var(--topbar-h);
    height: var(--tabs-h);
    z-index: 90;
    box-shadow: 0 1px 0 var(--border);
  }
  .page-tab {
    padding: 0 20px;
    border: none;
    background: none;
    font-size: 13.5px;
    font-weight: 500;
    font-family: var(--font);
    color: var(--text-3);
    cursor: pointer;
    border-bottom: 3px solid transparent;
    display: flex;
    align-items: center;
    gap: 8px;
    transition: color .12s;
    white-space: nowrap;
  }
  .page-tab:hover { color: var(--text-1); }
  .page-tab.active {
    color: var(--text-1);
    border-bottom-color: var(--im-lime);
    font-weight: 600;
  }
  .page-tab-count {
    font-size: 11px;
    font-weight: 700;
    background: var(--surface-2);
    color: var(--text-3);
    padding: 1px 7px;
    border-radius: 10px;
    min-width: 24px;
    text-align: center;
  }
  .page-tab.active .page-tab-count {
    background: var(--im-lime);
    color: var(--im-black);
  }

  /* ── Page panels ──────────────────────────────────────────────────────── */
  .page { display: none; flex: 1; }
  .page.active { display: flex; }
  #page-accounts.active { flex-direction: row; }
  #page-renewals.active,
  #page-expansions.active,
  #page-changes.active,
  #page-consumption.active { flex-direction: column; }

  /* ── Consumption page ─────────────────────────────────────────────────── */
  .consumption-page { flex: 1; display: flex; flex-direction: column; min-height: 0; }
  .consumption-sub-tabs {
    background: var(--im-white); border-bottom: 1px solid var(--border);
    padding: 0 24px; display: flex; flex-shrink: 0;
  }
  .csub-btn {
    padding: 10px 18px; border: none; background: none; font-size: 13px;
    font-weight: 500; font-family: var(--font); color: var(--text-3);
    cursor: pointer; border-bottom: 2px solid transparent; margin-bottom: -1px;
    transition: color .12s; white-space: nowrap;
    display: flex; align-items: center; gap: 7px;
  }
  .csub-btn:hover { color: var(--text-1); }
  .csub-btn.active { color: var(--text-1); border-bottom-color: var(--im-lime); font-weight: 600; }
  .csub-count {
    font-size: 10.5px; font-weight: 700; background: var(--surface-2);
    color: var(--text-3); padding: 1px 6px; border-radius: 9px;
  }
  .csub-btn.active .csub-count { background: var(--im-lime); color: var(--im-black); }
  .consumption-panel { display: none; flex: 1; flex-direction: column; min-height: 0; }
  .consumption-panel.active { display: flex; }
  .con-stats {
    padding: 14px 24px; display: flex; gap: 32px; border-bottom: 1px solid var(--border);
    background: var(--surface); flex-wrap: wrap; flex-shrink: 0;
  }
  .pct-high { color: #166534; font-weight: 700; }
  .pct-med  { color: #92400E; font-weight: 700; }
  .pct-low  { color: #991B1B; font-weight: 700; }
  .pct-none { color: var(--text-3); }
  .flat-table tr.low-con td { background: #FEF3C7; }
  .flat-table tr.low-con:hover td { background: #FDE68A; }

  /* ── Flat table pages (Renewals / Expansions) ─────────────────────────── */
  .flat-page { flex: 1; display: flex; flex-direction: column; min-height: 0; }
  .flat-filters {
    background: var(--im-white);
    border-bottom: 1px solid var(--border);
    padding: 10px 24px;
    display: flex;
    align-items: center;
    gap: 6px;
    flex-wrap: wrap;
  }
  .flat-pill {
    padding: 5px 12px;
    border-radius: var(--radius-sm);
    border: 1px solid var(--border);
    background: transparent;
    font-size: 12.5px;
    font-family: var(--font);
    color: var(--text-2);
    cursor: pointer;
    transition: background .12s, color .12s, border-color .12s;
    white-space: nowrap;
  }
  .flat-pill:hover { background: var(--im-lime-lt); border-color: #C8E870; color: var(--text-1); }
  .flat-pill.active { background: var(--im-black); color: var(--im-lime); border-color: var(--im-black); font-weight: 600; }

  .flat-filters-csm {
    padding-top: 6px;
    border-top: 1px solid var(--surface-2);
  }

  .flat-stats {
    padding: 14px 24px;
    display: flex;
    gap: 32px;
    border-bottom: 1px solid var(--border);
    background: var(--surface);
    flex-wrap: wrap;
  }
  .stat-item { display: flex; flex-direction: column; gap: 2px; }
  .stat-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
  }
  .stat-value {
    font-size: 20px;
    font-weight: 800;
    color: var(--text-1);
    letter-spacing: -.5px;
  }

  .flat-table-wrap { flex: 1; overflow-y: auto; padding: 0 24px 40px; }
  .flat-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 13px;
    margin-top: 16px;
  }
  .flat-table th {
    text-align: left;
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    padding: 9px 12px;
    border-bottom: 1.5px solid var(--border);
    background: var(--im-white);
    position: sticky;
    top: 0;
    cursor: pointer;
    user-select: none;
    white-space: nowrap;
    transition: color .12s;
  }
  .flat-table th:hover { color: var(--text-1); }
  .flat-table th.sort-asc::after  { content: " \2191"; color: var(--im-lime-dk); }
  .flat-table th.sort-desc::after { content: " \2193"; color: var(--im-lime-dk); }
  .flat-table td {
    padding: 10px 12px;
    border-bottom: 1px solid var(--surface);
    color: var(--text-1);
    vertical-align: middle;
  }
  .flat-table tr:hover td { background: var(--im-lime-lt); }
  .flat-table tr:last-child td { border-bottom: none; }
  .flat-table .acct-link {
    font-weight: 600;
    color: var(--text-1);
    text-decoration: none;
    cursor: pointer;
  }
  .flat-table .acct-link:hover { text-decoration: underline; text-decoration-color: var(--im-lime); }
  .opp-link { color: var(--text-1); text-decoration: none; font-weight: 500; }
  .opp-link:hover { text-decoration: underline; text-decoration-color: var(--im-lime); color: var(--im-black); }
  .risk-high     { color: #991B1B; font-weight: 600; }
  .risk-moderate { color: #92400E; font-weight: 600; }
  .risk-low      { color: #166534; font-weight: 600; }
  .churn-q {
    display: inline-block;
    background: var(--amber-lt);
    color: #92400E;
    border: 1px solid #FCD34D;
    border-radius: 4px;
    font-size: 11px;
    font-weight: 700;
    padding: 2px 7px;
  }
</style>
</head>
<body>

<!-- TOP BAR -->
<div class="topbar">
  <a class="topbar-logo" href="https://www.immuta.com" target="_blank" rel="noopener">
    <svg width="114" height="20" viewBox="0 0 142 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M95.968 3.99802H99.8179V11.656C99.8179 16.8192 96.57 20.0561 92.1339 20.0561C87.6978 20.0561 84.5925 16.8192 84.5925 11.656V3.99802H88.4424V11.656C88.4424 14.7508 89.7733 16.5192 92.1339 16.5192C94.4946 16.5192 95.9838 14.7508 95.9838 11.656V3.99802H95.968ZM40.675 15.635L35.0507 3.99802H30.7571V19.7561H34.6071V11.7981L38.457 19.8982H42.7505L46.6004 11.7981V19.7561H50.4503V3.99802H46.1568L40.6909 15.635H40.675ZM77.4789 3.99802H73.1854L67.7195 15.4929L62.0951 3.99802H57.8016V19.7561H61.6515V11.7981L65.5014 19.8982H69.7949L73.6448 11.7981V19.7561H77.4947V3.99802H77.4789ZM120.509 3.99802H105.426V7.53491H111.051V19.7561H114.901V7.53491H120.525V3.99802H120.509ZM141.2 19.8982H137.065L136.036 17.3877H128.352L127.322 19.8982H123.329L130.126 4.14013H134.42L141.216 19.8982H141.2ZM134.689 14.3087L132.17 8.41913L129.651 14.3087H134.673H134.689ZM24.1188 0.161133V23.7351H0.464844V0.161133H24.1188ZM19.0965 3.99802H15.2466V19.7561H19.0965V3.99802Z" fill="white"/>
    </svg>
    <span class="topbar-section">Customer Accounts</span>
  </a>
  <div class="search-wrap">
    <span class="search-icon">&#128269;</span>
    <input type="text" id="search" placeholder="Search accounts..." autocomplete="off">
  </div>
  <div class="count-badge" id="count-badge"></div>
  <div class="diff-date-wrap">
    <span class="diff-date-label">Compare to</span>
    <div class="diff-date-outer">
      <select id="diff-date" class="diff-date-select"></select>
      <span class="diff-date-chevron">&#9660;</span>
    </div>
  </div>
  <!-- Drive file 1K5pNbz2TPrxGSvU8l2hbEjbMFfFShQ09, as the uc?export=download
       form so the button downloads directly rather than opening the Drive
       preview page. Swap to /file/d/<id>/view to land on the preview instead. -->
  <a class="refresh-btn" href="https://drive.google.com/uc?export=download&id=1K5pNbz2TPrxGSvU8l2hbEjbMFfFShQ09" target="_blank" rel="noopener" style="text-decoration:none">&#11123; Download Update</a>
  <button class="refresh-btn" id="refresh-btn" onclick="triggerRefresh()">&#8635; Refresh</button>
</div>

<!-- REFRESH MODAL -->
<div id="refresh-modal" class="modal-overlay" style="display:none" onclick="closeRefreshModal()">
  <div class="modal-box" onclick="event.stopPropagation()">
    <div class="modal-title">Start the refresh server first</div>
    <p class="modal-body">In a separate R session, run this from your project root:</p>
    <code class="modal-code" onclick="copyModalCmd(this)">source("scripts/serve.R")</code>
    <p class="modal-hint">Click the code to copy &bull; Leave that R session running while you use the dashboard</p>
    <p class="modal-body">Then click <strong>Refresh</strong> again.</p>
    <br>
    <button class="modal-close" onclick="closeRefreshModal()">Got it</button>
  </div>
</div>

<!-- PAGE TABS -->
<div class="page-tabs">
  <button class="page-tab active" data-page="accounts" onclick="switchPage(\'accounts\')">
    Accounts <span class="page-tab-count" id="tab-count-accounts">0</span>
  </button>
  <button class="page-tab" data-page="renewals" onclick="switchPage(\'renewals\')">
    Renewals <span class="page-tab-count" id="tab-count-renewals">0</span>
  </button>
  <button class="page-tab" data-page="expansions" onclick="switchPage(\'expansions\')">
    Expansions <span class="page-tab-count" id="tab-count-expansions">0</span>
  </button>
  <button class="page-tab" data-page="changes" onclick="switchPage(\'changes\')">
    Changes <span class="page-tab-count" id="tab-count-changes">0</span>
  </button>
  <button class="page-tab" data-page="consumption" onclick="switchPage(\'consumption\')">
    Consumption <span class="page-tab-count" id="tab-count-consumption">—</span>
  </button>
  <button class="page-tab" data-page="self_managed" onclick="switchPage(\'self_managed\')">
    Self-Managed <span class="page-tab-count" id="tab-count-self-managed">—</span>
  </button>
</div>

<!-- ACCOUNTS PAGE -->
<div id="page-accounts" class="page active">
  <div class="layout">
    <aside class="sidebar">
      <div class="sidebar-section">
        <div class="sidebar-label">Filter by Region</div>
        <div class="region-filters">
          <button class="pill active" data-region="all">All Regions</button>
          {{REGION_PILLS}}
        </div>
      </div>
      <div class="sidebar-section">
        <div class="sidebar-label">Filter by CSM</div>
        <div class="region-filters" id="csm-filters">
          <button class="pill active" data-csm="all">All CSMs</button>
          {{CSM_PILLS}}
        </div>
      </div>
      <div class="sidebar-section">
        <div class="sidebar-label">Jump to Letter</div>
        <div class="alpha-jump" id="alpha-jump"></div>
      </div>
    </aside>
    <main class="main" id="main-content">
      <div id="account-list"></div>
      <div id="no-results">No accounts match your search.</div>
    </main>
  </div>
</div>

<!-- RENEWALS PAGE -->
<div id="page-renewals" class="page">
  <div class="flat-page">
    <div class="flat-filters" id="renewal-filters"></div>
    <div class="flat-filters flat-filters-csm" id="renewal-csm-filters"></div>
    <div class="flat-stats" id="renewal-stats"></div>
    <div class="flat-table-wrap" id="renewal-table"></div>
  </div>
</div>

<!-- EXPANSIONS PAGE -->
<div id="page-expansions" class="page">
  <div class="flat-page">
    <div class="flat-filters" id="expansion-filters"></div>
    <div class="flat-filters flat-filters-csm" id="expansion-csm-filters"></div>
    <div class="flat-stats" id="expansion-stats"></div>
    <div class="flat-table-wrap" id="expansion-table"></div>
  </div>
</div>

<!-- CHANGES PAGE -->
<div id="page-changes" class="page">
  <div class="flat-page">
    <div class="changes-header" id="changes-header"></div>
    <div class="changes-body"   id="changes-content"></div>
  </div>
</div>

<!-- CONSUMPTION PAGE -->
<div id="page-consumption" class="page">
  <div class="consumption-page">
    <div class="consumption-sub-tabs">
      <button class="csub-btn active" data-csub="all" onclick="switchConSub(\'all\')">
        All Customers <span class="csub-count" id="csub-count-all">—</span>
      </button>
      <button class="csub-btn" data-csub="low" onclick="switchConSub(\'low\')">
        Low Consumption <span class="csub-count" id="csub-count-low">—</span>
      </button>
      <button class="csub-btn" data-csub="changes" onclick="switchConSub(\'changes\')">
        Changes <span class="csub-count" id="csub-count-changes">—</span>
      </button>
    </div>
    <div class="consumption-panel active" id="con-panel-all">
      <div class="con-stats" id="con-stats"></div>
      <div class="flat-table-wrap" id="con-table"></div>
    </div>
    <div class="consumption-panel" id="con-panel-low">
      <div class="con-stats" id="con-stats-low"></div>
      <div class="flat-table-wrap" id="con-table-low"></div>
    </div>
    <div class="consumption-panel" id="con-panel-changes">
      <div class="changes-header" id="con-changes-header"></div>
      <div class="changes-body"   id="con-changes-content"></div>
    </div>
  </div>
</div>

<!-- SELF-MANAGED PAGE -->
<div id="page-self_managed" class="page">
  <div class="flat-page">
    <div class="flat-filters" id="sm-support-filters">
      <button class="flat-pill active" data-sm-support="all" onclick="setSMSupport(\'all\')">All Support Statuses</button>
      <button class="flat-pill" data-sm-support="Supported" onclick="setSMSupport(\'Supported\')">Supported</button>
      <button class="flat-pill" data-sm-support="Unsupported" onclick="setSMSupport(\'Unsupported\')">Unsupported</button>
      <button class="flat-pill" data-sm-support="Unknown" onclick="setSMSupport(\'Unknown\')">Unknown</button>
    </div>
    <div class="flat-filters flat-filters-csm" id="sm-lifecycle-filters">
      <button class="flat-pill active" data-sm-lifecycle="all" onclick="setSMLifecycle(\'all\')">All Lifecycle Stages</button>
      {{SM_LIFECYCLE_PILLS}}
    </div>
    <div class="flat-stats" id="sm-stats"></div>
    <div class="flat-table-wrap" id="sm-table"></div>
  </div>
</div>

<script>
const ACCOUNTS          = {{JSON_DATA}};
const SNAP_DATES        = {{SNAP_DATES}};
const CHANGES_DATA      = {{CHANGES}};
const CONSUMPTION_DATA  = {{CONSUMPTION}};

// ── State ──────────────────────────────────────────────────────────────────
let activeRegion    = "all";
let activeCSM       = "all";
let searchTerm      = "";
let activePage      = "accounts";
let renewalSort         = { col: "end_date_raw", dir: 1 };
let expansionSort       = { col: "close_date_raw", dir: 1 };
let conSort             = { col: null, dir: 1 };
let conSubTab           = "all";
let smSort              = { col: null, dir: 1 };
let activeSMSupport     = "all";
let activeSMLifecycle   = "all";
const SM_COLS = [
  "Account Name", "Immuta Version", "Support Status", "Lifecycle Stage",
  "Registered Users", "Licensed Users", "Consumption Ratio",
  "Data Sources Count", "Active Policies (Total)"
];
const CON_COLS = [
  "Account Name", "Contract ARR Rollup", "Is SaaS Customer",
  "Customer on Unsupported Version", "Registered Users", "Licensed Users",
  "Consumption Ratio", "Data Sources Count", "Changes in Data Sources Count",
  "Active Policies (Total)", "Changes in Active Policies"
];

// ── Flat data derived from ACCOUNTS ────────────────────────────────────────
const ALL_RENEWALS   = [];
const ALL_EXPANSIONS = [];
ACCOUNTS.forEach(a => {
  a.renewals.forEach(r => ALL_RENEWALS.push(
    Object.assign({}, r, { account: a.name, region: a.region, csm: a.csm })
  ));
  a.expansions.forEach(e => ALL_EXPANSIONS.push(
    Object.assign({}, e, { account: a.name, region: a.region, csm: a.csm })
  ));
});

// Update global tab counts once
document.getElementById("tab-count-accounts").textContent   = ACCOUNTS.length;
document.getElementById("tab-count-renewals").textContent    = ALL_RENEWALS.length;
document.getElementById("tab-count-expansions").textContent  = ALL_EXPANSIONS.length;
document.getElementById("tab-count-changes").textContent     =
  (CHANGES_DATA && CHANGES_DATA.has_data && CHANGES_DATA.summary)
    ? CHANGES_DATA.summary.reduce((s, r) => s + (r.Total_Moves || 0), 0)
    : 0;

// ── Helpers ────────────────────────────────────────────────────────────────
function esc(s) {
  if (!s) return "";
  return String(s)
    .replace(/&/g,"&amp;").replace(/</g,"&lt;")
    .replace(/>/g,"&gt;").replace(/"/g,"&quot;");
}

function fmtBool(v) {
  if (v === true)  return \'<span class="arch-badge arch-yes">&#10003; Yes</span>\';
  if (v === false) return \'<span class="arch-badge arch-no">No</span>\';
  return \'<span class="arch-badge arch-no">—</span>\';
}

function fieldHtml(label, value, extraClass) {
  const v = (value === null || value === undefined || value === "" || value === "\\u2014")
    ? \'<span class="em-dash">—</span>\' : esc(value);
  return `<div class="field-item">
    <div class="field-label">${label}</div>
    <div class="field-value${extraClass ? \' \' + extraClass : \'\'}">${v}</div>
  </div>`;
}

function fieldHtmlRaw(label, innerHtml) {
  return `<div class="field-item">
    <div class="field-label">${label}</div>
    <div class="field-value">${innerHtml}</div>
  </div>`;
}

// Compact label:value row for the three health-summary columns. The stacked
// label-over-value of fieldHtml() is too tall once a column carries 6-8 rows.
function pairHtml(label, innerHtml, bad) {
  return `<div class="ov-pair">
    <span class="ov-pair-k">${label}</span>
    <span class="ov-pair-v${bad ? \' bad\' : \'\'}">${innerHtml}</span>
  </div>`;
}

// Adoption row: label + stoplight on the first line, then the definition text
// from the picklist underneath. The leading colour word is stripped because the
// stoplight already says it — "Black: Less than 25% consumption" renders as a
// black dot labelled Black, with "Less than 25% consumption" beneath.
function adoptionHtml(label, v) {
  const raw = (v === null || v === undefined) ? "" : String(v).trim();
  const blank = !raw || raw === "-" || raw === "\\u2014";
  const def = blank ? "" : raw.replace(/^(green|amber|yellow|red|black)\\b[\\s:\\u2013\\u2014-]*/i, "");
  return `<div class="ov-adopt">
    <div class="ov-adopt-top">
      <span class="ov-pair-k">${label}</span>
      <span class="ov-pair-v">${fmtStoplight(v)}</span>
    </div>
    ${def ? `<div class="ov-adopt-def">${esc(def)}</div>` : ""}
  </div>`;
}

// The same SaaS / SM: Supported / SM: Unsupported badge used in the card header
// and on the PNG health summary, so all three describe an account identically.
function deployTag(a) {
  if (a.is_saas) return \'<span class="tag tag-saas">SaaS</span>\';
  return a.unsupported
    ? \'<span class="tag tag-sm-unsup">SM: Unsupported</span>\'
    : \'<span class="tag tag-sm-sup">SM: Supported</span>\';
}

function fmtStoplight(v) {
  const raw = (v === null || v === undefined) ? "" : String(v).trim();
  if (!raw || raw === "-" || raw === "\\u2014") return \'<span class="em-dash">—</span>\';
  // The Salesforce picklist value is a full sentence, e.g. "Green (any 2): ..."
  // — pull the leading color word and keep the rest as a hover tooltip.
  // The picklists say AMBER, not "yellow" — omitting it rendered every amber
  // account as an em-dash. "yellow" stays as an alias.
  const m = raw.match(/^(green|amber|yellow|red|black)\\b/i);
  const labels = { green: "Green", amber: "Amber", yellow: "Yellow", red: "Red", black: "Black" };
  if (!m) return `<span class="em-dash" title="${esc(raw)}">—</span>`;
  const color = m[1].toLowerCase();
  return `<span class="stoplight-badge" title="${esc(raw)}"><span class="stoplight-dot stoplight-${color}"></span>${labels[color]}</span>`;
}

// ── Build cards ────────────────────────────────────────────────────────────
function buildCard(a) {
  const hasRenewals   = a.renewals.length > 0;
  const hasExpansions = a.expansions.length > 0;

  // Renewal rows
  let renewRows = "";
  if (hasRenewals) {
    renewRows = a.renewals.map(r => `
      <tr>
        <td>${r.url ? \'<a class="opp-link" href="\' + esc(r.url) + \'" target="_blank" rel="noopener">\' + esc(r.name) + \'</a>\' : esc(r.name)}</td>
        <td>${esc(r.amount)}</td>
        <td>${esc(r.end_date)}</td>
        <td><strong>${esc(r.churn_quarter)}</strong></td>
        <td>${esc(r.next_steps)}</td>
      </tr>`).join("");
  }

  // Expansion rows
  let expRows = "";
  if (hasExpansions) {
    expRows = a.expansions.map(e => `
      <tr>
        <td>${e.url ? \'<a class="opp-link" href="\' + esc(e.url) + \'" target="_blank" rel="noopener">\' + esc(e.name) + \'</a>\' : esc(e.name)}</td>
        <td>${esc(e.owner)}</td>
        <td>${esc(e.amount)}</td>
        <td>${esc(e.fiscal_period)}</td>
        <td>${esc(e.next_steps)}</td>
      </tr>`).join("");
  }

  // Products rows
  let prodRows = "";
  if (a.products.length > 0) {
    prodRows = a.products.map(p => `
      <tr><td>${esc(p.name)}</td><td>${esc(p.quantity)}</td></tr>`).join("");
  }

  // Header tags
  const regionTag = a.region && a.region !== "\\u2014"
    ? `<span class="tag tag-region">${esc(a.region)}</span>` : "";
  const saasTag = a.is_saas
    ? \'<span class="tag tag-saas">SaaS</span>\' : "";
  const smTag = !a.is_saas
    ? (a.unsupported
        ? \'<span class="tag tag-sm-unsup">SM: Unsupported</span>\'
        : \'<span class="tag tag-sm-sup">SM: Supported</span>\')
    : "";
  const churnTag = a.churn_summary
    ? `<span class="tag tag-churn">${esc(a.churn_summary)}</span>` : "";

  return `
<div class="card" id="acct-${esc(a.id)}" data-region="${esc(a.region)}" data-name="${esc(a.name.toLowerCase())}">
  <div class="card-header" onclick="toggleCard(this)">
    <div class="card-name">${esc(a.name)}</div>
    <div class="card-arr">${esc(a.arr)}</div>
    ${a.consumption_link}
    ${a.customer_central_url ? `<a class="cc-link" href="${esc(a.customer_central_url)}" target="_blank" rel="noopener" onclick="event.stopPropagation()">Customer Central &#8599;</a>` : \'\'}
    <div class="card-meta">
      ${regionTag}${saasTag}${smTag}${churnTag}
    </div>
    <span class="expand-icon">&#9660;</span>
  </div>
  <div class="card-body">
    <div class="tabs">
      <button class="tab-btn active" onclick="switchTab(this,\'overview-${esc(a.id)}\')">Overview</button>
      <button class="tab-btn" onclick="switchTab(this,\'arch-${esc(a.id)}\')">Architecture${a.products.length > 0 ? \' (\' + a.products.length + \')\' : \'\'}</button>
      <button class="tab-btn" onclick="switchTab(this,\'renew-${esc(a.id)}\')">Renewals${hasRenewals ? \' (\' + a.renewals.length + \')\' : \'\'}</button>
      <button class="tab-btn" onclick="switchTab(this,\'expand-${esc(a.id)}\')">Expansions${hasExpansions ? \' (\' + a.expansions.length + \')\' : \'\'}</button>
    </div>

    <!-- Overview tab -->
    <div class="tab-pane active" id="overview-${esc(a.id)}">
      <div class="overview-cols">
        <div class="overview-col">
          ${fieldHtml("Contract ARR", a.arr, "arr-value")}
          ${fieldHtml("Region", a.region)}
        </div>
        <div class="overview-col">
          ${fieldHtml("Customer Success Manager", a.csm)}
          ${fieldHtml("Account Owner", a.account_owner)}
          ${fieldHtml("Field CTO", a.field_cto)}
        </div>
        <div class="overview-col">
          <div class="ov-head">Adoption</div>
          ${adoptionHtml("PBAC", a.pbac_adoption)}
          ${adoptionHtml("Comply", a.comply_adoption)}
          ${adoptionHtml("Request", a.request_adoption)}
          ${adoptionHtml("Agentic", a.agentic_governance_adoption)}
          ${pairHtml("Registered", esc(a.registered_users))}
          ${pairHtml("Licensed", esc(a.licensed_users))}
          ${pairHtml("Ratio", `<span class="${pctClass(a.consumption_ratio_raw)}">${esc(a.consumption_ratio)}</span>`)}
          ${fieldHtml("Lifecycle Stage", a.lifecycle_stage)}
        </div>
        <div class="overview-col">
          <div class="ov-head">Performance</div>
          <div>${deployTag(a)}</div>
          ${pairHtml("Version", esc(a.immuta_version))}
          ${pairHtml("Incidents 90d", esc(a.inc_90d))}
          ${pairHtml("Incidents 360d", esc(a.inc_360d))}
          ${pairHtml("CustX 90d", esc(a.custx_90d), a.custx_90d_raw > 0)}
          ${pairHtml("CustX open", esc(a.custx_open), a.custx_open_raw > 0)}
        </div>
        <div class="overview-col">
          <div class="ov-head">Engagement</div>
          ${pairHtml("Last QBR", esc(a.last_qbr))}
          ${pairHtml("Next QBR", esc(a.next_qbr))}
          ${fieldHtmlRaw("QBR Note", `<span class="ov-note">${esc(a.qbr_note)}</span>`)}
        </div>
      </div>
    </div>

    <!-- Architecture tab -->
    <div class="tab-pane" id="arch-${esc(a.id)}">
      <div style="margin-bottom:12px">
        ${fmtBool(a.is_saas).replace("Yes","SaaS Customer").replace("No","Self-Hosted")}
        ${a.unsupported ? \'<span class="arch-badge arch-warn">&#9888; Unsupported Version</span>\' : \'\'}
      </div>
      ${a.products.length > 0 ? `
      <table class="data-table">
        <thead><tr><th>Product</th><th>Qty</th></tr></thead>
        <tbody>${prodRows}</tbody>
      </table>` : \'<p class="empty-msg">No active orders on record.</p>\'}
    </div>

    <!-- Renewals tab -->
    <div class="tab-pane" id="renew-${esc(a.id)}">
      ${hasRenewals ? `
      <table class="data-table">
        <thead><tr><th>Opportunity</th><th>Amount</th><th>End Date</th><th>Churn Quarter</th><th>Next Steps</th></tr></thead>
        <tbody>${renewRows}</tbody>
      </table>` : \'<p class="empty-msg">No open renewals.</p>\'}
    </div>

    <!-- Expansions tab -->
    <div class="tab-pane" id="expand-${esc(a.id)}">
      ${hasExpansions ? `
      <table class="data-table">
        <thead><tr><th>Opportunity</th><th>Owner</th><th>Amount</th><th>Fiscal Period</th><th>Next Steps</th></tr></thead>
        <tbody>${expRows}</tbody>
      </table>` : \'<p class="empty-msg">No open expansion opportunities.</p>\'}
    </div>
  </div>
</div>`;
}

// ── Page switching ─────────────────────────────────────────────────────────
function switchPage(name) {
  activePage = name;
  document.querySelectorAll(".page").forEach(p => p.classList.toggle("active", p.id === "page-" + name));
  document.querySelectorAll(".page-tab").forEach(b => b.classList.toggle("active", b.dataset.page === name));
  document.getElementById("search").placeholder =
    name === "accounts" ? "Search accounts..." : "Filter by account name...";
  render();
}

// ── Render dispatcher ──────────────────────────────────────────────────────
function render() {
  if      (activePage === "accounts")    renderAccounts();
  else if (activePage === "renewals")    renderRenewals();
  else if (activePage === "expansions")  renderExpansions();
  else if (activePage === "changes")     renderChanges();
  else if (activePage === "consumption") renderConsumption();
  else if (activePage === "self_managed") renderSelfManaged();
}

// ── Consumption ────────────────────────────────────────────────────────────
function switchConSub(name) {
  conSubTab = name;
  document.querySelectorAll(".csub-btn").forEach(b => b.classList.toggle("active", b.dataset.csub === name));
  document.querySelectorAll(".consumption-panel").forEach(p => p.classList.toggle("active", p.id === "con-panel-" + name));
  renderConsumption();
}

function sortCon(col) {
  conSort.dir = conSort.col === col ? -conSort.dir : 1;
  conSort.col = col;
  renderConsumption();
}

function pctClass(ratio) {
  if (ratio === null || ratio === undefined || isNaN(ratio)) return "pct-none";
  if (ratio >= 0.5)  return "pct-high";
  if (ratio >= 0.25) return "pct-med";
  return "pct-low";
}

function renderConsumption() {
  const rows    = (CONSUMPTION_DATA && CONSUMPTION_DATA.rows) || [];
  const lowRows = rows.filter(r => r["Low Consumption"] === true);
  const chgRows = (CONSUMPTION_DATA && CONSUMPTION_DATA.changes && CONSUMPTION_DATA.changes.field_changes) || [];

  document.getElementById("tab-count-consumption").textContent = rows.length;
  document.getElementById("csub-count-all").textContent     = rows.length;
  document.getElementById("csub-count-low").textContent     = lowRows.length;
  document.getElementById("csub-count-changes").textContent = chgRows.length;

  if      (conSubTab === "all")     renderConTable(rows,    "con-stats",     "con-table",     "No consumption data — run op_list_consumption() and regenerate the report.");
  else if (conSubTab === "low")     renderConTable(lowRows, "con-stats-low", "con-table-low", "No customers currently flagged as low consumption.");
  else if (conSubTab === "changes") renderConsumptionChanges(chgRows);
}

function renderConTable(rows, statsElId, tableElId, emptyMsg) {
  const statsEl = document.getElementById(statsElId);
  const tableEl = document.getElementById(tableElId);

  if (!rows.length) {
    if (statsEl) statsEl.innerHTML = "";
    if (tableEl) tableEl.innerHTML = `<p class="empty-msg" style="padding:40px">${emptyMsg}</p>`;
    return;
  }

  // Stats strip
  if (statsEl) {
    const totalArr  = rows.reduce((s, r) => s + (parseFloat(r["Contract ARR Rollup"]) || 0), 0);
    const saasCount = rows.filter(r => r["Is SaaS Customer"] === true || r["Is SaaS Customer"] === "TRUE").length;
    const lowCount  = rows.filter(r => r["Low Consumption"] === true).length;
    statsEl.innerHTML =
      `<div class="stat-item"><div class="stat-label">Customers</div><div class="stat-value">${rows.length}</div></div>` +
      `<div class="stat-item"><div class="stat-label">Total ARR</div><div class="stat-value">$${Math.round(totalArr / 1e6 * 10) / 10}M</div></div>` +
      `<div class="stat-item"><div class="stat-label">SaaS</div><div class="stat-value">${saasCount}</div></div>` +
      `<div class="stat-item"><div class="stat-label">Low Consumption</div><div class="stat-value">${lowCount}</div></div>`;
  }

  // Fixed column set (see CON_COLS)
  const cols = CON_COLS;

  // Sort
  const sorted = conSort.col ? rows.slice().sort((a, b) => {
    const av = parseFloat(a[conSort.col]), bv = parseFloat(b[conSort.col]);
    if (!isNaN(av) && !isNaN(bv)) return (av - bv) * conSort.dir;
    return String(a[conSort.col] || "").localeCompare(String(b[conSort.col] || "")) * conSort.dir;
  }) : rows;

  const hdrs = cols.map(c => {
    const arrow = conSort.col === c ? (conSort.dir === 1 ? " ↑" : " ↓") : "";
    return `<th onclick="sortCon(\'${c}\')" style="cursor:pointer">${esc(c)}${arrow}</th>`;
  }).join("");

  const brs = sorted.map(r => {
    const rowCls = r["Low Consumption"] === true ? \' class="low-con"\' : "";
    return `<tr${rowCls}>` + cols.map(c => {
      const v = r[c];
      if (c === "Consumption Ratio") {
        const pct = parseFloat(v);
        if (v === null || v === undefined || v === "NA" || isNaN(pct)) return `<td><span class="em-dash">—</span></td>`;
        return `<td class="${pctClass(pct)}">${Math.round(pct * 1000) / 10}%</td>`;
      }
      if (c === "Changes in Data Sources Count" || c === "Changes in Active Policies") {
        const d = parseFloat(v);
        if (v === null || v === undefined || v === "NA" || isNaN(d)) return `<td><span class="em-dash">—</span></td>`;
        const cls  = d > 0 ? "pct-high" : d < 0 ? "pct-low" : "pct-none";
        const sign = d > 0 ? "+" : "";
        return `<td class="${cls}">${sign}${d.toLocaleString("en-US", {maximumFractionDigits: 2})}</td>`;
      }
      if (v === null || v === undefined || v === "NA") return `<td><span class="em-dash">—</span></td>`;
      if (c === "Is SaaS Customer")
        return `<td>${v === true || v === "TRUE" ? \'<span class="tag tag-saas">SaaS</span>\' : \'<span class="tag tag-region">Self-Mgd</span>\'}</td>`;
      if (c === "Customer on Unsupported Version")
        return `<td>${v === true || v === "TRUE" ? \'<span class="tag tag-churn">Yes</span>\' : \'\'}</td>`;
      const n = parseFloat(v);
      if (!isNaN(n) && typeof v !== "boolean" && String(v).trim() !== "")
        return `<td style="text-align:right">${n.toLocaleString("en-US", {maximumFractionDigits: 2})}</td>`;
      return `<td>${esc(String(v))}</td>`;
    }).join("") + "</tr>";
  }).join("");

  tableEl.innerHTML = `<table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${brs}</tbody></table>`;
}

function renderConsumptionChanges(rows) {
  const hdr      = document.getElementById("con-changes-header");
  const body     = document.getElementById("con-changes-content");
  const diffDate = (CONSUMPTION_DATA && CONSUMPTION_DATA.diff_date) || "";

  hdr.innerHTML = diffDate
    ? `<div><div class="changes-date-label">Comparing to</div><div class="changes-date-val">${esc(diffDate)}</div></div>`
    : "";

  if (!rows || rows.length === 0) {
    body.innerHTML = `<p class="empty-msg" style="padding:40px">No consumption drops detected${diffDate ? \' since \' + esc(diffDate) : \'\'}.</p>`;
    return;
  }

  const na = `<td class="chg-null">—</td>`;
  const trs = rows.map(r => {
    const acct = r["Account Name"] || "";
    return `<tr>
      <td><span class="acct-link" onclick="goToAccount(\'${esc(acct)}\')">${esc(acct)}</span></td>
      ${r.user_drop ? `<td>${chgArrow(r["Registered Users.old"], r["Registered Users.new"])} <span class="chg-null">(&#8722;${r.user_drop_pct}%)</span></td>` : na}
      ${r.ds_drop   ? `<td>${chgArrow(r["Data Sources Count.old"], r["Data Sources Count.new"])}</td>` : na}
      ${r.pol_drop  ? `<td>${chgArrow(r["Active Policies (Total).old"], r["Active Policies (Total).new"])}</td>` : na}
    </tr>`;
  }).join("");

  body.innerHTML = `
    <div class="chg-group">
      <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Changed (${rows.length})</span></div>
      <table class="flat-table">
        <thead><tr><th>Account</th><th>Registered Users</th><th>Data Sources</th><th>Active Policies</th></tr></thead>
        <tbody>${trs}</tbody>
      </table>
    </div>`;
}

// ── Self-Managed ─────────────────────────────────────────────────────────────
function smVersionStatus(r) {
  const v = r["Customer on Unsupported Version"];
  if (v === null || v === undefined || v === "" || v === "NA") return "Unknown";
  return (v === true || v === "TRUE" || v === "Yes" || v === "yes") ? "Unsupported" : "Supported";
}

function smSupportBadge(status) {
  const cls = status === "Supported" ? "arch-yes" : status === "Unsupported" ? "arch-warn" : "arch-no";
  return `<span class="arch-badge ${cls}">${esc(status)}</span>`;
}

function setSMSupport(val) {
  activeSMSupport = val;
  document.querySelectorAll(\'.flat-pill[data-sm-support]\').forEach(b =>
    b.classList.toggle("active", b.dataset.smSupport === val));
  renderSelfManaged();
}

function setSMLifecycle(val) {
  activeSMLifecycle = val;
  document.querySelectorAll(\'.flat-pill[data-sm-lifecycle]\').forEach(b =>
    b.classList.toggle("active", b.dataset.smLifecycle === val));
  renderSelfManaged();
}

function sortSM(col) {
  smSort.dir = smSort.col === col ? -smSort.dir : 1;
  smSort.col = col;
  renderSelfManaged();
}

function renderSelfManaged() {
  const allRows = (CONSUMPTION_DATA && CONSUMPTION_DATA.rows) || [];
  const selfManagedRows = allRows.filter(r =>
    !(r["Is SaaS Customer"] === true || r["Is SaaS Customer"] === "TRUE")
  );

  document.getElementById("tab-count-self-managed").textContent = selfManagedRows.length;

  let rows = selfManagedRows;
  if (activeSMSupport !== "all") rows = rows.filter(r => smVersionStatus(r) === activeSMSupport);
  if (activeSMLifecycle !== "all") rows = rows.filter(r => (r["Lifecycle Stage"] || "") === activeSMLifecycle);

  const statsEl = document.getElementById("sm-stats");
  const tableEl = document.getElementById("sm-table");

  if (!selfManagedRows.length) {
    statsEl.innerHTML = "";
    tableEl.innerHTML = \'<p class="empty-msg" style="padding:40px">No self-managed accounts found, or consumption data hasn\\\'t been refreshed yet.</p>\';
    return;
  }

  const unsupportedCount = selfManagedRows.filter(r => smVersionStatus(r) === "Unsupported").length;
  const supportedCount   = selfManagedRows.filter(r => smVersionStatus(r) === "Supported").length;
  const lowConCount      = selfManagedRows.filter(r => r["Low Consumption"] === true).length;
  statsEl.innerHTML =
    `<div class="stat-item"><div class="stat-label">Self-Managed</div><div class="stat-value">${selfManagedRows.length}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Supported</div><div class="stat-value">${supportedCount}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Unsupported</div><div class="stat-value">${unsupportedCount}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Low Consumption</div><div class="stat-value">${lowConCount}</div></div>`;

  if (!rows.length) {
    tableEl.innerHTML = \'<p class="empty-msg" style="padding:40px">No self-managed accounts match the current filters.</p>\';
    return;
  }

  const cols = SM_COLS;
  const sorted = smSort.col ? rows.slice().sort((a, b) => {
    let av, bv;
    if (smSort.col === "Support Status") { av = smVersionStatus(a); bv = smVersionStatus(b); }
    else { av = a[smSort.col]; bv = b[smSort.col]; }
    const anum = parseFloat(av), bnum = parseFloat(bv);
    if (!isNaN(anum) && !isNaN(bnum) && String(av).trim() !== "" && String(bv).trim() !== "") {
      return (anum - bnum) * smSort.dir;
    }
    return String(av || "").localeCompare(String(bv || "")) * smSort.dir;
  }) : rows;

  const hdrs = cols.map(c => {
    const arrow = smSort.col === c ? (smSort.dir === 1 ? " \\u2191" : " \\u2193") : "";
    return `<th onclick="sortSM(\'${c}\')" style="cursor:pointer">${esc(c)}${arrow}</th>`;
  }).join("");

  const brs = sorted.map(r => {
    const rowCls = r["Low Consumption"] === true ? \' class="low-con"\' : "";
    return `<tr${rowCls}>` + cols.map(c => {
      if (c === "Support Status") return `<td>${smSupportBadge(smVersionStatus(r))}</td>`;
      const v = r[c];
      if (c === "Consumption Ratio") {
        const pct = parseFloat(v);
        if (v === null || v === undefined || v === "NA" || isNaN(pct)) return `<td><span class="em-dash">—</span></td>`;
        return `<td class="${pctClass(pct)}">${Math.round(pct * 1000) / 10}%</td>`;
      }
      if (v === null || v === undefined || v === "" || v === "NA") return `<td><span class="em-dash">—</span></td>`;
      const n = parseFloat(v);
      if (!isNaN(n) && typeof v !== "boolean" && String(v).trim() !== "" && c !== "Immuta Version")
        return `<td style="text-align:right">${n.toLocaleString("en-US", {maximumFractionDigits: 2})}</td>`;
      return `<td>${esc(String(v))}</td>`;
    }).join("") + "</tr>";
  }).join("");

  tableEl.innerHTML = `<table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${brs}</tbody></table>`;
}

// ── Accounts render ────────────────────────────────────────────────────────
function renderAccounts() {
  const q      = searchTerm.toLowerCase();
  const region = activeRegion;
  const visible = ACCOUNTS.filter(a =>
    (region === "all" || a.region === region) &&
    (activeCSM === "all" || a.csm === activeCSM) &&
    (!q || a.name.toLowerCase().includes(q))
  );
  const groups = {};
  visible.forEach(a => {
    const letter = (a.name[0] || "#").toUpperCase();
    if (!groups[letter]) groups[letter] = [];
    groups[letter].push(a);
  });
  const letters = Object.keys(groups).sort();
  let html = "";
  letters.forEach(letter => {
    html += `<div class="letter-group" id="letter-${letter}"><div class="letter-heading">${letter}</div>`;
    groups[letter].forEach(a => { html += buildCard(a); });
    html += "</div>";
  });
  document.getElementById("account-list").innerHTML = html;
  document.getElementById("no-results").style.display = visible.length === 0 ? "block" : "none";
  document.getElementById("count-badge").textContent = visible.length + " account" + (visible.length !== 1 ? "s" : "");
  document.getElementById("tab-count-accounts").textContent = visible.length;
  buildAlphaJump(letters);
}

// ── Renewals render ────────────────────────────────────────────────────────
function renderRenewals() {
  const q = searchTerm.toLowerCase();
  const region = activeRegion;

  let rows = ALL_RENEWALS.filter(r =>
    (region === "all" || r.region === region) &&
    (activeCSM === "all" || r.csm === activeCSM) &&
    (!q || r.account.toLowerCase().includes(q))
  );

  // Sort
  rows = rows.slice().sort((a, b) => {
    let av = a[renewalSort.col] || "", bv = b[renewalSort.col] || "";
    if (renewalSort.col === "amount_raw") { av = +av || 0; bv = +bv || 0; }
    return renewalSort.dir * (av < bv ? -1 : av > bv ? 1 : 0);
  });

  // Stats
  const totalArr = rows.reduce((s, r) => s + (+r.amount_raw || 0), 0);
  const acctSet  = new Set(rows.map(r => r.account));
  document.getElementById("renewal-stats").innerHTML = `
    <div class="stat-item"><div class="stat-label">Open Renewals</div><div class="stat-value">${rows.length}</div></div>
    <div class="stat-item"><div class="stat-label">Total ARR</div><div class="stat-value">${fmtUsd(totalArr)}</div></div>
    <div class="stat-item"><div class="stat-label">Accounts</div><div class="stat-value">${acctSet.size}</div></div>`;
  document.getElementById("tab-count-renewals").textContent = rows.length;
  document.getElementById("count-badge").textContent = rows.length + " renewal" + (rows.length !== 1 ? "s" : "");

  // Region + CSM pills
  buildFlatPills("renewal-filters", "renewal");
  buildFlatCSMPills("renewal-csm-filters");

  // Table
  const cols = [
    { key: "account",       label: "Account" },
    { key: "name",          label: "Opportunity" },
    { key: "amount_raw",    label: "Amount" },
    { key: "end_date_raw",  label: "End Date" },
    { key: "churn_quarter", label: "Churn Quarter" },
    { key: "risk",          label: "Risk" },
    { key: "next_steps",    label: "Next Steps" },
    { key: "csm",           label: "CSM" },
    { key: "region",        label: "Region" }
  ];
  const headers = cols.map(c => {
    const cls = renewalSort.col === c.key ? (renewalSort.dir === 1 ? "sort-asc" : "sort-desc") : "";
    return `<th class="${cls}" onclick="sortRenewals(\'${c.key}\')">${c.label}</th>`;
  }).join("");

  const bodyRows = rows.map(r => {
    const riskCls = r.risk.toLowerCase() === "high" ? "risk-high" :
                    r.risk.toLowerCase() === "moderate" ? "risk-moderate" :
                    r.risk.toLowerCase() === "low" ? "risk-low" : "";
    const cq = r.churn_quarter && r.churn_quarter !== "—"
      ? `<span class="churn-q">${esc(r.churn_quarter)}</span>` : esc(r.churn_quarter);
    return `<tr>
      <td><span class="acct-link" onclick="goToAccount(\'${esc(r.account)}\')">${esc(r.account)}</span></td>
      <td>${r.url ? \'<a class="opp-link" href="\' + esc(r.url) + \'" target="_blank" rel="noopener">\' + esc(r.name) + \'</a>\' : esc(r.name)}</td>
      <td>${esc(r.amount)}</td>
      <td>${esc(r.end_date)}</td>
      <td>${cq}</td>
      <td class="${riskCls}">${esc(r.risk)}</td>
      <td>${esc(r.next_steps)}</td>
      <td>${esc(r.csm)}</td>
      <td>${esc(r.region)}</td>
    </tr>`;
  }).join("");

  document.getElementById("renewal-table").innerHTML = rows.length === 0
    ? \'<p class="empty-msg" style="padding:24px">No renewals match the current filter.</p>\'
    : `<table class="flat-table"><thead><tr>${headers}</tr></thead><tbody>${bodyRows}</tbody></table>`;
}

// ── Expansions render ──────────────────────────────────────────────────────
function renderExpansions() {
  const q = searchTerm.toLowerCase();
  const region = activeRegion;

  let rows = ALL_EXPANSIONS.filter(e =>
    (region === "all" || e.region === region) &&
    (activeCSM === "all" || e.csm === activeCSM) &&
    (!q || e.account.toLowerCase().includes(q))
  );

  rows = rows.slice().sort((a, b) => {
    let av = a[expansionSort.col] || "", bv = b[expansionSort.col] || "";
    if (expansionSort.col === "amount_raw") { av = +av || 0; bv = +bv || 0; }
    return expansionSort.dir * (av < bv ? -1 : av > bv ? 1 : 0);
  });

  const totalArr = rows.reduce((s, e) => s + (+e.amount_raw || 0), 0);
  const acctSet  = new Set(rows.map(e => e.account));
  document.getElementById("expansion-stats").innerHTML = `
    <div class="stat-item"><div class="stat-label">Open Expansions</div><div class="stat-value">${rows.length}</div></div>
    <div class="stat-item"><div class="stat-label">Pipeline Value</div><div class="stat-value">${fmtUsd(totalArr)}</div></div>
    <div class="stat-item"><div class="stat-label">Accounts</div><div class="stat-value">${acctSet.size}</div></div>`;
  document.getElementById("tab-count-expansions").textContent = rows.length;
  document.getElementById("count-badge").textContent = rows.length + " expansion" + (rows.length !== 1 ? "s" : "");

  buildFlatPills("expansion-filters", "expansion");
  buildFlatCSMPills("expansion-csm-filters");

  const cols = [
    { key: "account",        label: "Account" },
    { key: "name",           label: "Opportunity" },
    { key: "owner",          label: "Owner" },
    { key: "amount_raw",     label: "Amount" },
    { key: "fiscal_period",  label: "Fiscal Period" },
    { key: "close_date_raw", label: "Close Date" },
    { key: "next_steps",     label: "Next Steps" },
    { key: "region",         label: "Region" }
  ];
  const headers = cols.map(c => {
    const cls = expansionSort.col === c.key ? (expansionSort.dir === 1 ? "sort-asc" : "sort-desc") : "";
    return `<th class="${cls}" onclick="sortExpansions(\'${c.key}\')">${c.label}</th>`;
  }).join("");

  const bodyRows = rows.map(e => `<tr>
    <td><span class="acct-link" onclick="goToAccount(\'${esc(e.account)}\')">${esc(e.account)}</span></td>
    <td>${e.url ? \'<a class="opp-link" href="\' + esc(e.url) + \'" target="_blank" rel="noopener">\' + esc(e.name) + \'</a>\' : esc(e.name)}</td>
    <td>${esc(e.owner)}</td>
    <td>${esc(e.amount)}</td>
    <td>${esc(e.fiscal_period)}</td>
    <td>${esc(e.close_date)}</td>
    <td>${esc(e.next_steps)}</td>
    <td>${esc(e.region)}</td>
  </tr>`).join("");

  document.getElementById("expansion-table").innerHTML = rows.length === 0
    ? \'<p class="empty-msg" style="padding:24px">No expansions match the current filter.</p>\'
    : `<table class="flat-table"><thead><tr>${headers}</tr></thead><tbody>${bodyRows}</tbody></table>`;
}

// ── Sort handlers ──────────────────────────────────────────────────────────
function sortRenewals(col) {
  renewalSort.dir = renewalSort.col === col ? -renewalSort.dir : 1;
  renewalSort.col = col;
  renderRenewals();
}
function sortExpansions(col) {
  expansionSort.dir = expansionSort.col === col ? -expansionSort.dir : 1;
  expansionSort.col = col;
  renderExpansions();
}

// ── Navigate from table row to account card ────────────────────────────────
function goToAccount(name) {
  switchPage("accounts");
  searchTerm = name;
  document.getElementById("search").value = name;
  render();
  setTimeout(() => {
    const cards = document.querySelectorAll(".card");
    if (cards.length === 1) { cards[0].classList.add("open"); cards[0].scrollIntoView({ behavior: "smooth", block: "center" }); }
  }, 50);
}

// ── Region pills for flat pages ────────────────────────────────────────────
function buildFlatPills(containerId, pageKey) {
  const allRegions = [...new Set(ACCOUNTS.map(a => a.region).filter(Boolean))].sort();
  const container  = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = [
    `<button class="flat-pill${activeRegion === "all" ? " active" : ""}" onclick="setRegion(\'all\')">All Regions</button>`
  ].concat(allRegions.map(r =>
    `<button class="flat-pill${activeRegion === r ? " active" : ""}" onclick="setRegion(\'${esc(r)}\')">${esc(r)}</button>`
  )).join("");
}

function buildFlatCSMPills(containerId) {
  const allCSMs = [...new Set(ACCOUNTS.map(a => a.csm).filter(Boolean))].sort();
  const container = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = [
    `<button class="flat-pill${activeCSM === "all" ? " active" : ""}" onclick="setCSM(\'all\')">All CSMs</button>`
  ].concat(allCSMs.map(c =>
    `<button class="flat-pill${activeCSM === c ? " active" : ""}" onclick="setCSM(\'${esc(c)}\')">${esc(c)}</button>`
  )).join("");
}

function setRegion(r) {
  activeRegion = r;
  document.querySelectorAll("[data-region]").forEach(b => b.classList.toggle("active", b.dataset.region === r));
  render();
}

function setCSM(c) {
  activeCSM = c;
  document.querySelectorAll("[data-csm]").forEach(b => b.classList.toggle("active", b.dataset.csm === c));
  render();
}

// ── Alpha jump bar ─────────────────────────────────────────────────────────
function buildAlphaJump(visibleLetters) {
  const container = document.getElementById("alpha-jump");
  const all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".split("");
  container.innerHTML = all.map(l =>
    visibleLetters.includes(l)
      ? `<a href="#letter-${l}" class="has-accounts">${l}</a>`
      : `<a class="disabled">${l}</a>`
  ).join("");
}

// ── Card toggle ────────────────────────────────────────────────────────────
function toggleCard(header) { header.parentElement.classList.toggle("open"); }

// ── Card tab switch ────────────────────────────────────────────────────────
function switchTab(btn, paneId) {
  btn.closest(".tabs").querySelectorAll(".tab-btn").forEach(b => b.classList.remove("active"));
  btn.classList.add("active");
  btn.closest(".card-body").querySelectorAll(".tab-pane").forEach(p => p.classList.remove("active"));
  document.getElementById(paneId).classList.add("active");
}

// ── USD formatter ──────────────────────────────────────────────────────────
function fmtUsd(n) {
  if (!n || isNaN(n)) return "—";
  return "$" + Number(n).toLocaleString("en-US", { maximumFractionDigits: 0 });
}

// ── Sidebar pill listeners ─────────────────────────────────────────────────
document.querySelectorAll("[data-region]").forEach(btn => {
  btn.addEventListener("click", () => { setRegion(btn.dataset.region); });
});
document.querySelectorAll("[data-csm]").forEach(btn => {
  btn.addEventListener("click", () => { setCSM(btn.dataset.csm); });
});

document.getElementById("search").addEventListener("input", e => {
  searchTerm = e.target.value;
  render();
});

// ── Changes rendering ──────────────────────────────────────────────────────

function renderChanges() {
  const hdr  = document.getElementById("changes-header");
  const body = document.getElementById("changes-content");

  if (!CHANGES_DATA || !CHANGES_DATA.has_data) {
    hdr.innerHTML  = "";
    body.innerHTML = `<p class="empty-msg" style="padding:40px">No changes data — run enrich_list_object() to generate a diff.</p>`;
    return;
  }

  const sum = CHANGES_DATA.summary || [];
  hdr.innerHTML = `
    <div>
      <div class="changes-date-label">Comparing to</div>
      <div class="changes-date-val">${esc(CHANGES_DATA.diff_date)}</div>
    </div>
    <div style="width:1px;height:36px;background:var(--border);flex-shrink:0"></div>
    ${sum.map(r => `
      <div class="stat-item">
        <div class="stat-label">${esc(r.Component || "")}</div>
        <div class="stat-value" style="font-size:16px">${r.Total_Moves || 0}</div>
      </div>`).join("")}`;

  body.innerHTML = [
    buildChangeSection("accounts",      "Accounts",        CHANGES_DATA.accounts,      renderAccountChanges),
    buildChangeSection("renewals",      "Renewals",        CHANGES_DATA.renewals,      renderRenewalChanges),
    buildChangeSection("expansions",    "Expansions",      CHANGES_DATA.expansions,    renderExpansionChanges),
    buildChangeSection("csm_table",     "CSM Assignments", CHANGES_DATA.csm_table,     renderCSMChanges),
    buildChangeSection("saas_pipeline", "SaaS Pipeline",   CHANGES_DATA.saas_pipeline, renderSaaSChanges)
  ].join("");
}

function buildChangeSection(key, label, data, tableFn) {
  if (!data) return "";
  const nAdded   = (data.added   || []).length;
  const nRemoved = (data.removed || []).length;
  const nChanged = (data.field_changes || data.reassignments || []).length;
  const total    = nAdded + nRemoved + nChanged;

  const badges = [
    nAdded   > 0 ? `<span class="chg-badge chg-added">+${nAdded}</span>`       : "",
    nRemoved > 0 ? `<span class="chg-badge chg-removed">&#8722;${nRemoved}</span>` : "",
    nChanged > 0 ? `<span class="chg-badge chg-changed">&#126;${nChanged}</span>`  : ""
  ].join(" ");

  const bodyContent = total === 0
    ? `<p class="empty-msg" style="padding:14px 18px">No changes detected.</p>`
    : tableFn(data);

  return `
    <div class="change-section">
      <div class="change-section-hdr" onclick="toggleChgSection(\'${key}\')">
        <span class="change-section-title">${esc(label)}</span>
        ${badges}
        <span class="change-section-chevron" id="chsec-chev-${key}">&#9660;</span>
      </div>
      <div class="change-section-body" id="chsec-body-${key}">${bodyContent}</div>
    </div>`;
}

function toggleChgSection(key) {
  const body = document.getElementById("chsec-body-" + key);
  const chev = document.getElementById("chsec-chev-" + key);
  const isOpen = body.style.display !== "none";
  body.style.display = isOpen ? "none" : "";
  chev.innerHTML     = isOpen ? "&#9654;" : "&#9660;";
}

// chgArrow: renders old → new or just the value if unchanged
function chgArrow(oldVal, newVal) {
  const o = oldVal == null ? "" : String(oldVal);
  const n = newVal == null ? "" : String(newVal);
  if (!o && !n) return `<span class="chg-null">—</span>`;
  if (o === n)  return esc(o);
  return `<span class="chg-old">${esc(o) || "<em>empty</em>"}</span> &#8594; <span class="chg-new">${esc(n) || "<em>empty</em>"}</span>`;
}

// Generic simple table for added / removed rows
function buildSimpleTable(groupLabel, rows, cols, badgeCls) {
  if (!rows || rows.length === 0) return "";
  const hdrs = cols.map(c => `<th>${c.label}</th>`).join("");
  const body = rows.map(r =>
    `<tr>${cols.map(c => `<td>${esc(String(r[c.key] || ""))}</td>`).join("")}</tr>`
  ).join("");
  return `
    <div class="chg-group">
      <div class="chg-group-title"><span class="chg-badge ${badgeCls}">${groupLabel} (${rows.length})</span></div>
      <table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${body}</tbody></table>
    </div>`;
}

function renderAccountChanges(data) {
  const cols = [
    { key: "Account Name",             label: "Account" },
    { key: "Region",                   label: "Region" },
    { key: "Contract ARR Rollup",      label: "ARR" },
    { key: "Customer Success Manager", label: "CSM" }
  ];
  let html = "";
  html += buildSimpleTable("+ Added",         data.added,   cols, "chg-added");
  html += buildSimpleTable("&#8722; Removed", data.removed, cols, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const watchedPairs = [
      ["Contract ARR Rollup",      "ARR"],
      ["Customer Success Manager", "CSM"],
      ["Account Owner",            "Owner"],
      ["Field CTO",                "Field CTO"],
      ["Region",                   "Region"],
      ["Adoption Phase",           "Adoption"]
    ].filter(([f]) => fc.some(r => {
      const n = r[f + ".new"], o = r[f + ".old"];
      return (n || o) && n !== o;
    }));

    const hdrs = `<th>Account</th>${watchedPairs.map(([, l]) => `<th>${l}</th>`).join("")}`;
    const rows = fc.map(r => {
      const nm = r["Account Name"] || "";
      const cells = watchedPairs.map(([f]) =>
        `<td>${chgArrow(r[f + ".old"], r[f + ".new"])}</td>`
      ).join("");
      return `<tr>
        <td><span class="acct-link" onclick="goToAccount(\'${esc(nm)}\')">${esc(nm)}</span></td>
        ${cells}
      </tr>`;
    }).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Changed (${fc.length})</span></div>
        <table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${rows}</tbody></table>
      </div>`;
  }
  return html;
}

function renderRenewalChanges(data) {
  const addCols = [
    { key: "Opportunity Name", label: "Opportunity" },
    { key: "Account Name",     label: "Account" },
    { key: "Amount",           label: "Amount" },
    { key: "Close Date",       label: "Close Date" },
    { key: "Account Risk",     label: "Risk" }
  ];
  let html = "";
  html += buildSimpleTable("+ Added",         data.added,   addCols, "chg-added");
  html += buildSimpleTable("&#8722; Removed", data.removed, addCols, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const na = `<td class="chg-null">—</td>`;
    const rows = fc.map(r => {
      const acct = r["Account Name"] || "";
      return `<tr>
        <td>${esc(r["Opportunity Name"] || "")}</td>
        <td><span class="acct-link" onclick="goToAccount(\'${esc(acct)}\')">${esc(acct)}</span></td>
        ${r.risk_changed     ? `<td>${chgArrow(r["Account Risk.old"],   r["Account Risk.new"])}</td>`   : na}
        ${r.forecast_changed ? `<td>${chgArrow(r["Churn Forecast.old"], r["Churn Forecast.new"])}</td>` : na}
        ${r.amount_changed   ? `<td>${chgArrow(r["Amount.old"],         r["Amount.new"])}</td>`         : na}
        ${r.date_changed     ? `<td>${chgArrow(r["Close Date.old"],     r["Close Date.new"])}</td>`     : na}
      </tr>`;
    }).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Changed (${fc.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>Opportunity</th><th>Account</th><th>Risk</th><th>Forecast</th><th>Amount</th><th>Date</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

function renderExpansionChanges(data) {
  const addCols = [
    { key: "Opportunity Name",  label: "Opportunity" },
    { key: "Account Name",      label: "Account" },
    { key: "Amount",            label: "Amount" },
    { key: "Fiscal Period",     label: "Period" },
    { key: "Opportunity Owner", label: "Owner" }
  ];
  let html = "";
  html += buildSimpleTable("+ Added",         data.added,   addCols, "chg-added");
  html += buildSimpleTable("&#8722; Removed", data.removed, addCols, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const na = `<td class="chg-null">—</td>`;
    const rows = fc.map(r => {
      const acct = r["Account Name"] || "";
      return `<tr>
        <td>${esc(r["Opportunity Name"] || "")}</td>
        <td><span class="acct-link" onclick="goToAccount(\'${esc(acct)}\')">${esc(acct)}</span></td>
        ${r.amount_changed ? `<td>${chgArrow(r["Amount.old"],        r["Amount.new"])}</td>`        : na}
        ${r.period_changed ? `<td>${chgArrow(r["Fiscal Period.old"], r["Fiscal Period.new"])}</td>` : na}
      </tr>`;
    }).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Changed (${fc.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>Opportunity</th><th>Account</th><th>Amount</th><th>Fiscal Period</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

function renderCSMChanges(data) {
  let html = "";

  function dynTable(label, rows, badgeCls) {
    if (!rows || rows.length === 0) return "";
    const keys = Object.keys(rows[0] || {});
    if (!keys.length) return "";
    const hdrs = keys.map(k => `<th>${esc(k)}</th>`).join("");
    const body = rows.map(r =>
      `<tr>${keys.map(k => `<td>${esc(String(r[k] || ""))}</td>`).join("")}</tr>`
    ).join("");
    return `<div class="chg-group"><div class="chg-group-title"><span class="chg-badge ${badgeCls}">${label} (${rows.length})</span></div><table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${body}</tbody></table></div>`;
  }

  html += dynTable("+ Added",         data.added,   "chg-added");
  html += dynTable("&#8722; Removed", data.removed, "chg-removed");

  const rs = data.reassignments || [];
  if (rs.length > 0) {
    const keys   = Object.keys(rs[0] || {});
    const keyCol = keys[0] || "";
    const newCol = keys.find(k => k.endsWith(".new")) || "";
    const oldCol = keys.find(k => k.endsWith(".old")) || "";
    const fieldLabel = newCol.replace(".new", "") || "CSM";
    const rows = rs.map(r =>
      `<tr><td>${esc(r[keyCol] || "")}</td><td>${chgArrow(r[oldCol], r[newCol])}</td></tr>`
    ).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Reassignments (${rs.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>${esc(keyCol)}</th><th>${esc(fieldLabel)}</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

function renderSaaSChanges(data) {
  let html = "";

  function dynTable(label, rows, badgeCls) {
    if (!rows || rows.length === 0) return "";
    const keys = Object.keys(rows[0] || {});
    if (!keys.length) return "";
    const hdrs = keys.map(k => `<th>${esc(k)}</th>`).join("");
    const body = rows.map(r =>
      `<tr>${keys.map(k => `<td>${esc(String(r[k] || ""))}</td>`).join("")}</tr>`
    ).join("");
    return `<div class="chg-group"><div class="chg-group-title"><span class="chg-badge ${badgeCls}">${label} (${rows.length})</span></div><table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${body}</tbody></table></div>`;
  }

  html += dynTable("+ Added",         data.added,   "chg-added");
  html += dynTable("&#8722; Removed", data.removed, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const keys   = Object.keys(fc[0] || {});
    const newCol = keys.find(k => k.endsWith(".new")) || "";
    const oldCol = keys.find(k => k.endsWith(".old")) || "";
    const keyCol = keys.find(k => !k.endsWith(".new") && !k.endsWith(".old")) || keys[0] || "";
    if (newCol && oldCol) {
      const fieldLabel = newCol.replace(".new", "");
      const rows = fc.map(r =>
        `<tr><td>${esc(r[keyCol] || "")}</td><td>${chgArrow(r[oldCol], r[newCol])}</td></tr>`
      ).join("");
      html += `
        <div class="chg-group">
          <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Stage Changes (${fc.length})</span></div>
          <table class="flat-table">
            <thead><tr><th>${esc(keyCol)}</th><th>${esc(fieldLabel)}</th></tr></thead>
            <tbody>${rows}</tbody>
          </table>
        </div>`;
    } else {
      html += dynTable("&#126; Changed", fc, "chg-changed");
    }
  }
  return html;
}

// ── Diff-date picker ───────────────────────────────────────────────────────
function populateSnapshots(dates) {
  const sel = document.getElementById("diff-date");
  if (!dates || dates.length === 0) {
    sel.innerHTML = \'<option value="">No snapshots found</option>\';
    return;
  }
  sel.innerHTML = dates.map((d, i) =>
    `<option value="${d}"${i === 0 ? " selected" : ""}>${d}</option>`
  ).join("");
}

async function initSnapshots() {
  try {
    const res = await fetch("http://localhost:8765/snapshots", { signal: AbortSignal.timeout(2000) });
    const data = await res.json();
    if (data.dates && data.dates.length > 0) populateSnapshots(data.dates);
    else populateSnapshots(SNAP_DATES);
  } catch(e) {
    populateSnapshots(SNAP_DATES);   // server not running — use dates baked into the HTML
  }
}
initSnapshots();

// ── Refresh ────────────────────────────────────────────────────────────────
async function triggerRefresh() {
  const btn      = document.getElementById("refresh-btn");
  const diffDate = document.getElementById("diff-date").value;
  const orig     = btn.innerHTML;
  btn.innerHTML  = "Refreshing…";
  btn.disabled   = true;
  try {
    await fetch("http://localhost:8765/status", { signal: AbortSignal.timeout(3000) });
    const url = "http://localhost:8765/refresh" + (diffDate ? "?diff_date=" + encodeURIComponent(diffDate) : "");
    const res  = await fetch(url, { signal: AbortSignal.timeout(300000) });
    const data = await res.json();
    if (data.status === "error") throw new Error(data.message);
    btn.innerHTML = "✓ Done — reloading…";
    setTimeout(() => location.reload(), 1800);
  } catch(e) {
    btn.disabled  = false;
    btn.innerHTML = orig;
    document.getElementById("refresh-modal").style.display = "flex";
  }
}

function closeRefreshModal() {
  document.getElementById("refresh-modal").style.display = "none";
}

function copyModalCmd(el) {
  navigator.clipboard.writeText(el.textContent.trim()).then(() => {
    const orig = el.textContent;
    el.textContent = "Copied!";
    setTimeout(() => { el.textContent = orig; }, 1500);
  });
}

// ── Init ───────────────────────────────────────────────────────────────────
render();
</script>
</body>
</html>'

  tmpl <- gsub("{{REGION_PILLS}}", region_pills,          tmpl, fixed = TRUE)
  tmpl <- gsub("{{CSM_PILLS}}",   csm_pills,             tmpl, fixed = TRUE)
  tmpl <- gsub("{{SM_LIFECYCLE_PILLS}}", sm_lifecycle_pills, tmpl, fixed = TRUE)
  tmpl <- gsub("{{SNAP_DATES}}",  .to_json(snap_dates),  tmpl, fixed = TRUE)
  tmpl <- gsub("{{CHANGES}}",      changes_json,          tmpl, fixed = TRUE)
  tmpl <- gsub("{{CONSUMPTION}}",  consumption_json,      tmpl, fixed = TRUE)
  tmpl <- gsub("{{JSON_DATA}}",    json_data,             tmpl, fixed = TRUE)
  tmpl
}
