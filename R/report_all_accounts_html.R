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
#'   Defaults to \code{"output/account_briefs/immuta_accounts.html"}.
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
                                     output_path = "output/account_briefs/immuta_accounts.html") {

  # Ensure output directory exists
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  accounts     <- op_list$accounts
  renewals     <- op_list$renewals
  expansions   <- op_list$expansions
  active_orders <- op_list$active_orders

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
            amount        = fmt_usd(r[["Amount"]]),
            end_date      = fmt_date(r[["Current Contract End Date"]]),
            churn_quarter = safe_val(r[["Churn Quarter"]])
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
            name          = safe_val(e[["Opportunity Name"]]),
            owner         = safe_val(e[["Opportunity Owner"]]),
            amount        = fmt_usd(e[["Amount"]]),
            fiscal_period = safe_val(e[["Fiscal Period"]])
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

    list(
      id            = gsub("[^A-Za-z0-9]", "_", aname),
      name          = aname,
      region        = safe_val(acct$Region),
      arr           = fmt_usd(acct$`Contract ARR Rollup`),
      arr_raw       = suppressWarnings(as.numeric(acct$`Contract ARR Rollup`)),
      csm           = safe_val(acct$`Customer Success Manager`),
      account_owner = safe_val(acct$`Account Owner`),
      field_cto     = safe_val(acct$`Field CTO`),
      is_saas       = is_saas,
      unsupported   = isTRUE(unsupported),
      churn_summary = churn_summary,
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

  # Build HTML -----------------------------------------------------------------
  html <- .rah_html(json_data, regions)

  writeLines(html, output_path, useBytes = FALSE)
  message(".. wrote account HTML dashboard to ", output_path)
  invisible(output_path)
}


# ---- Internal: minimal JSON serialiser (no jsonlite dependency) -------------

.to_json <- function(x) {
  if (is.null(x))      return("null")
  if (is.logical(x))   return(if (isTRUE(x)) "true" else "false")
  if (is.numeric(x))   return(if (is.nan(x) || is.infinite(x) || is.na(x)) "null"
                              else as.character(x))
  if (is.character(x)) {
    if (length(x) == 1) {
      if (is.na(x)) return("null")
      x <- gsub("\\\\", "\\\\\\\\", x)
      x <- gsub('"',  '\\\\"',  x)
      x <- gsub('\n', '\\\\n', x)
      x <- gsub('\r', '\\\\r', x)
      x <- gsub('\t', '\\\\t', x)
      return(paste0('"', x, '"'))
    }
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))
  }
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


# ---- Internal: full HTML page template -------------------------------------

.rah_html <- function(json_data, regions) {

  region_pills <- paste(
    vapply(regions, function(r) {
      re <- html_escape(r)
      paste0('<button class="pill" data-region="', re, '">', re, '</button>')
    }, character(1)),
    collapse = "\n    "
  )

  tmpl <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Immuta Accounts</title>
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

  :root {
    --blue:    #005EB8;
    --blue-lt: #E8F1FB;
    --teal:    #00A3A1;
    --amber:   #F5A623;
    --red:     #D0021B;
    --green:   #27AE60;
    --gray1:   #1A1A2E;
    --gray2:   #4A4A6A;
    --gray3:   #8A8AA8;
    --gray4:   #D0D0E0;
    --gray5:   #F5F5FA;
    --white:   #FFFFFF;
    --radius:  8px;
    --shadow:  0 2px 8px rgba(0,0,50,.08);
    --sidebar: 260px;
  }

  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 14px;
    color: var(--gray1);
    background: var(--gray5);
    display: flex;
    flex-direction: column;
    min-height: 100vh;
  }

  /* ── Top bar ─────────────────────────────────────────────────────────── */
  .topbar {
    position: sticky;
    top: 0;
    z-index: 100;
    background: var(--blue);
    color: var(--white);
    padding: 0 20px;
    height: 56px;
    display: flex;
    align-items: center;
    gap: 16px;
    box-shadow: 0 2px 6px rgba(0,0,0,.25);
  }
  .topbar-logo {
    font-size: 18px;
    font-weight: 700;
    letter-spacing: -.3px;
    white-space: nowrap;
  }
  .topbar-logo span { color: #7EC8E3; }
  .search-wrap {
    flex: 1;
    max-width: 400px;
    position: relative;
  }
  .search-wrap input {
    width: 100%%;
    padding: 8px 12px 8px 36px;
    border: none;
    border-radius: 20px;
    font-size: 14px;
    background: rgba(255,255,255,.15);
    color: var(--white);
    outline: none;
    transition: background .2s;
  }
  .search-wrap input::placeholder { color: rgba(255,255,255,.6); }
  .search-wrap input:focus { background: rgba(255,255,255,.25); }
  .search-icon {
    position: absolute;
    left: 10px;
    top: 50%%;
    transform: translateY(-50%%);
    opacity: .7;
    font-size: 15px;
    pointer-events: none;
  }
  .count-badge {
    font-size: 12px;
    opacity: .75;
    white-space: nowrap;
  }

  /* ── Layout ──────────────────────────────────────────────────────────── */
  .layout {
    display: flex;
    flex: 1;
    gap: 0;
  }

  /* ── Sidebar ─────────────────────────────────────────────────────────── */
  .sidebar {
    width: var(--sidebar);
    flex-shrink: 0;
    background: var(--white);
    border-right: 1px solid var(--gray4);
    position: sticky;
    top: 56px;
    height: calc(100vh - 56px);
    overflow-y: auto;
    padding: 16px 0 32px;
  }
  .sidebar-section { margin-bottom: 20px; }
  .sidebar-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: .8px;
    color: var(--gray3);
    padding: 0 16px 8px;
  }
  .region-filters {
    display: flex;
    flex-direction: column;
    gap: 2px;
    padding: 0 8px;
  }
  .pill {
    text-align: left;
    padding: 6px 10px;
    border-radius: 6px;
    border: none;
    background: transparent;
    font-size: 13px;
    color: var(--gray2);
    cursor: pointer;
    transition: background .15s, color .15s;
    font-family: inherit;
  }
  .pill:hover { background: var(--blue-lt); color: var(--blue); }
  .pill.active { background: var(--blue); color: var(--white); font-weight: 600; }

  .alpha-jump {
    display: flex;
    flex-wrap: wrap;
    gap: 3px;
    padding: 0 12px;
  }
  .alpha-jump a {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 600;
    color: var(--gray2);
    text-decoration: none;
    transition: background .15s, color .15s;
  }
  .alpha-jump a:hover { background: var(--blue-lt); color: var(--blue); }
  .alpha-jump a.has-accounts { color: var(--blue); }
  .alpha-jump a.disabled { color: var(--gray4); pointer-events: none; }

  /* ── Main content ────────────────────────────────────────────────────── */
  .main {
    flex: 1;
    padding: 20px 24px 40px;
    min-width: 0;
  }

  /* letter heading */
  .letter-group { margin-bottom: 8px; }
  .letter-heading {
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 1px;
    text-transform: uppercase;
    color: var(--gray3);
    padding: 12px 0 6px;
    border-bottom: 1px solid var(--gray4);
    margin-bottom: 10px;
  }

  /* ── Account card ────────────────────────────────────────────────────── */
  .card {
    background: var(--white);
    border-radius: var(--radius);
    box-shadow: var(--shadow);
    margin-bottom: 10px;
    overflow: hidden;
    border: 1px solid var(--gray4);
    transition: box-shadow .2s;
  }
  .card:hover { box-shadow: 0 4px 16px rgba(0,0,50,.12); }

  .card-header {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 14px 16px;
    cursor: pointer;
    user-select: none;
  }
  .card-header:hover { background: var(--gray5); }

  .card-name {
    font-size: 15px;
    font-weight: 600;
    color: var(--gray1);
    flex: 1;
    min-width: 0;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .card-arr {
    font-size: 14px;
    font-weight: 600;
    color: var(--blue);
    white-space: nowrap;
  }
  .card-meta {
    display: flex;
    align-items: center;
    gap: 10px;
    flex-shrink: 0;
  }
  .tag {
    font-size: 11px;
    font-weight: 600;
    padding: 2px 8px;
    border-radius: 10px;
    white-space: nowrap;
  }
  .tag-region { background: #EEF3FF; color: #3B5FC0; }
  .tag-saas   { background: #E8F8F4; color: var(--teal); }
  .tag-churn  { background: #FFF4E5; color: #B76E00; }
  .tag-renew  { background: #FDECEA; color: var(--red); }
  .churn-chip { font-size: 11px; color: var(--amber); font-weight: 600; }

  .expand-icon {
    font-size: 12px;
    color: var(--gray3);
    transition: transform .2s;
    flex-shrink: 0;
  }
  .card.open .expand-icon { transform: rotate(180deg); }

  .card-body {
    display: none;
    padding: 0 16px 16px;
    border-top: 1px solid var(--gray4);
  }
  .card.open .card-body { display: block; }

  /* tabs */
  .tabs {
    display: flex;
    gap: 0;
    border-bottom: 2px solid var(--gray4);
    margin-bottom: 14px;
    margin-top: 12px;
  }
  .tab-btn {
    padding: 8px 16px;
    border: none;
    background: none;
    font-size: 13px;
    font-weight: 500;
    color: var(--gray3);
    cursor: pointer;
    border-bottom: 2px solid transparent;
    margin-bottom: -2px;
    font-family: inherit;
    transition: color .15s;
  }
  .tab-btn:hover { color: var(--blue); }
  .tab-btn.active { color: var(--blue); border-bottom-color: var(--blue); font-weight: 600; }
  .tab-pane { display: none; }
  .tab-pane.active { display: block; }

  /* overview grid */
  .field-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
    gap: 10px;
  }
  .field-item {}
  .field-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: .6px;
    color: var(--gray3);
    margin-bottom: 2px;
  }
  .field-value {
    font-size: 13px;
    color: var(--gray1);
    font-weight: 500;
  }
  .field-value.arr-value { color: var(--blue); font-size: 16px; font-weight: 700; }
  .em-dash { color: var(--gray4); }

  /* table */
  .data-table {
    width: 100%%;
    border-collapse: collapse;
    font-size: 13px;
  }
  .data-table th {
    text-align: left;
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: .6px;
    color: var(--gray3);
    padding: 6px 10px;
    border-bottom: 1px solid var(--gray4);
  }
  .data-table td {
    padding: 8px 10px;
    border-bottom: 1px solid var(--gray5);
    color: var(--gray1);
  }
  .data-table tr:last-child td { border-bottom: none; }
  .data-table tr:hover td { background: var(--gray5); }
  .empty-msg {
    font-size: 12px;
    color: var(--gray3);
    font-style: italic;
    padding: 8px 0;
  }

  /* arch badges */
  .arch-badge {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 4px 10px;
    border-radius: 12px;
    font-size: 12px;
    font-weight: 600;
    margin: 3px;
  }
  .arch-yes { background: #E8F8F4; color: var(--teal); }
  .arch-no  { background: var(--gray5); color: var(--gray3); }
  .arch-warn { background: #FDECEA; color: var(--red); }

  /* no results */
  #no-results {
    display: none;
    text-align: center;
    padding: 60px 20px;
    color: var(--gray3);
    font-size: 16px;
  }
</style>
</head>
<body>

<!-- TOP BAR -->
<div class="topbar">
  <div class="topbar-logo">Immuta <span>Accounts</span></div>
  <div class="search-wrap">
    <span class="search-icon">&#128269;</span>
    <input type="text" id="search" placeholder="Search accounts..." autocomplete="off">
  </div>
  <div class="count-badge" id="count-badge"></div>
</div>

<div class="layout">

  <!-- SIDEBAR -->
  <aside class="sidebar">
    <div class="sidebar-section">
      <div class="sidebar-label">Filter by Region</div>
      <div class="region-filters">
        <button class="pill active" data-region="all">All Regions</button>
        {{REGION_PILLS}}
      </div>
    </div>
    <div class="sidebar-section">
      <div class="sidebar-label">Jump to Letter</div>
      <div class="alpha-jump" id="alpha-jump"></div>
    </div>
  </aside>

  <!-- MAIN -->
  <main class="main" id="main-content">
    <div id="account-list"></div>
    <div id="no-results">No accounts match your search.</div>
  </main>

</div>

<script>
const ACCOUNTS = {{JSON_DATA}};

// ── State ──────────────────────────────────────────────────────────────────
let activeRegion = "all";
let searchTerm   = "";

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

// ── Build cards ────────────────────────────────────────────────────────────
function buildCard(a) {
  const hasRenewals   = a.renewals.length > 0;
  const hasExpansions = a.expansions.length > 0;

  // Renewal rows
  let renewRows = "";
  if (hasRenewals) {
    renewRows = a.renewals.map(r => `
      <tr>
        <td>${esc(r.name)}</td>
        <td>${esc(r.amount)}</td>
        <td>${esc(r.end_date)}</td>
        <td><strong>${esc(r.churn_quarter)}</strong></td>
      </tr>`).join("");
  }

  // Expansion rows
  let expRows = "";
  if (hasExpansions) {
    expRows = a.expansions.map(e => `
      <tr>
        <td>${esc(e.name)}</td>
        <td>${esc(e.owner)}</td>
        <td>${esc(e.amount)}</td>
        <td>${esc(e.fiscal_period)}</td>
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
  const churnTag = a.churn_summary
    ? `<span class="tag tag-churn">${esc(a.churn_summary)}</span>` : "";

  return `
<div class="card" id="acct-${esc(a.id)}" data-region="${esc(a.region)}" data-name="${esc(a.name.toLowerCase())}">
  <div class="card-header" onclick="toggleCard(this)">
    <div class="card-name">${esc(a.name)}</div>
    <div class="card-arr">${esc(a.arr)}</div>
    <div class="card-meta">
      ${regionTag}${saasTag}${churnTag}
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
      <div class="field-grid">
        ${fieldHtml("Contract ARR", a.arr, "arr-value")}
        ${fieldHtml("Region", a.region)}
        ${fieldHtml("Customer Success Manager", a.csm)}
        ${fieldHtml("Account Owner", a.account_owner)}
        ${fieldHtml("Field CTO", a.field_cto)}
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
        <thead><tr><th>Opportunity</th><th>Amount</th><th>End Date</th><th>Churn Quarter</th></tr></thead>
        <tbody>${renewRows}</tbody>
      </table>` : \'<p class="empty-msg">No open renewals.</p>\'}
    </div>

    <!-- Expansions tab -->
    <div class="tab-pane" id="expand-${esc(a.id)}">
      ${hasExpansions ? `
      <table class="data-table">
        <thead><tr><th>Opportunity</th><th>Owner</th><th>Amount</th><th>Fiscal Period</th></tr></thead>
        <tbody>${expRows}</tbody>
      </table>` : \'<p class="empty-msg">No open expansion opportunities.</p>\'}
    </div>
  </div>
</div>`;
}

// ── Render ─────────────────────────────────────────────────────────────────
function render() {
  const q       = searchTerm.toLowerCase();
  const region  = activeRegion;

  const visible = ACCOUNTS.filter(a => {
    const matchRegion = region === "all" || a.region === region;
    const matchSearch = !q || a.name.toLowerCase().includes(q);
    return matchRegion && matchSearch;
  });

  // Group by first letter
  const groups = {};
  visible.forEach(a => {
    const letter = (a.name[0] || "#").toUpperCase();
    if (!groups[letter]) groups[letter] = [];
    groups[letter].push(a);
  });

  const letters = Object.keys(groups).sort();
  let html = "";
  letters.forEach(letter => {
    html += `<div class="letter-group" id="letter-${letter}">
      <div class="letter-heading">${letter}</div>`;
    groups[letter].forEach(a => { html += buildCard(a); });
    html += "</div>";
  });

  document.getElementById("account-list").innerHTML = html;
  document.getElementById("no-results").style.display = visible.length === 0 ? "block" : "none";
  document.getElementById("count-badge").textContent =
    visible.length + " account" + (visible.length !== 1 ? "s" : "");

  buildAlphaJump(letters);
}

// ── Alpha jump bar ─────────────────────────────────────────────────────────
function buildAlphaJump(visibleLetters) {
  const container = document.getElementById("alpha-jump");
  const all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".split("");
  container.innerHTML = all.map(l => {
    if (visibleLetters.includes(l)) {
      return `<a href="#letter-${l}" class="has-accounts">${l}</a>`;
    }
    return `<a class="disabled">${l}</a>`;
  }).join("");
}

// ── Card toggle ────────────────────────────────────────────────────────────
function toggleCard(header) {
  const card = header.parentElement;
  card.classList.toggle("open");
}

// ── Tab switch ─────────────────────────────────────────────────────────────
function switchTab(btn, paneId) {
  const tabs = btn.closest(".tabs");
  const body = tabs.nextElementSibling;
  tabs.querySelectorAll(".tab-btn").forEach(b => b.classList.remove("active"));
  btn.classList.add("active");
  // find all tab panes in parent card-body
  const cardBody = btn.closest(".card-body");
  cardBody.querySelectorAll(".tab-pane").forEach(p => p.classList.remove("active"));
  document.getElementById(paneId).classList.add("active");
}

// ── Filters ────────────────────────────────────────────────────────────────
document.querySelectorAll(".pill").forEach(btn => {
  btn.addEventListener("click", () => {
    document.querySelectorAll(".pill").forEach(b => b.classList.remove("active"));
    btn.classList.add("active");
    activeRegion = btn.dataset.region;
    render();
  });
});

document.getElementById("search").addEventListener("input", e => {
  searchTerm = e.target.value;
  render();
});

// ── Init ───────────────────────────────────────────────────────────────────
render();
</script>
</body>
</html>'

  tmpl <- gsub("{{REGION_PILLS}}", region_pills, tmpl, fixed = TRUE)
  tmpl <- gsub("{{JSON_DATA}}",    json_data,    tmpl, fixed = TRUE)
  tmpl
}
