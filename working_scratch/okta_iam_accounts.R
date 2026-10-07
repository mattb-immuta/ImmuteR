# =============================================================================
# Okta IAM accounts -> Markdown
#
# Lists every account whose IAM includes Okta, from the latest saved op_list
# snapshot (data/opportunities_<date>.RData). Accounts with several IAMs are
# included as long as Okta is one of them; their other IAMs are listed.
#
# Run from the project root:
#   source("working_scratch/okta_iam_accounts.R")
# Writes: output/okta_iam_accounts.md
# =============================================================================

devtools::load_all()   # for fmt_usd() / safe_val()

# --- Latest snapshot ----------------------------------------------------------
snaps <- list.files("data/", pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
if (length(snaps) == 0) stop("No op_list snapshot found in data/.")
snap_date <- max(as.Date(gsub("opportunities_(.+)\\.RData", "\\1", snaps)))
snap_file <- file.path("data", paste0("opportunities_", snap_date, ".RData"))
message("Loading ", snap_file)

# Load into its own environment so this doesn't overwrite an op_list already in
# the session.
snap_env <- new.env()
load(snap_file, envir = snap_env)
accts <- snap_env$op_list$accounts

if (!"iam" %in% names(accts)) {
  stop(snap_file, " has no accounts$iam column; it predates the ",
       "critical_technologies.json join. Rerun scripts/main.R first.")
}

# --- Okta accounts ------------------------------------------------------------
iam_split <- strsplit(ifelse(is.na(accts$iam), "", accts$iam), " · ", fixed = TRUE)
has_okta  <- vapply(iam_split, function(x) any(grepl("okta", x, ignore.case = TRUE)), logical(1))

okta <- accts[has_okta, , drop = FALSE]
okta_iams <- iam_split[has_okta]
other_iams <- vapply(okta_iams, function(x) {
  o <- x[!grepl("okta", x, ignore.case = TRUE)]
  if (length(o) == 0) "" else paste(o, collapse = ", ")
}, character(1))

col <- function(df, name) if (name %in% names(df)) df[[name]] else rep(NA, nrow(df))
md_cell <- function(x) gsub("|", "\\|", safe_val(x), fixed = TRUE)

tbl <- data.frame(
  account     = col(okta, "Account Name"),
  other_iams  = other_iams,
  ext_catalog = col(okta, "ext_catalog"),
  region      = col(okta, "Region"),
  csm         = col(okta, "Customer Success Manager"),
  arr_raw     = suppressWarnings(as.numeric(col(okta, "Contract ARR Rollup"))),
  stringsAsFactors = FALSE
)
tbl <- tbl[order(tbl$account), ]

n_multi <- sum(nzchar(tbl$other_iams))

# --- Markdown -----------------------------------------------------------------
lines <- c(
  "# Okta IAM Accounts",
  "",
  paste0("Source: `", snap_file, "` (accounts$iam, from ",
         "`data/input/critical_technologies.json`). Generated ", Sys.Date(), "."),
  "",
  paste0("- **", nrow(tbl), "** accounts use Okta"),
  paste0("- **", nrow(tbl) - n_multi, "** use Okta only"),
  paste0("- **", n_multi, "** use Okta alongside another IAM"),
  "",
  "| Account | Other IAMs | External Catalog | Region | CSM | ARR |",
  "|---|---|---|---|---|---:|",
  sprintf("| %s | %s | %s | %s | %s | %s |",
          md_cell(tbl$account),
          ifelse(nzchar(tbl$other_iams), md_cell(tbl$other_iams), "—"),
          md_cell(tbl$ext_catalog),
          md_cell(tbl$region),
          md_cell(tbl$csm),
          vapply(tbl$arr_raw, fmt_usd, character(1)))
)

dir.create("output", showWarnings = FALSE)
out_path <- "output/okta_iam_accounts.md"
writeLines(lines, out_path)
message("Wrote ", nrow(tbl), " Okta accounts (", n_multi, " multi-IAM) to ", out_path)

# --- HTML (same look as output/immuta_accounts.html) --------------------------
# Rows are embedded as JSON and rendered client-side so the search box and
# column sorting work without a server.
split_dot <- function(x) {
  if (is.na(x) || !nzchar(x)) character(0) else strsplit(x, " · ", fixed = TRUE)[[1]]
}
rows <- lapply(seq_len(nrow(tbl)), function(i) {
  j <- match(tbl$account[i], okta$`Account Name`)
  u <- col(okta, "customer_central_url")[j]
  iams <- okta_iams[[j]]
  list(
    account     = tbl$account[i],
    url         = if (is.na(u) || !nzchar(u)) "" else as.character(u),
    # I() keeps these as JSON arrays even with one element under auto_unbox.
    other_iams  = I(iams[!grepl("okta", iams, ignore.case = TRUE)]),
    ext_catalog = I(split_dot(tbl$ext_catalog[i])),
    region      = if (is.na(tbl$region[i])) "" else tbl$region[i],
    csm         = if (is.na(tbl$csm[i]))    "" else tbl$csm[i],
    arr_raw     = tbl$arr_raw[i],
    arr         = fmt_usd(tbl$arr_raw[i])
  )
})
rows_json <- jsonlite::toJSON(rows, auto_unbox = TRUE, na = "null")

total_arr <- sum(tbl$arr_raw, na.rm = TRUE)

html_tmpl <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Immuta | Okta IAM Accounts</title>
<link rel="icon" href="https://www.immuta.com/favicon.ico">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap" rel="stylesheet">
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
  :root {
    --im-black: #0D0D0D; --im-lime: #C8F135; --im-lime-dk: #A8CC18;
    --im-lime-lt: #F5FCD8; --im-white: #FFFFFF;
    --text-1: #111111; --text-2: #555555; --text-3: #999999;
    --border: #E5E5E5; --surface: #F8F8F6; --surface-2: #F0F0EE;
    --radius: 10px; --radius-sm: 6px;
    --shadow: 0 1px 3px rgba(0,0,0,.06), 0 4px 12px rgba(0,0,0,.06);
    --topbar-h: 60px;
    --font: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }
  body {
    font-family: var(--font); font-size: 14px; line-height: 1.5;
    color: var(--text-1); background: var(--surface);
    min-height: 100vh; -webkit-font-smoothing: antialiased;
  }

  /* Top bar */
  .topbar {
    position: sticky; top: 0; z-index: 100;
    background: var(--im-black); color: var(--im-white);
    padding: 0 24px; height: var(--topbar-h);
    display: flex; align-items: center; gap: 20px;
  }
  .topbar-logo { display: flex; align-items: center; gap: 14px; text-decoration: none; flex-shrink: 0; }
  .topbar-logo svg { display: block; flex-shrink: 0; }
  .topbar-section {
    font-size: 13px; font-weight: 400; color: rgba(255,255,255,.4);
    padding-left: 14px; border-left: 1px solid rgba(255,255,255,.15);
  }
  .search-wrap { flex: 1; max-width: 420px; position: relative; margin-left: 4px; }
  .search-wrap input {
    width: 100%; padding: 8px 14px 8px 38px;
    border: 1px solid rgba(255,255,255,.12); border-radius: var(--radius-sm);
    font-size: 13px; font-family: var(--font);
    background: rgba(255,255,255,.06); color: var(--im-white); outline: none;
    transition: background .15s, border-color .15s;
  }
  .search-wrap input::placeholder { color: rgba(255,255,255,.3); }
  .search-wrap input:focus { background: rgba(255,255,255,.1); border-color: var(--im-lime); }
  .search-icon {
    position: absolute; left: 11px; top: 50%; transform: translateY(-50%);
    color: rgba(255,255,255,.3); font-size: 14px; pointer-events: none;
  }
  .count-badge { font-size: 12px; color: rgba(255,255,255,.38); white-space: nowrap; margin-left: auto; }
  .topbar-updated { font-size: 10.5px; font-weight: 500; letter-spacing: .3px; color: rgba(255,255,255,.45); white-space: nowrap; }

  /* Page */
  .page { max-width: 1240px; margin: 0 auto; padding: 28px 24px 48px; }
  .page-title { font-size: 22px; font-weight: 800; letter-spacing: -.5px; }
  .page-sub { font-size: 13px; color: var(--text-2); margin-top: 4px; }
  .page-sub code { font-size: 12px; background: var(--surface-2); padding: 1px 5px; border-radius: 4px; }

  .stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 14px; margin: 22px 0 20px; }
  .stat {
    background: var(--im-white); border: 1px solid var(--border);
    border-radius: var(--radius); box-shadow: var(--shadow); padding: 14px 18px;
  }
  .stat-label { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 1px; color: var(--text-3); }
  .stat-value { font-size: 24px; font-weight: 800; letter-spacing: -.5px; margin-top: 2px; }
  .stat.accent { background: var(--im-black); border-color: var(--im-black); }
  .stat.accent .stat-label { color: rgba(255,255,255,.45); }
  .stat.accent .stat-value { color: var(--im-lime); }

  .filters { display: flex; gap: 6px; margin-bottom: 12px; }
  .pill {
    padding: 6px 12px; border-radius: var(--radius-sm); border: 1px solid var(--border);
    background: var(--im-white); font-size: 12.5px; font-family: var(--font);
    color: var(--text-2); cursor: pointer; transition: background .12s, color .12s;
  }
  .pill:hover { background: var(--im-lime-lt); color: var(--text-1); }
  .pill.active { background: var(--im-black); border-color: var(--im-black); color: var(--im-lime); font-weight: 600; }

  .card {
    background: var(--im-white); border: 1px solid var(--border);
    border-radius: var(--radius); box-shadow: var(--shadow); overflow-x: auto;
  }
  .flat-table { width: 100%; border-collapse: collapse; font-size: 13px; }
  .flat-table th {
    text-align: left; font-size: 10px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 1px; color: var(--text-3); padding: 10px 14px;
    border-bottom: 1.5px solid var(--border); background: var(--im-white);
    cursor: pointer; user-select: none; white-space: nowrap; transition: color .12s;
  }
  .flat-table th.num, .flat-table td.num { text-align: right; }
  .flat-table th:hover { color: var(--text-1); }
  .flat-table th.sort-asc::after  { content: " \\2191"; color: var(--im-lime-dk); }
  .flat-table th.sort-desc::after { content: " \\2193"; color: var(--im-lime-dk); }
  .flat-table td { padding: 10px 14px; border-bottom: 1px solid var(--surface-2); vertical-align: middle; }
  .flat-table tr:hover td { background: var(--im-lime-lt); }
  .flat-table tr:last-child td { border-bottom: none; }
  .acct-link { font-weight: 600; color: var(--text-1); text-decoration: none; }
  a.acct-link:hover { text-decoration: underline; text-decoration-color: var(--im-lime); text-decoration-thickness: 2px; }
  .tag {
    display: inline-block; font-size: 11.5px; font-weight: 500; padding: 2px 8px;
    border-radius: 999px; background: var(--surface-2); color: var(--text-2);
    margin: 1px 4px 1px 0; white-space: nowrap;
  }
  .tag.okta { background: var(--im-black); color: var(--im-lime); font-weight: 600; }
  .muted { color: var(--text-3); }
  .arr { font-weight: 600; font-variant-numeric: tabular-nums; }
  .empty-msg { padding: 28px; text-align: center; color: var(--text-3); font-size: 13px; }

  @media (max-width: 720px) {
    .topbar { padding: 0 16px; gap: 12px; }
    .topbar-section, .count-badge, .topbar-updated { display: none; }
    .page { padding: 20px 16px 40px; }
  }
</style>
</head>
<body>

<div class="topbar">
  <a class="topbar-logo" href="https://www.immuta.com" target="_blank" rel="noopener">
    <svg width="114" height="20" viewBox="0 0 142 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path {{LOGO_PATH}} fill="white"/>
    </svg>
    <span class="topbar-section">Okta IAM Accounts</span>
  </a>
  <div class="search-wrap">
    <span class="search-icon">&#128269;</span>
    <input type="text" id="search" placeholder="Search accounts, CSMs, IAMs..." autocomplete="off">
  </div>
  <div class="count-badge" id="count-badge"></div>
  <div class="topbar-updated">{{UPDATED}}</div>
</div>

<div class="page">
  <div class="page-title">Accounts using Okta for IAM</div>
  <div class="page-sub">Includes accounts running Okta alongside another IAM. Source: <code>{{SNAP_FILE}}</code> &middot; <code>accounts$iam</code> from <code>data/input/critical_technologies.json</code></div>

  <div class="stats">
    <div class="stat accent"><div class="stat-label">Okta accounts</div><div class="stat-value">{{N_TOTAL}}</div></div>
    <div class="stat"><div class="stat-label">Okta only</div><div class="stat-value">{{N_ONLY}}</div></div>
    <div class="stat"><div class="stat-label">Okta + another IAM</div><div class="stat-value">{{N_MULTI}}</div></div>
    <div class="stat"><div class="stat-label">Combined ARR</div><div class="stat-value">{{TOTAL_ARR}}</div></div>
  </div>

  <div class="filters">
    <button class="pill active" data-f="all">All</button>
    <button class="pill" data-f="only">Okta only</button>
    <button class="pill" data-f="multi">Multiple IAMs</button>
  </div>

  <div class="card">
    <table class="flat-table">
      <thead><tr>
        <th data-k="account">Account</th>
        <th data-k="iams">IAM</th>
        <th data-k="cat">External Catalog</th>
        <th data-k="region">Region</th>
        <th data-k="csm">CSM</th>
        <th data-k="arr_raw" class="num">ARR</th>
      </tr></thead>
      <tbody id="rows"></tbody>
    </table>
    <div class="empty-msg" id="empty" style="display:none">No accounts match.</div>
  </div>
</div>

<script>
const ROWS = {{ROWS}};
const DASH = "\\u2014";
let filt = "all", sortK = "account", sortDir = 1;

const esc = s => String(s).replace(/[&<>"]/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;","\\"":"&quot;"}[c]));
const tags = (xs, cls) => xs.length ? xs.map(x => `<span class="tag ${cls||""}">${esc(x)}</span>`).join("") : `<span class="muted">${DASH}</span>`;
const sortVal = (r, k) => k === "arr_raw" ? (r.arr_raw ?? -1)
  : k === "iams" ? r.other_iams.length + r.other_iams.join(" ").toLowerCase()
  : k === "cat" ? r.ext_catalog.join(" ").toLowerCase()
  : String(r[k] || "").toLowerCase();

function render() {
  const q = document.getElementById("search").value.trim().toLowerCase();
  const out = ROWS.filter(r => {
    if (filt === "only"  && r.other_iams.length)  return false;
    if (filt === "multi" && !r.other_iams.length) return false;
    if (!q) return true;
    return [r.account, r.region, r.csm, ...r.other_iams, ...r.ext_catalog].join(" ").toLowerCase().includes(q);
  }).sort((a, b) => {
    const x = sortVal(a, sortK), y = sortVal(b, sortK);
    return (x < y ? -1 : x > y ? 1 : 0) * sortDir;
  });

  document.getElementById("rows").innerHTML = out.map(r => `<tr>
    <td>${r.url ? `<a class="acct-link" href="${esc(r.url)}" target="_blank" rel="noopener">${esc(r.account)}</a>` : `<span class="acct-link">${esc(r.account)}</span>`}</td>
    <td>${tags(["Okta"], "okta")}${r.other_iams.map(x => `<span class="tag">${esc(x)}</span>`).join("")}</td>
    <td>${tags(r.ext_catalog)}</td>
    <td>${r.region ? esc(r.region) : `<span class="muted">${DASH}</span>`}</td>
    <td>${r.csm ? esc(r.csm) : `<span class="muted">${DASH}</span>`}</td>
    <td class="num arr">${esc(r.arr)}</td>
  </tr>`).join("");
  document.getElementById("empty").style.display = out.length ? "none" : "block";
  document.getElementById("count-badge").textContent = `${out.length} of ${ROWS.length} accounts`;
  document.querySelectorAll("th[data-k]").forEach(th => {
    th.classList.toggle("sort-asc",  th.dataset.k === sortK && sortDir === 1);
    th.classList.toggle("sort-desc", th.dataset.k === sortK && sortDir === -1);
  });
}

document.getElementById("search").addEventListener("input", render);
document.querySelectorAll(".pill").forEach(p => p.addEventListener("click", () => {
  document.querySelectorAll(".pill").forEach(x => x.classList.remove("active"));
  p.classList.add("active"); filt = p.dataset.f; render();
}));
document.querySelectorAll("th[data-k]").forEach(th => th.addEventListener("click", () => {
  const k = th.dataset.k;
  if (sortK === k) sortDir = -sortDir; else { sortK = k; sortDir = k === "arr_raw" ? -1 : 1; }
  render();
}));
render();
</script>
</body>
</html>'

logo_path <- paste0(
  'd="M95.968 3.99802H99.8179V11.656C99.8179 16.8192 96.57 20.0561 92.1339 20.0561C87.6978 20.0561 84.5925 16.8192 84.5925 11.656V3.99802H88.4424V11.656C88.4424 14.7508 89.7733 16.5192 92.1339 16.5192C94.4946 16.5192 95.9838 14.7508 95.9838 11.656V3.99802H95.968ZM40.675 15.635L35.0507 3.99802H30.7571V19.7561H34.6071V11.7981L38.457 19.8982H42.7505L46.6004 11.7981V19.7561H50.4503V3.99802H46.1568L40.6909 15.635H40.675ZM77.4789 3.99802H73.1854L67.7195 15.4929L62.0951 3.99802H57.8016V19.7561H61.6515V11.7981L65.5014 19.8982H69.7949L73.6448 11.7981V19.7561H77.4947V3.99802H77.4789ZM120.509 3.99802H105.426V7.53491H111.051V19.7561H114.901V7.53491H120.525V3.99802H120.509ZM141.2 19.8982H137.065L136.036 17.3877H128.352L127.322 19.8982H123.329L130.126 4.14013H134.42L141.216 19.8982H141.2ZM134.689 14.3087L132.17 8.41913L129.651 14.3087H134.673H134.689ZM24.1188 0.161133V23.7351H0.464844V0.161133H24.1188ZM19.0965 3.99802H15.2466V19.7561H19.0965V3.99802Z"')

# Literal substitution: gsub() would still interpret backslashes in `val`.
fill <- function(tmpl, key, val) {
  paste(strsplit(tmpl, paste0("{{", key, "}}"), fixed = TRUE)[[1]], collapse = as.character(val))
}
html <- html_tmpl
html <- fill(html, "LOGO_PATH", logo_path)
html <- fill(html, "UPDATED",   html_escape(paste0("Snapshot ", format(snap_date, "%d %b %Y"))))
html <- fill(html, "SNAP_FILE", html_escape(snap_file))
html <- fill(html, "N_TOTAL",   nrow(tbl))
html <- fill(html, "N_ONLY",    nrow(tbl) - n_multi)
html <- fill(html, "N_MULTI",   n_multi)
html <- fill(html, "TOTAL_ARR", fmt_usd(total_arr))
html <- fill(html, "ROWS",      rows_json)   # last: the JSON could contain "{{"

html_path <- "output/okta_iam_accounts.html"
writeLines(html, html_path, useBytes = FALSE)
message("Wrote ", html_path)
