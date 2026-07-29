# patch_consumption_tab.R
# Run once from the project root to add the Consumption tab to the HTML report.
# Source this file, then re-run main.R.

message("Patching R/report_all_accounts_html.R ...")

r_file <- "R/report_all_accounts_html.R"
if (!file.exists(r_file)) stop("Run from the project root — ", r_file, " not found.")

src <- paste(readLines(r_file, warn = FALSE), collapse = "\n")

# ── Helpers ──────────────────────────────────────────────────────────────────
patch <- function(src, old, new, label) {
  if (!grepl(old, src, fixed = TRUE)) {
    message("  SKIP (already applied or not found): ", label)
    return(src)
  }
  message("  OK: ", label)
  sub(old, new, src, fixed = TRUE)
}

# ── 1. CSS ────────────────────────────────────────────────────────────────────
src <- patch(src,
  "  #page-renewals.active,\n  #page-expansions.active,\n  #page-changes.active { flex-direction: column; }",
  "  #page-renewals.active,\n  #page-expansions.active,\n  #page-changes.active,\n  #page-consumption.active { flex-direction: column; }\n\n  /* ── Consumption page ───────────────────────────────────────────────────────────── */\n  .consumption-page { flex: 1; display: flex; flex-direction: column; min-height: 0; }\n  .consumption-sub-tabs {\n    background: var(--im-white); border-bottom: 1px solid var(--border);\n    padding: 0 24px; display: flex; flex-shrink: 0;\n  }\n  .csub-btn {\n    padding: 10px 18px; border: none; background: none; font-size: 13px;\n    font-weight: 500; font-family: var(--font); color: var(--text-3);\n    cursor: pointer; border-bottom: 2px solid transparent; margin-bottom: -1px;\n    transition: color .12s; white-space: nowrap;\n    display: flex; align-items: center; gap: 7px;\n  }\n  .csub-btn:hover { color: var(--text-1); }\n  .csub-btn.active { color: var(--text-1); border-bottom-color: var(--im-lime); font-weight: 600; }\n  .csub-count {\n    font-size: 10.5px; font-weight: 700; background: var(--surface-2);\n    color: var(--text-3); padding: 1px 6px; border-radius: 9px;\n  }\n  .csub-btn.active .csub-count { background: var(--im-lime); color: var(--im-black); }\n  .consumption-panel { display: none; flex: 1; flex-direction: column; min-height: 0; }\n  .consumption-panel.active { display: flex; }\n  .con-stats {\n    padding: 14px 24px; display: flex; gap: 32px; border-bottom: 1px solid var(--border);\n    background: var(--surface); flex-wrap: wrap; flex-shrink: 0;\n  }\n  .pct-high { color: #166534; font-weight: 700; }\n  .pct-med  { color: #92400E; font-weight: 700; }\n  .pct-low  { color: #991B1B; font-weight: 700; }\n  .pct-none { color: var(--text-3); }\n  .flat-table tr.low-con td { background: #FEF3C7; }\n  .flat-table tr.low-con:hover td { background: #FDE68A; }",
  "CSS: page-consumption.active + consumption styles"
)

# ── 2. Tab button ─────────────────────────────────────────────────────────────
src <- patch(src,
  "  <button class=\"page-tab\" data-page=\"changes\" onclick=\"switchPage(\\'changes\\')\">
    Changes <span class=\"page-tab-count\" id=\"tab-count-changes\">0</span>
  </button>
</div>",
  "  <button class=\"page-tab\" data-page=\"changes\" onclick=\"switchPage(\\'changes\\')\">
    Changes <span class=\"page-tab-count\" id=\"tab-count-changes\">0</span>
  </button>
  <button class=\"page-tab\" data-page=\"consumption\" onclick=\"switchPage(\\'consumption\\')\">
    Consumption <span class=\"page-tab-count\" id=\"tab-count-consumption\">—</span>
  </button>
</div>",
  "Tab button: Consumption"
)

# ── 3. HTML panel ─────────────────────────────────────────────────────────────
src <- patch(src,
  "<div id=\"page-changes\" class=\"page\">\n  <div class=\"flat-page\">\n    <div class=\"changes-header\" id=\"changes-header\"></div>\n    <div class=\"changes-body\"   id=\"changes-content\"></div>\n  </div>\n</div>\n\n<script>",
  "<div id=\"page-changes\" class=\"page\">\n  <div class=\"flat-page\">\n    <div class=\"changes-header\" id=\"changes-header\"></div>\n    <div class=\"changes-body\"   id=\"changes-content\"></div>\n  </div>\n</div>\n\n<!-- CONSUMPTION PAGE -->\n<div id=\"page-consumption\" class=\"page\">\n  <div class=\"consumption-page\">\n    <div class=\"consumption-sub-tabs\">\n      <button class=\"csub-btn active\" id=\"csub-renewals\" onclick=\"switchConsumptionTab(\\'renewals\\')\">Renewals <span class=\"csub-count\" id=\"csub-count-renewals\">0</span></button>\n      <button class=\"csub-btn\" id=\"csub-low\" onclick=\"switchConsumptionTab(\\'low\\')\">Low Consumption <span class=\"csub-count\" id=\"csub-count-low\">0</span></button>\n      <button class=\"csub-btn\" id=\"csub-mapping\" onclick=\"switchConsumptionTab(\\'mapping\\')\">By Connector</button>\n    </div>\n    <div class=\"consumption-panel active\" id=\"cpanel-renewals\"><div class=\"con-stats\" id=\"con-stats-renewals\"></div><div class=\"flat-table-wrap\" id=\"con-table-renewals\"></div></div>\n    <div class=\"consumption-panel\" id=\"cpanel-low\"><div class=\"con-stats\" id=\"con-stats-low\"></div><div class=\"flat-table-wrap\" id=\"con-table-low\"></div></div>\n    <div class=\"consumption-panel\" id=\"cpanel-mapping\"><div class=\"flat-table-wrap\" id=\"con-table-mapping\"></div></div>\n  </div>\n</div>\n\n<script>",
  "HTML: page-consumption panel"
)

# ── 4. JS constants ───────────────────────────────────────────────────────────
src <- patch(src,
  "const ACCOUNTS      = {{JSON_DATA}};\nconst SNAP_DATES    = {{SNAP_DATES}};\nconst CHANGES_DATA  = {{CHANGES}};",
  "const ACCOUNTS          = {{JSON_DATA}};\nconst SNAP_DATES        = {{SNAP_DATES}};\nconst CHANGES_DATA      = {{CHANGES}};\nconst CONSUMPTION_DATA  = {{CONSUMPTION}};",
  "JS: CONSUMPTION_DATA constant"
)

# ── 5. Sort state ─────────────────────────────────────────────────────────────
src <- patch(src,
  "let renewalSort     = { col: \"end_date_raw\", dir: 1 };\nlet expansionSort   = { col: \"close_date_raw\", dir: 1 };",
  "let renewalSort         = { col: \"end_date_raw\", dir: 1 };\nlet expansionSort       = { col: \"close_date_raw\", dir: 1 };\nlet conRenewalSort      = { col: \"Date\", dir: 1 };\nlet conLowSort          = { col: \"License_Consumption_num\", dir: 1 };\nlet conMappingSort      = { col: \"sum_arr\", dir: -1 };\nlet activeConTab        = \"renewals\";",
  "JS: consumption sort state"
)

# ── 6. Render dispatcher + all consumption JS ─────────────────────────────────
old_render <- 'function render() {
  if      (activePage === "accounts")   renderAccounts();
  else if (activePage === "renewals")   renderRenewals();
  else if (activePage === "expansions") renderExpansions();
  else if (activePage === "changes")    renderChanges();
}'

new_render <- 'function render() {
  if      (activePage === "accounts")    renderAccounts();
  else if (activePage === "renewals")    renderRenewals();
  else if (activePage === "expansions")  renderExpansions();
  else if (activePage === "changes")     renderChanges();
  else if (activePage === "consumption") renderConsumption();
}

// ── Consumption ───────────────────────────────────────────────────────────────────────
function switchConsumptionTab(tab) {
  activeConTab = tab;
  document.querySelectorAll(".csub-btn").forEach(b => b.classList.toggle("active", b.id === "csub-" + tab));
  document.querySelectorAll(".consumption-panel").forEach(p => p.classList.toggle("active", p.id === "cpanel-" + tab));
}
function sortConRenewals(col) { conRenewalSort.dir = conRenewalSort.col===col?-conRenewalSort.dir:1; conRenewalSort.col=col; renderConsumption(); }
function sortConLow(col)      { conLowSort.dir     = conLowSort.col===col?-conLowSort.dir:1;         conLowSort.col=col;     renderConsumption(); }
function sortConMapping(col)  { conMappingSort.dir = conMappingSort.col===col?-conMappingSort.dir:1; conMappingSort.col=col; renderConsumption(); }

function fmtPct(pctStr) {
  if (!pctStr||pctStr==="NA"||pctStr==="—") return \'<span class="pct-none">—</span>\';
  const n = parseFloat(pctStr);
  const cls = isNaN(n)?"pct-none":n>=80?"pct-high":n>=20?"pct-med":"pct-low";
  return \'<span class="\' + cls + \'">\' + esc(pctStr) + \'</span>\';
}

function renderConsumption() {
  const noData = \'<p class="empty-msg" style="padding:40px">No consumption data — run op_list_consumption() and regenerate the report.</p>\';
  if (!CONSUMPTION_DATA) {
    ["con-table-renewals","con-table-low","con-table-mapping"].forEach(id => document.getElementById(id).innerHTML = noData);
    return;
  }
  const renewals  = CONSUMPTION_DATA.renewals        || [];
  const low       = CONSUMPTION_DATA.low_consumption || [];
  const byMapping = (CONSUMPTION_DATA.stats&&CONSUMPTION_DATA.stats.by_mapping)||[];
  document.getElementById("csub-count-renewals").textContent   = renewals.length;
  document.getElementById("csub-count-low").textContent        = low.length;
  document.getElementById("tab-count-consumption").textContent = renewals.length;
  const cols = [
    {label:"Customer",sort:"Customer"},{label:"Renewal Date",sort:"Date"},{label:"ARR",sort:"Amount_raw"},
    {label:"SaaS",sort:"SaaS"},{label:"Data Sources",sort:"Data_Sources_raw"},{label:"Users",sort:"Users_raw"},
    {label:"Consumption",sort:"License_Consumption_num"},{label:"Policies",sort:"Policies_raw"},
    {label:"Queries 31d",sort:"q31_raw"},{label:"Queries 90d",sort:"q90_raw"},{label:"Cases",sort:"Support_Cases_raw"},
  ];
  function csort(rows,s) {
    return rows.slice().sort((a,b)=>{
      let av=a[s.col],bv=b[s.col];
      if(s.col.endsWith("_raw")||s.col.endsWith("_num")){av=parseFloat(av)||0;bv=parseFloat(bv)||0;return(av-bv)*s.dir;}
      return String(av||"").localeCompare(String(bv||""))*s.dir;
    });
  }
  function buildTable(cid,rows,st,fn,isLow) {
    if(!rows.length){document.getElementById(cid).innerHTML=\'<p class="empty-msg" style="padding:24px">No data.</p>\';return;}
    const hdrs=cols.map(c=>{const a=st.col===c.sort?(st.dir===1?" ↑":" ↓"):"";return\'<th onclick="\'+fn+\'(\\\'\'+c.sort+\'\\\')" style="cursor:pointer">\'+c.label+a+\'</th>\';}).join("");
    const brs=csort(rows,st).map(r=>{
      const n=parseFloat(r.License_Consumption_num),lo=isLow||(!isNaN(n)&&n<20);
      return\'<tr\'+(lo?\' class="low-con"\':\'\')+\'><td style="white-space:nowrap;font-weight:600">\'+esc(r.Customer)+\'</td><td style="white-space:nowrap">\'+esc(r.Date)+\'</td><td>\'+( r.Amount_raw?"$"+Number(r.Amount_raw).toLocaleString("en-US",{maximumFractionDigits:0}):"--")+\'</td><td>\'+(r.SaaS===true||r.SaaS==="TRUE"?\'<span class="tag tag-saas">SaaS</span>\':\'<span class="tag tag-region">Self-Mgd</span>\')+\'</td><td style="text-align:right">\'+(r.Data_Sources!=null?Number(r.Data_Sources).toLocaleString():"--")+\'</td><td style="text-align:right">\'+(r.Users!=null?Number(r.Users).toLocaleString():"--")+\'</td><td style="text-align:right">\'+fmtPct(r.License_Consumption)+\'</td><td style="text-align:right">\'+(r.Policies!=null?Number(r.Policies).toLocaleString():"--")+\'</td><td style="text-align:right">\'+(r.Users_With_Queries_Last_31_Days!=null?Number(r.Users_With_Queries_Last_31_Days).toLocaleString():"--")+\'</td><td style="text-align:right">\'+(r.Users_With_Queries_Last_90_Days!=null?Number(r.Users_With_Queries_Last_90_Days).toLocaleString():"--")+\'</td><td style="text-align:right">\'+(r.Support_Cases!=null?r.Support_Cases:"--")+\'</td></tr>\';
    }).join("");
    document.getElementById(cid).innerHTML=\'<table class="flat-table"><thead><tr>\'+hdrs+\'</tr></thead><tbody>\'+brs+\'</tbody></table>\';
  }
  const tot=renewals.reduce((s,r)=>s+(parseFloat(r.Amount_raw)||0),0);
  const wp=renewals.filter(r=>r.License_Consumption_num!=null&&!isNaN(parseFloat(r.License_Consumption_num)));
  const avg=wp.length?wp.reduce((s,r)=>s+parseFloat(r.License_Consumption_num),0)/wp.length:null;
  document.getElementById("con-stats-renewals").innerHTML=\'<div class="stat-item"><div class="stat-label">Customers</div><div class="stat-value">\'+renewals.length+\'</div></div><div class="stat-item"><div class="stat-label">Total ARR</div><div class="stat-value">$\'+Math.round(tot/1e6*10)/10+\'M</div></div><div class="stat-item"><div class="stat-label">Avg Consumption</div><div class="stat-value">\'+( avg!=null?Math.round(avg)+"%":"--")+\'</div></div><div class="stat-item"><div class="stat-label">Low (&lt;20%)</div><div class="stat-value" style="color:#991B1B">\'+low.length+\'</div></div>\';
  document.getElementById("con-stats-low").innerHTML=\'<div class="stat-item"><div class="stat-label">Customers</div><div class="stat-value" style="color:#991B1B">\'+low.length+\'</div></div><div class="stat-item"><div class="stat-label">ARR at Risk</div><div class="stat-value" style="color:#991B1B">$\'+Math.round(low.reduce((s,r)=>s+(parseFloat(r.Amount_raw)||0),0)/1e6*10)/10+\'M</div></div><div class="stat-item"><div class="stat-label">Threshold</div><div class="stat-value">&lt;20%</div></div>\';
  buildTable("con-table-renewals",renewals,conRenewalSort,"sortConRenewals",false);
  buildTable("con-table-low",low,conLowSort,"sortConLow",true);
  if(!byMapping.length){document.getElementById("con-table-mapping").innerHTML=\'<p class="empty-msg" style="padding:24px">No data.</p>\';}
  else{
    const mc=[{label:"Connector / Product",sort:"Mo_Mapping"},{label:"# Contracts",sort:"n"},{label:"Total ARR",sort:"sum_arr"},{label:"Total Billed",sort:"sum_total"}];
    const ms=byMapping.slice().sort((a,b)=>{const av=parseFloat(a[conMappingSort.col])||0,bv=parseFloat(b[conMappingSort.col])||0;if(!isNaN(av)&&!isNaN(bv))return(av-bv)*conMappingSort.dir;return String(a[conMappingSort.col]||"").localeCompare(String(b[conMappingSort.col]||""))*conMappingSort.dir;});
    const mh=mc.map(c=>{const a=conMappingSort.col===c.sort?(conMappingSort.dir===1?" ↑":" ↓"):"";return\'<th onclick="sortConMapping(\\\'\'+c.sort+\'\\\')" style="cursor:pointer">\'+c.label+a+\'</th>\';}).join("");
    const mb2=ms.map(r=>\'<tr><td style="font-weight:600">\'+esc(r.Mo_Mapping)+\'</td><td style="text-align:right">\'+r.n+\'</td><td style="text-align:right">$\'+Number(r.sum_arr||0).toLocaleString("en-US",{maximumFractionDigits:0})+\'</td><td style="text-align:right">$\'+Number(r.sum_total||0).toLocaleString("en-US",{maximumFractionDigits:0})+\'</td></tr>\').join("");
    document.getElementById("con-table-mapping").innerHTML=\'<table class="flat-table"><thead><tr>\'+mh+\'</tr></thead><tbody>\'+mb2+\'</tbody></table>\';
  }
}'

src <- patch(src, old_render, new_render, "JS: render dispatcher + renderConsumption()")

# ── 7. R gsub injection ───────────────────────────────────────────────────────
src <- patch(src,
  "  tmpl <- gsub(\"{{CHANGES}}\",     changes_json,          tmpl, fixed = TRUE)\n  tmpl <- gsub(\"{{JSON_DATA}}\",   json_data,             tmpl, fixed = TRUE)",
  "  tmpl <- gsub(\"{{CHANGES}}\",      changes_json,          tmpl, fixed = TRUE)\n  tmpl <- gsub(\"{{CONSUMPTION}}\",  \"null\",                tmpl, fixed = TRUE)\n  tmpl <- gsub(\"{{JSON_DATA}}\",    json_data,             tmpl, fixed = TRUE)",
  "R: gsub {{CONSUMPTION}} injection"
)

# ── Write back ────────────────────────────────────────────────────────────────
writeLines(src, r_file)
message("Done. Now run devtools::load_all() and re-run main.R.")
