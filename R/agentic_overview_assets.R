# Agentic Engagement Overview: the CSS, panel markup and JavaScript shared by
# the dashboard's Adoption > Agentic Engagement Overview tab
# (report_all_accounts_html()) and the standalone agentic_owner_summary.html
# (report_agentic_owner_html()). Edit here; both pages pick it up.
#
# The JS expects these globals from its host page: AGENTIC_DATA (the payload
# from .agx_json()), ACCOUNTS (fallback rows, may be empty), activeRegion,
# activeCSM, searchTerm, and the helpers esc(), adoptBlank(), covNum(),
# ratingColour(), setCount(), toggleCard(), goToAccount(), setCSM(),
# switchPage() / PAGES (CS Coverage only). The host also needs an element
# with id "count-badge". AGX_STANDALONE = true hides the "Account card" link.
# Plain strings, filled into the host template with .ao_fill() (literal).

.AGX_CSS <- r"---(
  /* ── Agentic Engagement Overview (Adoption sub-tab, id "agentic") ────────── */
  .agx-tabs { margin: 0 0 14px; }
  .agx-tabs { align-items: center; }
  .agx-tabs .agx-ext { margin-left: auto; margin-bottom: 6px; }
  .agx-tabs .tab-btn .n { display: inline-block; margin-left: 6px; font-size: 10.5px; font-weight: 700; padding: 1px 7px;
                          border-radius: 9px; background: var(--surface-2); color: var(--text-3); }
  .agx-tabs .tab-btn.active .n { background: var(--im-lime); color: var(--im-black); }
  .agx-tabs .tab-btn.churn .n { background: var(--red-lt); color: #991B1B; }
  .agx-rm-table th { cursor: pointer; user-select: none; white-space: nowrap; }
  .agx-rm-table th:hover { color: var(--text-1); }
  /* Built from report 00OVV000007LHjN2AW (op_list$agentic_profile) by the same
     rules as agentic_owner_summary.html: KPIs, the Coverage funnel and the
     Conversation Status chart, evaluation / deployment status, then one
     expandable card per account. */
  .agx { padding: 14px 24px 40px; }
  .agx-note { font-size: 12.5px; color: #92400E; background: var(--amber-lt); border: 1px solid #FCD34D;
              border-radius: var(--radius-sm); padding: 8px 12px; margin-bottom: 12px; }
  .agx-filters { display: flex; flex-wrap: wrap; gap: 8px; align-items: center; margin-bottom: 14px; }
  .agx-chip { font: inherit; font-size: 12.5px; padding: 5px 12px; border-radius: 999px; border: 1px solid var(--border);
              background: var(--im-white); color: var(--text-2); cursor: pointer; white-space: nowrap; }
  .agx-chip:hover { border-color: var(--text-3); color: var(--text-1); }
  .agx-chip.on { background: var(--im-black); color: var(--im-lime); border-color: var(--im-black); }
  .agx-chip.reset { border-style: dashed; }
  .agx-chip .n { margin-left: 6px; font-size: 11px; font-weight: 700; color: #B91C1C; }
  .agx-chip.on .n { color: var(--im-lime); }
  .agx-active { font-size: 12px; color: var(--text-2); margin-left: auto; }
  .agx-kpis { display: grid; grid-template-columns: repeat(6, minmax(0, 1fr)); gap: 12px; margin-bottom: 14px; }
  /* Change since the comparison snapshot (changes_detail$agentic) */
  .agx-delta { display: inline-block; font-size: 11px; font-weight: 700; margin-left: 6px; padding: 0 6px;
               border-radius: 999px; vertical-align: 2px; font-variant-numeric: tabular-nums; white-space: nowrap; }
  .agx-delta.up   { background: #DCFCE7; color: #166534; }
  .agx-delta.down { background: var(--red-lt); color: #991B1B; }
  .agx-delta.neutral { background: var(--surface-2); color: var(--text-2); }
  .agx-kpi .v .agx-delta { font-size: 12px; vertical-align: 6px; }
  .agx-since { font-size: 12px; color: var(--text-2); margin: -4px 0 12px; }
  .agx-since b { color: var(--text-1); }
  .agx-chg td { vertical-align: top; }
  .agx-chg .old { color: #991B1B; text-decoration: line-through; }
  .agx-chg .new { color: #166534; font-weight: 600; }
  /* Agentic PoV projects (op_list$agentic_pov) */
  .agx-pov-table tr.click { cursor: pointer; }
  .agx-pov-table td { vertical-align: top; }
  .agx-pov-notes { max-width: 460px; color: var(--text-2); display: -webkit-box; -webkit-line-clamp: 2;
                   -webkit-box-orient: vertical; overflow: hidden; }
  .agx-pov-block { border: 1px solid var(--border); border-left: 3px solid #7C3AED; border-radius: var(--radius-sm);
                   padding: 10px 12px; margin-top: 12px; background: var(--im-white); }
  .agx-pov-block .h { display: flex; gap: 8px; align-items: center; flex-wrap: wrap; font-weight: 600; font-size: 13px; }
  .agx-pov-block .m { font-size: 12px; color: var(--text-2); margin-top: 2px; }
  .agx-pov-block .n { white-space: pre-wrap; font-size: 13px; line-height: 1.5; margin-top: 6px; }
  .agx-kpi { background: var(--im-white); border: 1px solid var(--border); border-radius: var(--radius); padding: 12px 16px; }
  .agx-kpi .l { font-size: 10.5px; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; color: var(--text-3); }
  .agx-kpi .v { font-size: 26px; font-weight: 800; letter-spacing: -.02em; margin-top: 2px; font-variant-numeric: tabular-nums; }
  .agx-kpi .v small { font-size: 14px; font-weight: 600; color: var(--text-2); margin-left: 6px; }
  .agx-kpi .s { font-size: 12px; color: var(--text-3); }
  .agx-grid2 { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); gap: 12px; margin-bottom: 12px; }
  .agx-card { background: var(--im-white); border: 1px solid var(--border); border-radius: var(--radius); padding: 14px 18px; }
  .agx-card h3 { font-size: 14.5px; font-weight: 700; margin: 0 0 2px; }
  .agx-sub { font-size: 12px; color: var(--text-3); margin-bottom: 10px; }
  .agx-fun { display: grid; grid-template-columns: 170px minmax(0, 1fr) 118px; gap: 12px; align-items: center; margin: 9px 0; }
  .agx-fun-l { font-size: 13px; font-weight: 600; }
  .agx-fun-l span { display: block; font-size: 11px; font-weight: 400; color: var(--text-3); }
  .agx-track { height: 20px; background: var(--surface-2); border-radius: 5px; overflow: hidden; }
  .agx-fill { height: 100%; border-radius: 5px; background: var(--im-black); transition: width .25s; }
  .agx-fill.lime { background: var(--im-lime-dk); }
  .agx-fill.held { background: #16A34A; }
  .agx-fun-n { font-size: 12.5px; text-align: right; font-variant-numeric: tabular-nums; }
  .agx-fun-n b { font-size: 14px; }
  .agx-st { display: grid; grid-template-columns: 180px minmax(0, 1fr) 76px; gap: 10px; align-items: center;
            padding: 4px 6px; border-radius: var(--radius-sm); cursor: pointer; }
  .agx-st:hover { background: var(--im-lime-lt); }
  .agx-st.on { background: var(--im-lime-lt); outline: 1px solid var(--im-lime-dk); }
  .agx-st .agx-track { height: 14px; }
  .agx-st-name { font-size: 12.5px; display: flex; align-items: center; gap: 8px; min-width: 0; }
  .agx-st-name em { font-style: normal; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .agx-st-n { font-size: 12.5px; font-weight: 600; text-align: right; font-variant-numeric: tabular-nums; }
  .agx-sw { width: 10px; height: 10px; border-radius: 3px; flex: none; display: inline-block; }
  .agx-legend { display: flex; flex-wrap: wrap; gap: 6px 14px; font-size: 11px; color: var(--text-3); margin-top: 8px; }
  .agx-legend span { display: inline-flex; align-items: center; gap: 5px; }
  .agx-split { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); gap: 24px; }
  .agx-split h4 { font-size: 11px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: var(--text-3); margin: 0 0 6px; }
  .agx-empty { padding: 20px; text-align: center; color: var(--text-3); font-size: 12.5px; }
  .agx-head { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 18px 0 8px; }
  .agx-head h3 { font-size: 15px; font-weight: 700; margin: 0; }
  .agx-head .agx-tools { margin-left: auto; display: flex; gap: 6px; align-items: center; }
  .agx-head select, .agx-head button { font: inherit; font-size: 12.5px; padding: 5px 10px; border: 1px solid var(--border);
                                       border-radius: var(--radius-sm); background: var(--im-white); color: var(--text-1); cursor: pointer; }
  .agx-facets { display: flex; flex-wrap: wrap; gap: 6px; align-items: center; margin-bottom: 10px; }
  .agx-facets .lbl { font-size: 10.5px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: var(--text-3); margin-right: 2px; }
  .agx-mode { display: inline-flex; border: 1px solid var(--border); border-radius: 999px; overflow: hidden; margin-left: 4px; }
  .agx-mode button { font: inherit; font-size: 11.5px; padding: 4px 10px; border: 0; background: var(--im-white); color: var(--text-2); cursor: pointer; }
  .agx-mode button.on { background: var(--im-black); color: #fff; }
  /* Account card header: name + owner on the left, statuses on the right. */
  .agx-acct .card-header { gap: 12px; padding: 12px 18px; }
  .agx-acct .agx-who { flex: 1; min-width: 0; }
  .agx-acct .agx-who .card-name { display: block; }
  .agx-who-sub { font-size: 12px; color: var(--text-3); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .agx-who-sub b { color: var(--text-2); font-weight: 600; }
  .agx-pills { display: flex; flex-wrap: wrap; gap: 6px; justify-content: flex-end; flex: 0 1 auto; }
  .agx-pill { display: inline-flex; align-items: center; gap: 6px; font-size: 11.5px; font-weight: 600; padding: 3px 9px;
              border-radius: 999px; white-space: nowrap; background: var(--surface-2); color: var(--text-1); max-width: 220px; }
  .agx-pill em { font-style: normal; overflow: hidden; text-overflow: ellipsis; }
  .agx-pill .k { font-size: 9.5px; font-weight: 700; letter-spacing: .05em; text-transform: uppercase; color: var(--text-3); }
  .agx-pill.miss { background: var(--red-lt); color: #991B1B; }
  .agx-done { font-size: 11.5px; font-weight: 700; min-width: 42px; text-align: center; padding: 2px 8px; border-radius: 10px; }
  .agx-done.full { background: var(--im-lime-lt); color: #4B5E00; }
  .agx-done.part { background: var(--amber-lt); color: #92400E; }
  .agx-done.none { background: var(--red-lt); color: #991B1B; }
  .agx-dot { display: inline-block; width: 9px; height: 9px; border-radius: 50%; border: 1px solid rgba(0,0,0,.15);
             margin-right: 7px; vertical-align: 0; }
  .agx-body { padding-top: 14px; }
  .agx-links { display: flex; gap: 8px; margin-bottom: 12px; }
  .agx-fields { display: grid; grid-template-columns: repeat(auto-fill, minmax(190px, 1fr)); gap: 14px 18px; margin-bottom: 14px; }
  .agx-texts { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; }
  .agx-text { background: var(--surface); border: 1px solid var(--border); border-radius: var(--radius-sm); padding: 10px 12px; }
  .agx-text .field-value { white-space: pre-wrap; line-height: 1.5; font-weight: 400; font-size: 13px; }
  .agx-gap { color: #B91C1C; font-weight: 600; font-size: 12.5px; }
  .agx-tbd { color: #92400E; font-weight: 600; font-size: 12.5px; }
  .agx-hint { display: block; font-size: 11.5px; color: var(--text-3); font-weight: 400; }
  .agx-risk { display: inline-block; font-size: 11.5px; font-weight: 600; padding: 2px 8px; border-radius: 999px; background: var(--red-lt); color: #991B1B; }
  @media (max-width: 1200px) { .agx-kpis { grid-template-columns: repeat(3, minmax(0, 1fr)); } .agx-texts { grid-template-columns: 1fr; } }
  @media (max-width: 900px)  { .agx-grid2, .agx-split { grid-template-columns: 1fr; } .agx-kpis { grid-template-columns: repeat(2, minmax(0, 1fr)); }
                               .agx-fun { grid-template-columns: 120px minmax(0, 1fr) 96px; } .agx-pills { display: none; } }
  @media (max-width: 600px)  { .agx-fun { grid-template-columns: minmax(0, 1fr) auto; gap: 4px 10px; }
                               .agx-fun-l { grid-column: 1 / -1; } .agx-st { grid-template-columns: 120px minmax(0, 1fr) 32px; } }
  /* CS Coverage: agentic profile by CSM (moved here from the Agentic tab). */
  .cov-ag { margin-bottom: 14px; }
  .cov-ag .card-body { padding: 0 18px 10px; }
  .cov-ag-table tr.click { cursor: pointer; }
  .cov-ag-table tr.click:hover td { background: var(--im-lime-lt); }
  .cov-ag-mini { display: flex; align-items: center; gap: 8px; min-width: 120px; }
  .cov-ag-mini .agx-track { height: 8px; flex: 1; }
  .cov-ag-mini b { font-size: 12px; font-weight: 600; width: 34px; text-align: right; font-variant-numeric: tabular-nums; }
)---"

.AGX_PANEL <- r"---(
      <div class="agx">
        <div id="agx-note"></div>
        <!-- Same two tabs as agentic_owner_summary.html: accounts counted, and
             the accounts left off because a renewal is at a churn Account Risk. -->
        <div class="tabs agx-tabs">
          <button class="tab-btn active" data-agxtab="cov" onclick="agxSetTab(&quot;cov&quot;)">Coverage<span class="n" id="agx-n-cov"></span></button>
          <button class="tab-btn churn" data-agxtab="churn" onclick="agxSetTab(&quot;churn&quot;)">Removed for churn<span class="n" id="agx-n-churn"></span></button>
          <!-- Same button style as Customer Central on the account cards. -->
          <a class="cc-link agx-ext" href="https://app.dust.tt/share/frame/96ae59ad-4e70-40f2-aa55-3c8b086831e9"
             target="_blank" rel="noopener">FCTO Agentic POC Dust Frame &#8599;</a>
        </div>
        <div id="agx-tab-cov">
        <div class="agx-filters" id="agx-filters"></div>
        <div class="agx-kpis" id="agx-kpis"></div>
        <div id="agx-since"></div>
        <div class="agx-grid2">
          <div class="agx-card">
            <h3>Coverage</h3>
            <div class="agx-sub">Share of accounts in view with each piece in Salesforce</div>
            <div id="agx-funnel"></div>
          </div>
          <div class="agx-card">
            <h3>Where the conversation stands</h3>
            <div class="agx-sub">Conversation Status &middot; click a bar to filter</div>
            <div id="agx-status"></div>
            <div class="agx-legend" id="agx-legend"></div>
          </div>
        </div>
        <div class="agx-card" id="agx-evdep-card">
          <h3>Evaluation and deployment</h3>
          <div class="agx-sub">Evaluation Status and Deployment Status &middot; click a bar to filter</div>
          <div class="agx-split">
            <div><h4>Evaluation status</h4><div id="agx-eval"></div></div>
            <div><h4>Deployment status</h4><div id="agx-deploy"></div></div>
          </div>
        </div>
        <!-- Agentic proof-of-value projects (report 00OVV000007QLj72AG, op_list$agentic_pov). -->
        <div class="agx-card" id="agx-pov-card" style="margin-top:12px">
          <h3 id="agx-pov-h">Agentic PoVs</h3>
          <div class="agx-sub" id="agx-pov-sub"></div>
          <div style="overflow-x:auto" id="agx-pov"></div>
        </div>
        <!-- What changed since the comparison snapshot (changes_detail$agentic). -->
        <div class="card" id="agx-chg-card" style="margin-top:12px">
          <div class="card-header" onclick="toggleCard(this)">
            <div class="card-name" id="agx-chg-h">What changed</div>
            <span class="agx-who-sub" id="agx-chg-sub"></span>
            <span class="expand-icon">&#9660;</span>
          </div>
          <div class="card-body cov-body" id="agx-chg"></div>
        </div>
        <div class="agx-head">
          <h3 id="agx-acct-h">Accounts</h3>
          <div class="agx-tools">
            <select id="agx-sort" onchange="agxSetSort(this.value)">
              <option value="name">Sort: Name (A&ndash;Z)</option>
              <option value="arr">Sort: Contract ARR</option>
              <option value="date">Sort: Latest conversation</option>
              <option value="stage">Sort: Conversation stage</option>
              <option value="done">Sort: Least complete first</option>
            </select>
            <button onclick="agxExpandAll(true)">Expand all</button>
            <button onclick="agxExpandAll(false)">Collapse all</button>
          </div>
        </div>
        <div class="agx-facets" id="agx-facets"></div>
        <div id="agx-list"></div>
        </div>
        <div id="agx-tab-churn" hidden>
          <div class="agx-card">
            <h3 id="agx-rm-h">Removed for churn</h3>
            <div class="agx-sub" id="agx-rm-sub"></div>
            <div style="overflow-x:auto" id="agx-removed"></div>
          </div>
        </div>
      </div>
)---"

.AGX_JS <- r"---(
// ── Agentic Engagement Overview ────────────────────────────────────────────────────
// One row per account from the agentic profile report (00OVV000007LHjN2AW,
// op_list$agentic_profile), rolled up by .ao_build() in
// R/report_agentic_owner_html.R. The counting rules are the ones
// agentic_owner_summary.html uses: a value starting "TBD" is shown but not
// counted as known, and Conversation Status is placed on a stage by keyword.
// Accounts with a renewal at Account Risk Verbally Confirmed / Will Churn are
// left out (listed under "Removed for churn" at the bottom).
// Region / CSM pills and the search box apply as on the other Adoption tabs.
const AGX_STAGES = [
  { key: "held",  label: "Conversation held", color: "#16A34A", re: /\b(held|complete|completed|done|met|had|occurred)\b/i },
  { key: "sched", label: "Scheduled",         color: "#84CC16", re: /\b(schedul|booked|planned|calendar)/i },
  { key: "prog",  label: "In progress",       color: "#F59E0B", re: /(request|outreach|reach|contact|pending|progress|initiat|engag|follow)/i },
  { key: "none",  label: "Not started",       color: "#9CA3AF", re: /(not started|^none$|^no$|not yet|no conversation|declin)/i },
  { key: "other", label: "Other",             color: "#6366F1", re: /.^/ },
  { key: "unset", label: "No status",         color: "#E5E7EB", re: /.^/ }
];
const AGX_STAGE_BY = Object.fromEntries(AGX_STAGES.map((s, i) => [s.key, Object.assign({ rank: i }, s)]));
// Evaluation / Deployment Status: picklist values aren't fixed here, so each
// is coloured by keyword (done / under way / not started / other).
const AGX_PROGRESS = [
  { key: "done", label: "Done",        color: "#16A34A", re: /(complete|deployed|production|\blive\b|done|success|approved|signed|won)/i },
  { key: "prog", label: "Under way",   color: "#F59E0B", re: /(progress|evaluat|pilot|poc|proof|testing|trial|planned|plan|scop|design|build|pending|started|ongoing)/i },
  { key: "none", label: "Not started", color: "#9CA3AF", re: /(not started|^none$|^no$|not yet|^n\/?a$|no plans|declin|paused|on hold|not )/i },
  { key: "other", label: "Other",      color: "#6366F1", re: /.^/ },
  { key: "unset", label: "No status",  color: "#E5E7EB", re: /.^/ }
];
const AGX_PROGRESS_BY = Object.fromEntries(AGX_PROGRESS.map((s, i) => [s.key, Object.assign({ rank: i }, s)]));
const AGX_ADOPT = { green: "#22C55E", amber: "#F59E0B", red: "#EF4444", black: "#111111" };

// Every field on the report that describes the agentic profile. These are the
// "Incomplete" facets and the denominator of the completeness badge.
const AGX_FIELDS = [
  { key: "owner",        label: "Agentic Owner - Customer" },
  { key: "title",        label: "Title" },
  { key: "vision",       label: "Customer Agentic Vision" },
  { key: "architecture", label: "Agentic Architecture" },
  { key: "date",         label: "Conversation Date" },
  { key: "status",       label: "Conversation Status" },
  { key: "eval",         label: "Evaluation Status" },
  { key: "deploy",       label: "Deployment Status" },
  { key: "next_steps",   label: "Next Steps/Follow Up" }
];

const agxBlank = v => v === null || v === undefined || adoptBlank(v);
const agxTbd   = v => !agxBlank(v) && /^\s*tbd/i.test(String(v));
const agxKnown = v => !agxBlank(v) && !agxTbd(v);

// "Not started" is checked before "held" so "No conversation held" isn't read as held.
function agxStage(status) {
  if (!agxKnown(status)) return "unset";
  const s = String(status);
  if (AGX_STAGE_BY.none.re.test(s)) return "none";
  for (const k of ["held", "sched", "prog"]) if (AGX_STAGE_BY[k].re.test(s)) return k;
  return "other";
}
function agxProgress(v) {
  if (!agxKnown(v)) return "unset";
  const s = String(v);
  if (AGX_PROGRESS_BY.none.re.test(s)) return "none";
  for (const k of ["done", "prog"]) if (AGX_PROGRESS_BY[k].re.test(s)) return k;
  return "other";
}

// The rows. Without the agentic report (an op_list built before it was
// pulled, or a failed pull) fall back to the four agentic fields the account
// report carries, so the tab still says something.
// Agentic PoV projects (op_list$agentic_pov, op_list_agentic_pov()): one entry
// per project, matched to an account from its "PoV - <Account>" name. Status
// values aren't fixed, so each is placed on a stage by keyword; "lost" and
// "planned" are checked first so "Not started" / "Closed lost" aren't read as
// in progress / complete.
const AGX_POV = [
  { key: "lost", label: "Closed / cancelled", color: "#DC2626", re: /(cancel|closed lost|\blost\b|abandon|fail|terminat|on hold)/i },
  { key: "done", label: "Complete",           color: "#16A34A", re: /(complete|closed won|\bwon\b|success|done|converted|finished)/i },
  { key: "plan", label: "Planned",            color: "#F59E0B", re: /(not started|plan|schedul|pending|scoping|draft|\bnew\b|kick)/i },
  { key: "live", label: "In progress",        color: "#7C3AED", re: /(progress|active|running|underway|executing|open|started|ongoing)/i },
  { key: "other", label: "Other",             color: "#6B7280", re: /.^/ }
];
const AGX_POV_BY_KEY = Object.fromEntries(AGX_POV.map(s => [s.key, s]));
function agxPovStage(v) {
  if (agxBlank(v)) return "other";
  const hit = AGX_POV.find(st => st.re.test(String(v)));
  return hit ? hit.key : "other";
}
const agxKey = v => String(v === null || v === undefined ? "" : v).trim().toLowerCase();
const AG_POVS = ((typeof AGENTIC_DATA !== "undefined" && AGENTIC_DATA && AGENTIC_DATA.povs) || []).map((p, i) => Object.assign({}, p, {
  _p: i,
  povStage: agxPovStage(p.status),
  label: agxBlank(p.status) ? "No status" : String(p.status).trim(),
  daysLeft: p.expected_end ? Math.round((new Date(p.expected_end + "T00:00:00") - Date.now()) / 864e5) : null
}));
const AGX_POVS_FOR = {};
AG_POVS.forEach(p => { const k = agxKey(p.account || p.pov_account); (AGX_POVS_FOR[k] = AGX_POVS_FOR[k] || []).push(p); });
const agxPovsOf = a => AGX_POVS_FOR[agxKey(a.account)] || AGX_POVS_FOR[agxKey(a.name)] || [];

const AGX_LIVE = !!(AGENTIC_DATA && AGENTIC_DATA.has_data);
const AG_ROWS = (AGX_LIVE ? AGENTIC_DATA.rows : ACCOUNTS.map(a => ({
  name: a.name, account: a.name, csm: a.csm, region: a.region,
  owner: a.agentic_owner, title: a.agentic_owner_title,
  vision: a.agentic_vision, architecture: a.agentic_architecture,
  arr: covNum(a.arr_raw), adoption: ratingColour(a.product_rating) || null, url: null,
  months_as_customer: covNum(a.months_as_customer), customer_since: adoptBlank(a.customer_since) ? null : a.customer_since
}))).map((a, i) => agxPrep(a, i, agxPovsOf));
// One rollup row -> the fields every view reads. Shared by the current rows and
// the previous snapshot's (AGX_PREV), so both are counted by the same rules.
function agxPrep(a, i, povsOf) {
  const r = Object.assign({}, a);
  r._i = i;
  r.name = agxBlank(r.name) ? "(no name)" : String(r.name);
  r.csm = agxBlank(r.csm) ? "Unassigned" : String(r.csm).trim();
  r.region = agxBlank(r.region) ? "" : r.region;
  r.stage = agxStage(r.status);
  r.evalStage = agxProgress(r.eval);
  r.deployStage = agxProgress(r.deploy);
  r.hasOwner = agxKnown(r.owner);
  r.hasVision = agxKnown(r.vision);
  r.hasArch = agxKnown(r.architecture);
  r.held = r.stage === "held";
  r.statusLabel = agxBlank(r.status) ? "No status" : String(r.status).trim();
  r.evalLabel = agxBlank(r.eval) ? "No status" : String(r.eval).trim();
  r.evaluating = typeof r.evaluating === "boolean" ? r.evaluating : agxEvaluating(r.eval);
  r.deployLabel = agxBlank(r.deploy) ? "No status" : String(r.deploy).trim();
  r.daysAgo = r.date ? Math.round((Date.now() - new Date(r.date + "T00:00:00")) / 864e5) : null;
  r.done = AGX_FIELDS.filter(f => agxKnown(r[f.key])).length;
  r.adoption = agxBlank(r.adoption) ? "" : String(r.adoption).toLowerCase();
  r.povs = povsOf(r);
  return r;
}
const AG_REMOVED = AGX_LIVE ? (AGENTIC_DATA.removed || []) : [];

// The comparison snapshot (enrich_list_object() -> changes_detail$agentic): its
// rollup, recounted under the current filters, gives the delta on every number.
const AGX_HAS_PREV = AGX_LIVE && Array.isArray(AGENTIC_DATA.prev) && AGENTIC_DATA.prev.length > 0;
const AGX_PREV_DATE = AGX_HAS_PREV ? (AGENTIC_DATA.prev_date || "") : "";
const AGX_PREV_POVS_FOR = {};
(AGX_HAS_PREV && AGENTIC_DATA.prev_povs ? AGENTIC_DATA.prev_povs : []).forEach(p => {
  const k = agxKey(p.account);
  (AGX_PREV_POVS_FOR[k] = AGX_PREV_POVS_FOR[k] || []).push(Object.assign({}, p, { povStage: agxPovStage(p.status) }));
});
const AGX_PREV = AGX_HAS_PREV ? AGENTIC_DATA.prev.map((a, i) =>
  agxPrep(a, i, r => AGX_PREV_POVS_FOR[agxKey(r.account)] || AGX_PREV_POVS_FOR[agxKey(r.name)] || [])) : [];
const AGX_CHG = AGX_HAS_PREV && AGENTIC_DATA.changes ? AGENTIC_DATA.changes : null;
const agxPrevLabel = () => AGX_PREV_DATE ? agxDate(AGX_PREV_DATE) : "the last run";

// "▲ 2" / "▼ 1" since the comparison snapshot; nothing when unchanged or no
// comparison. good: +1 = up is good (green), -1 = up is bad, 0 = neutral.
function agxDelta(now, prev, good = 1, fmt = v => v) {
  if (!AGX_HAS_PREV || prev === null || prev === undefined) return "";
  const d = now - prev;
  if (!d) return "";
  const cls = good === 0 ? "neutral" : (d > 0) === (good > 0) ? "up" : "down";
  return `<span class="agx-delta ${cls}" title="${esc(fmt(prev))} on ${esc(agxPrevLabel())}">${d > 0 ? "▲" : "▼"} ${fmt(Math.abs(d))}</span>`;
}
// Fields the fallback rows can't have are not counted against them.
const AGX_FIELDS_LIVE = AGX_LIVE ? AGX_FIELDS : AGX_FIELDS.filter(f => ["owner", "title", "vision", "architecture"].includes(f.key));
if (!AGX_LIVE) AG_ROWS.forEach(r => { r.done = AGX_FIELDS_LIVE.filter(f => agxKnown(r[f.key])).length; });

const agxState = { gaps: new Set(), status: "", eval: "", deploy: "", evalTF: "", inc: new Set(), incMode: "all", sort: "name" };
const agxOpen  = new Set();
let agxShown   = [];

const agxPct = (n, d) => d ? Math.round(n / d * 100) : 0;
const agxUsd = v => (v === null || v === undefined || v === "" || isNaN(v)) ? "—"
  : v >= 1e6 ? "$" + (v / 1e6).toFixed(1) + "M" : v >= 1e3 ? "$" + Math.round(v / 1e3) + "K" : "$" + Math.round(v);
const agxDate = d => d ? new Date(d + "T00:00:00").toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric" }) : "";
// Customer age, worded like fmt_tenure() in R: "50 months (since Mar 3, 2022)".
// customer_since is ISO from .ao_build(), already formatted in the ACCOUNTS fallback.
function agxTenure(a) {
  const m = covNum(a.months_as_customer);
  if (m === null) return `<span class="em-dash">\u2014</span>`;
  const s = agxBlank(a.customer_since) ? "" : String(a.customer_since);
  const since = !s ? "" : /^\d{4}-\d{2}-\d{2}$/.test(s) ? agxDate(s) : s;
  return `${m} month${m === 1 ? "" : "s"}` + (since ? `<span class="agx-hint">since ${esc(since)}</span>` : "");
}
const agxAgo = a => a.daysAgo === null ? "" : a.daysAgo >= 0 ? a.daysAgo + " days ago" : "in " + (-a.daysAgo) + " days";

// Global region / CSM / search, then the tab's own gap chips. Not the bar
// filters: each chart keeps showing every value so you can switch between them.
// Evaluating = TRUE for Evaluation Status Active / Completed (or a checkbox
// TRUE), FALSE otherwise. Same rule as .ao_build()'s `evaluating` column.
function agxEvaluating(v) {
  return v === true || /^\s*(active|completed?|true)\s*$/i.test(String(v === null || v === undefined ? "" : v));
}
function agxSetEvalTF(v) { agxState.evalTF = agxState.evalTF === v ? "" : v; renderAgentic(); }

function agxBase(ignoreEvalTF) { return agxFilterRows(AG_ROWS, ignoreEvalTF); }
// The previous snapshot under the same filters (and bar picks), or null.
function agxPrevView() { return AGX_HAS_PREV ? agxBars(agxFilterRows(AGX_PREV), "") : null; }
function agxFilterRows(rows, ignoreEvalTF) {
  const q = searchTerm.toLowerCase();
  return rows.filter(a =>
    (activeRegion === "all" || a.region === activeRegion) &&
    (activeCSM === "all" || a.csm === activeCSM) &&
    (!agxState.gaps.has("owner")  || !a.hasOwner) &&
    (!agxState.gaps.has("vision") || !a.hasVision) &&
    (!agxState.gaps.has("conv")   || !a.held) &&
    (!agxState.gaps.has("pov")    || a.povs.length) &&
    (ignoreEvalTF || !agxState.evalTF || (agxState.evalTF === "TRUE") === a.evaluating) &&
    (!q || [a.name, a.csm, a.owner, a.title, a.vision, a.architecture, a.status, a.eval, a.deploy, a.next_steps,
            ...a.povs.map(p => [p.project, p.status, p.stage, p.notes].join(" "))]
             .some(v => v && String(v).toLowerCase().includes(q))));
}
function agxBars(rows, which) {
  return rows.filter(a =>
    (which === "status" || !agxState.status || a.statusLabel === agxState.status) &&
    (which === "eval"   || !agxState.eval   || a.evalLabel   === agxState.eval) &&
    (which === "deploy" || !agxState.deploy || a.deployLabel === agxState.deploy));
}
function agxIncFilter(rows) {
  if (!agxState.inc.size) return rows;
  const keys = [...agxState.inc];
  return rows.filter(a => agxState.incMode === "any" ? keys.some(k => !agxKnown(a[k])) : keys.every(k => !agxKnown(a[k])));
}

function agxToggleGap(g)   { agxState.gaps.has(g) ? agxState.gaps.delete(g) : agxState.gaps.add(g); renderAgentic(); }
function agxToggleInc(k)   { agxState.inc.has(k) ? agxState.inc.delete(k) : agxState.inc.add(k); renderAgentic(); }
function agxSetMode(m)     { agxState.incMode = m; renderAgentic(); }
function agxSetSort(s)     { agxState.sort = s; renderAgentic(); }
function agxPickBar(el) {
  const which = el.dataset.which, v = el.dataset.value;
  agxState[which] = agxState[which] === v ? "" : v;
  renderAgentic();
}
function agxReset() {
  Object.assign(agxState, { status: "", eval: "", deploy: "", evalTF: "", incMode: "all" });
  agxState.gaps.clear(); agxState.inc.clear();
  renderAgentic();
}
function agxToggle(header, i) {
  header.parentElement.classList.toggle("open");
  if (agxOpen.has(i)) agxOpen.delete(i); else agxOpen.add(i);
}
function agxExpandAll(open) {
  agxShown.forEach(a => open ? agxOpen.add(a._i) : agxOpen.delete(a._i));
  renderAgentic();
}
function agxGoAccount(i) {
  const a = AG_ROWS[i];
  goToAccount(agxBlank(a.account) ? a.name : a.account);
}

function agxFiltersHtml(base) {
  const chip = (g, label) => `<button class="agx-chip${agxState.gaps.has(g) ? " on" : ""}" onclick="agxToggleGap(&quot;${g}&quot;)">${label}</button>`;
  const bits = [];
  if (agxState.status) bits.push("Conversation: " + agxState.status);
  if (agxState.eval)   bits.push("Evaluation: " + agxState.eval);
  if (agxState.deploy) bits.push("Deployment: " + agxState.deploy);
  if (agxState.evalTF) bits.push("Evaluating: " + agxState.evalTF);
  if (agxState.inc.size) bits.push("incomplete " + AGX_FIELDS.filter(f => agxState.inc.has(f.key)).map(f => f.label).join(agxState.incMode === "any" ? " or " : " and "));
  return `<span class="agx-facets"><span class="lbl">Gaps</span></span>` +
    chip("owner", "No owner") + chip("vision", "No vision") + chip("conv", "No conversation held") +
    (AG_POVS.length ? `<span class="agx-facets"><span class="lbl">PoV</span></span>` + chip("pov", "Has an agentic PoV") : "") +
    (AGX_LIVE ? (() => {
      // Counts ignore the TRUE/FALSE facet itself, so both options always show what they'd give.
      const pool = agxBase(true), nT = pool.filter(a => a.evaluating).length;
      const tf = (v, n) => `<button class="agx-chip${agxState.evalTF === v ? " on" : ""}" title="Evaluation Status ${v === "TRUE" ? "Active or Completed" : "anything else, or blank"}" onclick="agxSetEvalTF(&quot;${v}&quot;)">${v}<span class="n">${n}</span></button>`;
      return `<span class="agx-facets"><span class="lbl">Evaluating</span></span>` + tf("TRUE", nT) + tf("FALSE", pool.length - nT);
    })() : "") +
    `<button class="agx-chip reset" onclick="agxReset()">Clear filters</button>` +
    `<span class="agx-active">${bits.length ? "Filtered: " + esc(bits.join(" · ")) : ""}</span>`;
}

function agxKpiStats(rows) {
  const ps = rows.flatMap(a => a.povs);
  return {
    n: rows.length, arr: rows.reduce((t, a) => t + (covNum(a.arr) || 0), 0),
    o: rows.filter(a => a.hasOwner).length, v: rows.filter(a => a.hasVision).length,
    h: rows.filter(a => a.held).length, full: rows.filter(a => a.done === AGX_FIELDS_LIVE.length).length,
    recent: rows.filter(a => a.held && a.daysAgo !== null && a.daysAgo <= 90).length,
    pov: rows.filter(a => a.povs.length).length,
    live: ps.filter(p => p.povStage === "live").length, done: ps.filter(p => p.povStage === "done").length
  };
}
function agxKpisHtml(rows, prevRows) {
  const c = agxKpiStats(rows), p = prevRows ? agxKpiStats(prevRows) : {};
  const hasArr = rows.some(a => covNum(a.arr) !== null);
  const k = (l, val, d, s, pc) => `<div class="agx-kpi"><div class="l">${l}</div><div class="v">${val}${pc !== undefined ? `<small>${pc}%</small>` : ""}${d}</div><div class="s">${s}</div></div>`;
  const nRm = AGX_LIVE ? agxRemovedRows().length : 0;
  const rmNote = nRm ? ` \u00B7 <a class="opp-link" href="#" onclick="event.preventDefault(); agxSetTab(&quot;churn&quot;)">${nRm} removed for churn</a>` : "";
  return k("Accounts", c.n, agxDelta(c.n, p.n, 0),
           (hasArr ? agxUsd(c.arr) + " contract ARR" + agxDelta(c.arr, p.arr, 0, agxUsd) : "in view") + rmNote) +
    k("Agentic owner known", c.o, agxDelta(c.o, p.o), (c.n - c.o) + " still unknown", agxPct(c.o, c.n)) +
    k("Vision captured", c.v, agxDelta(c.v, p.v), (c.n - c.v) + " still missing", agxPct(c.v, c.n)) +
    k("Conversation held", c.h, agxDelta(c.h, p.h), c.recent + " in the last 90 days", agxPct(c.h, c.n)) +
    k("Full profile", c.full, agxDelta(c.full, p.full), "all " + AGX_FIELDS_LIVE.length + " fields filled in", agxPct(c.full, c.n)) +
    k("Agentic PoVs", c.pov, agxDelta(c.pov, p.pov), `${c.live} in progress${agxDelta(c.live, p.live)} \u00B7 ${c.done} complete${agxDelta(c.done, p.done)}`, agxPct(c.pov, c.n));
}

function agxFunnelHtml(rows, prevRows) {
  const n = rows.length;
  const cnt = (rs, f) => rs ? rs.filter(f).length : null;
  const steps = [
    { l: "Agentic owner known",   s: "Agentic Owner - Customer", f: a => a.hasOwner, cls: "" },
    { l: "Vision captured",       s: "Customer Agentic Vision",  f: a => a.hasVision, cls: "lime" },
    { l: "Owner and vision",      s: "both filled in",           f: a => a.hasOwner && a.hasVision, cls: "lime" },
    { l: "Architecture captured", s: "Agentic Architecture",     f: a => a.hasArch, cls: "lime" },
    { l: "Conversation held",     s: "Conversation Status",      f: a => a.held, cls: "held" }
  ].map(st => Object.assign(st, { c: cnt(rows, st.f), pc: cnt(prevRows, st.f) }));
  return steps.map(st =>
    `<div class="agx-fun"><div class="agx-fun-l">${st.l}<span>${st.s}</span></div>` +
    `<div class="agx-track"><div class="agx-fill ${st.cls}" style="width:${agxPct(st.c, n)}%"></div></div>` +
    `<div class="agx-fun-n"><b>${st.c}</b>${agxDelta(st.c, st.pc)} / ${n} &middot; ${agxPct(st.c, n)}%</div></div>`).join("");
}

// One clickable bar per value of `labelKey`, coloured by its stage.
function agxBarsHtml(rows, which, labelKey, stageKey, stageBy, prevRows) {
  const by = {};
  rows.forEach(a => { (by[a[labelKey]] = by[a[labelKey]] || { label: a[labelKey], stage: a[stageKey], n: 0, p: 0 }).n++; });
  // Values that were there last run but are gone now still get a row, at 0.
  if (prevRows) prevRows.forEach(a => { (by[a[labelKey]] = by[a[labelKey]] || { label: a[labelKey], stage: a[stageKey], n: 0, p: 0 }).p++; });
  const list = Object.values(by).sort((x, y) => stageBy[x.stage].rank - stageBy[y.stage].rank || y.n - x.n);
  const max = Math.max(1, ...list.map(d => d.n));
  return list.length ? list.map(d =>
    `<div class="agx-st${agxState[which] === d.label ? " on" : ""}" data-which="${which}" data-value="${esc(d.label)}" onclick="agxPickBar(this)">` +
    `<div class="agx-st-name"><span class="agx-sw" style="background:${stageBy[d.stage].color}"></span><em title="${esc(d.label)}">${esc(d.label)}</em></div>` +
    `<div class="agx-track"><div class="agx-fill" style="width:${d.n / max * 100}%;background:${stageBy[d.stage].color}"></div></div>` +
    `<div class="agx-st-n">${d.n}${prevRows ? agxDelta(d.n, d.p, 0) : ""}</div></div>`).join("") : `<div class="agx-empty">No accounts in view</div>`;
}

function agxFacetsHtml(rows) {
  const chips = AGX_FIELDS_LIVE.map(f => {
    const n = rows.filter(a => !agxKnown(a[f.key])).length;
    return `<button class="agx-chip${agxState.inc.has(f.key) ? " on" : ""}" onclick="agxToggleInc(&quot;${f.key}&quot;)">${f.label}<span class="n">${n}</span></button>`;
  }).join("");
  const mode = agxState.inc.size > 1
    ? `<span class="agx-mode"><button class="${agxState.incMode === "all" ? "on" : ""}" onclick="agxSetMode(&quot;all&quot;)">All selected</button>` +
      `<button class="${agxState.incMode === "any" ? "on" : ""}" onclick="agxSetMode(&quot;any&quot;)">Any selected</button></span>` : "";
  return `<span class="lbl">Incomplete</span>${chips}${mode}`;
}

// A field value for the expanded card: the value, "TBD" (shown, not counted),
// or "Incomplete".
function agxVal(v) {
  if (agxKnown(v)) return esc(String(v));
  if (agxTbd(v))   return `<span class="agx-tbd">TBD</span><span class="agx-hint">${esc(String(v))}</span>`;
  return `<span class="agx-gap">Incomplete</span>`;
}
function agxField(label, inner) {
  return `<div class="field-item"><div class="field-label">${label}</div><div class="field-value">${inner}</div></div>`;
}
function agxPill(k, label, stage, stageBy, value) {
  if (!agxKnown(value)) return `<span class="agx-pill miss"><span class="k">${k}</span><em>${agxTbd(value) ? "TBD" : "Incomplete"}</em></span>`;
  return `<span class="agx-pill" title="${esc(k + ": " + label)}"><span class="agx-sw" style="background:${stageBy[stage].color}"></span>` +
         `<span class="k">${k}</span><em>${esc(label)}</em></span>`;
}

function agxCardHtml(a) {
  const nF = AGX_FIELDS_LIVE.length;
  const doneCls = a.done === nF ? "full" : a.done === 0 ? "none" : "part";
  const dot = a.adoption ? `<span class="agx-dot" title="Product adoption: ${esc(a.adoption)}" style="background:${AGX_ADOPT[a.adoption] || "#D1D5DB"}"></span>` : "";
  const who = a.hasOwner ? `<b>${esc(a.owner)}</b>${agxKnown(a.title) ? " · " + esc(a.title) : ""}` : `<span class="agx-gap">No agentic owner</span>`;
  const pills = AGX_LIVE
    ? agxPill("Conv", a.statusLabel, a.stage, AGX_STAGE_BY, a.status) +
      agxPill("Eval", a.evalLabel, a.evalStage, AGX_PROGRESS_BY, a.eval) +
      agxPill("Deploy", a.deployLabel, a.deployStage, AGX_PROGRESS_BY, a.deploy)
    : "";
  const povPill = a.povs.length ? (() => {
    const p = a.povs[0], st = AGX_POV_BY_KEY[p.povStage];
    return `<span class="agx-pill" title="${esc("Agentic PoV: " + p.label + (a.povs.length > 1 ? " (+" + (a.povs.length - 1) + " more)" : ""))}">` +
           `<span class="agx-sw" style="background:${st.color}"></span><span class="k">PoV</span><em>${esc(p.label)}</em></span>`;
  })() : "";
  const open = agxOpen.has(a._i);
  // The standalone page (report_agentic_owner_html()) has no account cards to open.
  const links = (typeof AGX_STANDALONE !== "undefined" && AGX_STANDALONE) ? []
    : [`<a class="cc-link" href="#" onclick="event.preventDefault(); agxGoAccount(${a._i})">Account card &#8599;</a>`];
  if (a.url) links.push(`<a class="cc-link" href="${esc(a.url)}" target="_blank" rel="noopener">Salesforce &#8599;</a>`);
  const dateHtml = a.date ? esc(agxDate(a.date)) + `<span class="agx-hint">${agxAgo(a)}</span>` : agxVal(null);
  const fields = [
    agxField("Customer Success Manager", esc(a.csm)),
    agxField("Customer for", agxTenure(a)),
    agxField("Agentic Owner - Customer", agxVal(a.owner)),
    agxField("Title", agxVal(a.title))
  ];
  if (AGX_LIVE) fields.push(
    agxField("Conversation Date", dateHtml),
    agxField("Conversation Status", agxKnown(a.status)
      ? `<span class="agx-sw" style="background:${AGX_STAGE_BY[a.stage].color};margin-right:6px"></span>${esc(a.statusLabel)}` : agxVal(a.status)),
    agxField("Evaluation Status", agxVal(a.eval)),
    agxField("Evaluating", a.evaluating ? "<b>TRUE</b>" : "FALSE"),
    agxField("Deployment Status", agxVal(a.deploy))
  );
  fields.push(agxField("Region", a.region ? esc(a.region) : `<span class="em-dash">—</span>`),
              agxField("Contract ARR", agxUsd(covNum(a.arr))));
  const text = (label, v) => `<div class="agx-text">${agxField(label, agxVal(v))}</div>`;
  return `<div class="card agx-acct${open ? " open" : ""}" id="agx-card-${a._i}">
    <div class="card-header" onclick="agxToggle(this, ${a._i})">
      <div class="agx-who"><span class="card-name">${dot}${esc(a.name)}</span>
        <div class="agx-who-sub">${esc(a.csm)} &middot; ${who}</div></div>
      <div class="agx-pills">${povPill}${pills}</div>
      <span class="agx-done ${doneCls}" title="${a.done} of ${nF} agentic fields filled in">${a.done}/${nF}</span>
      <span class="card-arr">${agxUsd(covNum(a.arr))}</span>
      <span class="expand-icon">&#9660;</span>
    </div>
    <div class="card-body"><div class="agx-body">
      <div class="agx-links">${links.join("")}</div>
      <div class="agx-fields">${fields.join("")}</div>
      <div class="agx-texts">
        ${text("Customer Agentic Vision", a.vision)}
        ${text("Agentic Architecture", a.architecture)}
        ${AGX_LIVE ? text("Next Steps/Follow Up", a.next_steps) : ""}
      </div>
      ${a.povs.map(agxPovBlock).join("")}
    </div></div>
  </div>`;
}

// What changed since the comparison snapshot: accounts that entered / left the
// Coverage tab and per-account agentic field changes. Follows region / CSM /
// search. Shared with the dashboard's Changes tab (renderAgenticChanges()).
function agxChangesHtml(ch, opts) {
  opts = opts || {};
  if (!ch) return `<p class="empty-msg" style="padding:14px 0">No comparison snapshot with agentic data yet.</p>`;
  const q = (opts.filter && typeof searchTerm !== "undefined") ? searchTerm.toLowerCase() : "";
  const keep = r => !opts.filter || ((activeRegion === "all" || r.region === activeRegion) &&
                                     (activeCSM === "all" || r.csm === activeCSM) &&
                                     (!q || String(r.name || "").toLowerCase().includes(q)));
  const arrow = (o, n) => {
    o = agxBlank(o) ? "" : String(o); n = agxBlank(n) ? "" : String(n);
    if (o === n) return "";
    return `<span class="old">${esc(o) || "<em>empty</em>"}</span> \u2192 <span class="new">${esc(n) || "<em>empty</em>"}</span>`;
  };
  const SHORT = [["owner", "Owner"], ["status", "Conversation"], ["date", "Conversation date"], ["eval", "Evaluation"],
                 ["deploy", "Deployment"], ["evaluating", "Evaluating"], ["pov", "PoV"]];
  const rows = [];
  (ch.added || []).filter(keep).forEach(r => rows.push([r.name, r.csm, `<span class="chg-badge chg-added">+ new on the Coverage tab</span>`]));
  (ch.removed || []).filter(keep).forEach(r => rows.push([r.name, r.csm, `<span class="chg-badge chg-removed">\u2212 left the Coverage tab</span>`]));
  (ch.field_changes || []).filter(keep).forEach(r => {
    const bits = SHORT.map(([f, l]) => {
      const a = arrow(r[f + ".old"], r[f + ".new"]);
      return a ? `<div><b>${l}:</b> ${a}</div>` : "";
    }).join("");
    const long = String(r.changed || "").split(", ").filter(x => /Vision|Architecture|Next steps|Title/.test(x));
    rows.push([r.name, r.csm, bits + (long.length ? `<div><b>Updated:</b> ${esc(long.join(", "))}</div>` : "")]);
  });
  if (!rows.length) return `<p class="empty-msg" style="padding:14px 0">No agentic changes${opts.filter ? " for the accounts in view" : ""}.</p>`;
  rows.sort((a, b) => String(a[0]).localeCompare(String(b[0])));
  return `<table class="flat-table agx-chg"><thead><tr><th>Account</th><th>CSM</th><th>What changed</th></tr></thead><tbody>` +
    rows.map(r => `<tr><td><b>${esc(r[0])}</b></td><td>${esc(r[1] || "")}</td><td>${r[2]}</td></tr>`).join("") + `</tbody></table>`;
}

// One PoV project on an account card.
function agxPovBlock(p) {
  const st = AGX_POV_BY_KEY[p.povStage];
  const end = p.expected_end ? esc(agxDate(p.expected_end)) +
    (p.daysLeft === null ? "" : p.daysLeft >= 0 ? ` (in ${p.daysLeft} days)` : ` (${-p.daysLeft} days ago)`) : "—";
  return `<div class="agx-pov-block">
    <div class="h"><span class="k" style="font-size:9.5px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;color:var(--text-3)">Agentic PoV</span>
      ${esc(p.project)}<span class="agx-pill"><span class="agx-sw" style="background:${st.color}"></span><em>${esc(p.label)}</em></span></div>
    <div class="m">Opportunity stage: ${agxBlank(p.stage) ? "—" : esc(p.stage)} &middot; Expected end: ${end}</div>
    <div class="n">${agxBlank(p.notes) ? `<span class="agx-gap">No project notes</span>` : esc(p.notes)}</div>
  </div>`;
}

// The Agentic PoVs card: every project on an account in view (plus, with no
// filters on, projects that matched no account), soonest expected end first.
// Clicking a matched project opens and scrolls to its account card.
function agxPovHtml(view) {
  const unfiltered = activeRegion === "all" && activeCSM === "all" && !searchTerm && !agxState.gaps.size &&
                     !agxState.status && !agxState.eval && !agxState.deploy && !agxState.evalTF;
  const rows = view.flatMap(a => a.povs.map(p => ({ p, a })));
  const seen = new Set(rows.map(r => r.p._p));
  if (unfiltered) AG_POVS.filter(p => !seen.has(p._p)).forEach(p => rows.push({ p, a: null }));
  rows.sort((x, y) => (x.p.expected_end || "9999").localeCompare(y.p.expected_end || "9999") ||
                      String(x.p.project).localeCompare(String(y.p.project)));
  const dash = `<span class="em-dash">—</span>`;
  const body = rows.map(({ p, a }) => {
    const st = AGX_POV_BY_KEY[p.povStage];
    const removed = !a && p.matched && AG_REMOVED.some(r => agxKey(r.account || r.name) === agxKey(p.account));
    const why = !p.matched ? "no account match in Salesforce" : removed ? "removed for churn" : "not on the agentic report";
    const acct = a ? `<b>${esc(a.name)}</b>` : `${esc(p.account || p.pov_account || p.project)}<span class="agx-hint">${why}</span>`;
    return `<tr class="${a ? "click" : ""}"${a ? ` onclick="agxOpenCard(${a._i})"` : ""}>` +
      `<td>${acct}</td><td>${esc(p.project)}</td>` +
      `<td><span class="agx-pill"><span class="agx-sw" style="background:${st.color}"></span><em>${esc(p.label)}</em></span></td>` +
      `<td>${agxBlank(p.stage) ? dash : esc(p.stage)}</td>` +
      `<td>${p.expected_end ? esc(agxDate(p.expected_end)) + `<span class="agx-hint">${p.daysLeft >= 0 ? "in " + p.daysLeft + " days" : -p.daysLeft + " days ago"}</span>` : dash}</td>` +
      `<td>${agxBlank(p.notes) ? dash : `<div class="agx-pov-notes" title="${esc(p.notes)}">${esc(p.notes)}</div>`}</td></tr>`;
  }).join("");
  const n = rows.length, live = rows.filter(r => r.p.povStage === "live").length;
  document.getElementById("agx-pov-h").textContent = `Agentic PoVs (${n})`;
  document.getElementById("agx-pov-sub").innerHTML =
    `Proof-of-value projects from Salesforce report 00OVV000007QLj72AG, matched to accounts from &ldquo;PoV - &lt;Account&gt;&rdquo; &middot; ${live} in progress &middot; click a project to open its account`;
  return `<table class="flat-table agx-pov-table"><thead><tr><th>Account</th><th>Project</th><th>Status</th>` +
    `<th>Opportunity stage</th><th>Expected end</th><th>Project notes</th></tr></thead><tbody>` +
    (body || `<tr><td colspan="6" class="agx-empty">No PoV projects for the accounts in view</td></tr>`) + `</tbody></table>`;
}
function agxOpenCard(i) {
  agxOpen.add(i); agxTab = "cov"; renderAgentic();
  const el = document.getElementById("agx-card-" + i);
  if (el && el.scrollIntoView) el.scrollIntoView({ behavior: "smooth", block: "start" });
}

function agxSorted(rows) {
  const s = agxState.sort;
  const key = {
    name:  a => 0,
    arr:   a => -(covNum(a.arr) || 0),
    date:  a => a.date ? -new Date(a.date + "T00:00:00").getTime() : Infinity,
    stage: a => AGX_STAGE_BY[a.stage].rank,
    done:  a => a.done
  }[s] || (a => 0);
  return rows.slice().sort((x, y) => (key(x) - key(y)) || x.name.localeCompare(y.name));
}

let agxTab = "cov";
function agxSetTab(t) { agxTab = t; renderAgentic(); }
const agxRmSort = { col: "name", dir: 1 };
function agxSortRm(col) {
  if (agxRmSort.col === col) agxRmSort.dir = -agxRmSort.dir;
  else { agxRmSort.col = col; agxRmSort.dir = col === "arr" ? -1 : 1; }
  renderAgentic();
}
// The accounts .ao_build() dropped for exclude_risk, with their Account Risk
// and earliest renewal close: the "Removed for churn" tab of
// agentic_owner_summary.html. Follows the region / CSM pills and search.
function agxRemovedRows() {
  const q = searchTerm.toLowerCase();
  return AG_REMOVED.filter(a =>
    (activeRegion === "all" || a.region === activeRegion) &&
    (activeCSM === "all" || a.csm === activeCSM) &&
    (!q || [a.name, a.csm, a.owner, a.status, a.risk].some(v => v && String(v).toLowerCase().includes(q))));
}
const AGX_RM_COLS = [
  { key: "name",   label: "Account",             val: a => String(a.name || "").toLowerCase() },
  { key: "csm",    label: "CSM",                 val: a => agxBlank(a.csm) ? "\uffff" : String(a.csm).toLowerCase() },
  { key: "risk",   label: "Account risk",        val: a => String(a.risk || "") },
  { key: "close",  label: "Renewal close",       val: a => a.close || "\uffff" },
  { key: "owner",  label: "Agentic owner",       val: a => agxKnown(a.owner) ? String(a.owner).toLowerCase() : "\uffff" },
  { key: "status", label: "Conversation status", val: a => agxKnown(a.status) ? String(a.status).toLowerCase() : "\uffff" },
  { key: "pov",    label: "Agentic PoV",         val: a => (agxPovsOf(a)[0] || {}).label || "\uffff" },
  { key: "arr",    label: "Contract ARR",        val: a => covNum(a.arr) === null ? -1 : covNum(a.arr), num: true }
];
function agxRemovedHtml(list) {
  const c = AGX_RM_COLS.find(x => x.key === agxRmSort.col) || AGX_RM_COLS[0];
  const rows = list.slice().sort((x, y) => {
    const a = c.val(x), b = c.val(y);
    const d = typeof a === "number" ? a - b : String(a).localeCompare(String(b));
    return d * agxRmSort.dir || String(x.name).localeCompare(String(y.name));
  });
  const dash = `<span class="em-dash">\u2014</span>`;
  const head = AGX_RM_COLS.map(k =>
    `<th class="${k.num ? "cov-num" : ""}" onclick="agxSortRm(&quot;${k.key}&quot;)">${k.label}${agxRmSort.col === k.key ? (agxRmSort.dir === 1 ? " \u2191" : " \u2193") : ""}</th>`).join("");
  const body = rows.map(a => {
    const nm = a.url ? `<a class="opp-link" href="${esc(a.url)}" target="_blank" rel="noopener">${esc(a.name)}</a>` : esc(a.name);
    return `<tr><td>${nm}${a.region ? `<span class="agx-hint">${esc(a.region)}</span>` : ""}</td>` +
      `<td>${agxBlank(a.csm) ? dash : esc(a.csm)}</td><td><span class="agx-risk">${esc(a.risk)}</span></td>` +
      `<td>${a.close ? esc(agxDate(a.close)) : dash}</td>` +
      `<td>${agxKnown(a.owner) ? esc(a.owner) + (agxKnown(a.title) ? `<span class="agx-hint">${esc(a.title)}</span>` : "") : dash}</td>` +
      `<td>${agxKnown(a.status) ? esc(a.status) : dash}</td>` +
      `<td>${agxPovsOf(a).length ? agxPovsOf(a).map(p => esc(p.label)).join(", ") : dash}</td>` +
      `<td class="cov-num">${agxUsd(covNum(a.arr))}</td></tr>`;
  }).join("");
  return `<table class="flat-table agx-rm-table"><thead><tr>${head}</tr></thead><tbody>` +
    (body || `<tr><td colspan="${AGX_RM_COLS.length}" class="agx-empty">${AG_REMOVED.length ? "No removed accounts match these filters" : "No accounts were removed for churn"}</td></tr>`) +
    `</tbody></table>`;
}

function renderAgentic() {
  const base = agxBase(), view = agxBars(base, "");
  setCount("sub-count-agentic", view.length);
  document.getElementById("agx-note").innerHTML = AGX_LIVE ? "" :
    `<div class="agx-note">The agentic profile report (00OVV000007LHjN2AW) is not in this op_list, so only the four agentic fields on the account report are shown. Re-run enrich_opportunities() to pull it.</div>`;
  document.getElementById("agx-filters").innerHTML = agxFiltersHtml(base);
  const pview = agxPrevView(), pbase = AGX_HAS_PREV ? agxFilterRows(AGX_PREV) : null;
  document.getElementById("agx-kpis").innerHTML = agxKpisHtml(view, pview);
  document.getElementById("agx-funnel").innerHTML = agxFunnelHtml(view, pview);
  document.getElementById("agx-since").innerHTML = AGX_HAS_PREV
    ? `<div class="agx-since">\u25B2 / \u25BC = change since the <b>${esc(agxPrevLabel())}</b> snapshot, under the same filters (hover for the earlier figure)</div>` : "";
  document.getElementById("agx-chg-card").style.display = AGX_HAS_PREV ? "" : "none";
  if (AGX_HAS_PREV) {
    const chgHtml = agxChangesHtml(AGX_CHG, { filter: true });
    const nChg = (chgHtml.match(/<tr>/g) || []).length - 1;
    document.getElementById("agx-chg-h").textContent = `What changed since ${agxPrevLabel()}` + (nChg > 0 ? ` (${nChg})` : "");
    document.getElementById("agx-chg-sub").textContent = "accounts added, removed or with agentic fields updated \u00B7 follows region / CSM / search";
    document.getElementById("agx-chg").innerHTML = chgHtml;
  }
  document.getElementById("agx-status").innerHTML = AGX_LIVE
    ? agxBarsHtml(agxBars(base, "status"), "status", "statusLabel", "stage", AGX_STAGE_BY, pbase && agxBars(pbase, "status"))
    : `<div class="agx-empty">Needs the agentic profile report</div>`;
  const used = new Set(agxBars(base, "status").map(a => a.stage));
  document.getElementById("agx-legend").innerHTML = AGX_LIVE ? AGX_STAGES.filter(s => used.has(s.key)).map(s =>
    `<span><span class="agx-sw" style="background:${s.color}"></span>${s.label}</span>`).join("") : "";
  document.getElementById("agx-evdep-card").style.display = AGX_LIVE ? "" : "none";
  if (AGX_LIVE) {
    document.getElementById("agx-eval").innerHTML   = agxBarsHtml(agxBars(base, "eval"),   "eval",   "evalLabel",   "evalStage",   AGX_PROGRESS_BY, pbase && agxBars(pbase, "eval"));
    document.getElementById("agx-deploy").innerHTML = agxBarsHtml(agxBars(base, "deploy"), "deploy", "deployLabel", "deployStage", AGX_PROGRESS_BY, pbase && agxBars(pbase, "deploy"));
  }

  document.getElementById("agx-pov-card").style.display = AG_POVS.length ? "" : "none";
  if (AG_POVS.length) document.getElementById("agx-pov").innerHTML = agxPovHtml(view);

  document.getElementById("agx-facets").innerHTML = agxFacetsHtml(view);
  agxShown = agxSorted(agxIncFilter(view));
  document.getElementById("agx-acct-h").textContent = agxState.inc.size
    ? `Accounts (${agxShown.length} of ${view.length})` : `Accounts (${agxShown.length})`;
  document.getElementById("agx-sort").value = agxState.sort;
  document.getElementById("agx-list").innerHTML = agxShown.length
    ? agxShown.map(agxCardHtml).join("")
    : `<div class="agx-empty">No accounts match these filters</div>`;
  // Removed for churn tab + the tab badges.
  const rm = agxRemovedRows();
  const rmArr = rm.reduce((t, a) => t + (covNum(a.arr) || 0), 0);
  document.getElementById("agx-n-cov").textContent = view.length;
  document.getElementById("agx-n-churn").textContent = rm.length;
  document.querySelector(".agx-tabs .tab-btn.churn").style.display = AGX_LIVE ? "" : "none";
  document.querySelectorAll(".agx-tabs .tab-btn").forEach(b => b.classList.toggle("active", b.dataset.agxtab === agxTab));
  document.getElementById("agx-tab-cov").hidden = agxTab !== "cov";
  document.getElementById("agx-tab-churn").hidden = agxTab !== "churn";
  document.getElementById("agx-rm-h").textContent = `Removed for churn (${rm.length})` + (rm.length ? ` \u00B7 ${agxUsd(rmArr)} contract ARR` : "");
  document.getElementById("agx-rm-sub").innerHTML =
    `Accounts left off the Coverage tab because a renewal has Account Risk ${esc((AGENTIC_DATA && AGENTIC_DATA.exclude) || "")} &middot; click a column header to sort`;
  document.getElementById("agx-removed").innerHTML = agxRemovedHtml(rm);
  const shownN = agxTab === "churn" ? rm.length : agxShown.length;
  document.getElementById("count-badge").textContent = shownN + " account" + (shownN !== 1 ? "s" : "");
}

// ── Agentic profile by CSM (Engagement > CS Coverage) ──────────────────────
// Follows the Coverage tab's region pills and search. Clicking a row opens
// Adoption > Agentic Engagement Overview filtered to that CSM.
let covAgOpen = true;
function covAgToggle(header) { header.parentElement.classList.toggle("open"); covAgOpen = !covAgOpen; }
function covAgGo(csm) {
  setCSM(csm);
  PAGES.adoption.active = "agentic";
  switchPage("adoption");
}
function covAgenticHtml() {
  const q = searchTerm.toLowerCase();
  const rows = AG_ROWS.filter(a => (activeRegion === "all" || a.region === activeRegion) &&
                                   (!q || a.csm.toLowerCase().includes(q) || a.name.toLowerCase().includes(q)));
  const by = {};
  rows.forEach(a => {
    const r = by[a.csm] = by[a.csm] || { csm: a.csm, n: 0, o: 0, v: 0, h: 0, pov: 0, arr: 0, hasArr: false };
    r.n++; r.o += a.hasOwner; r.v += a.hasVision; r.h += a.held; r.pov += a.povs.length ? 1 : 0;
    if (covNum(a.arr) !== null) { r.arr += covNum(a.arr); r.hasArr = true; }
  });
  const list = Object.values(by).sort((x, y) => y.n - x.n || x.csm.localeCompare(y.csm));
  const hasArr = list.some(r => r.hasArr);
  const bar = (c, n, cls) => `<div class="cov-ag-mini"><div class="agx-track"><div class="agx-fill ${cls}" style="width:${agxPct(c, n)}%"></div></div><b>${agxPct(c, n)}%</b></div>`;
  const tot = list.reduce((t, r) => ({ n: t.n + r.n, o: t.o + r.o, v: t.v + r.v, h: t.h + r.h }), { n: 0, o: 0, v: 0, h: 0 });
  const chip = (label, value) => `<span class="cov-chip"><span class="cov-chip-k">${label}</span>${value}</span>`;
  const body = list.length ? list.map(r =>
    `<tr class="click" data-csm="${esc(r.csm)}" onclick="covAgGo(this.dataset.csm)"><td><b>${esc(r.csm)}</b></td>` +
    `<td class="cov-num">${r.n}</td>${hasArr ? `<td class="cov-num">${agxUsd(r.arr)}</td>` : ""}` +
    `<td>${bar(r.o, r.n, "")}</td><td>${bar(r.v, r.n, "lime")}</td><td>${bar(r.h, r.n, "held")}</td>` +
    `<td class="cov-num">${r.pov || ""}</td>` +
    `<td class="cov-num">${(r.n - r.o) + (r.n - r.v) + (r.n - r.h)}</td></tr>`).join("")
    : `<tr><td colspan="8" class="cov-empty">No accounts in view</td></tr>`;
  return `<div class="card cov-card cov-ag${covAgOpen ? " open" : ""}">
    <div class="card-header" onclick="covAgToggle(this)">
      <div class="card-name">Agentic profile by CSM</div>
      <div class="cov-chips">
        ${chip("Accounts", tot.n)}
        ${chip("Owner known", agxPct(tot.o, tot.n) + "%")}
        ${chip("Vision", agxPct(tot.v, tot.n) + "%")}
        ${chip("Conversation held", agxPct(tot.h, tot.n) + "%")}
      </div>
      <span class="expand-icon">&#9660;</span>
    </div>
    <div class="card-body">
      <div class="agx-sub" style="margin-top:10px">From the agentic profile report, churned accounts excluded &middot; click a row to open that CSM on Adoption &rsaquo; Agentic Engagement Overview</div>
      <table class="flat-table cov-ag-table">
        <thead><tr><th>CSM</th><th class="cov-num">Accounts</th>${hasArr ? `<th class="cov-num">Contract ARR</th>` : ""}
        <th>Owner known</th><th>Vision captured</th><th>Conversation held</th><th class="cov-num">With a PoV</th><th class="cov-num">Gaps</th></tr></thead>
        <tbody>${body}</tbody>
      </table>
    </div>
  </div>`;
}
)---"
