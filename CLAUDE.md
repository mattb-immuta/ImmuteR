# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

ImmuteR is an R package (not a generic script collection) that fuses Salesforce, Looker, and Productboard
data for Immuta's customer success workflows. It pulls and enriches opportunity/renewal/expansion/consumption
data, generates risk and churn-forecast signals, and pushes outputs to Google Sheets and Rmarkdown/HTML
reports. There is a single owner/user (Matt) running this interactively from RStudio — it is not deployed
as a service beyond the local Plumber refresh API described below.

## Commands

```r
# Install/update dependencies from DESCRIPTION
devtools::install_deps()

# Load all package functions in dev mode (no install step) — do this before
# running any script or testing a function interactively
devtools::load_all()

# Regenerate man/ docs and NAMESPACE from roxygen comments after changing a
# function's roxygen block (@param, @export, etc.)
devtools::document()

# Full package check (R CMD check equivalent)
devtools::check()
```

There is no test suite (`testthat` is listed in Suggests and `Config/testthat/edition: 3` is set in
DESCRIPTION, but there is no `tests/` directory yet). Don't assume `devtools::test()` will find anything.

**Running the pipeline:**

```r
devtools::load_all()
source("scripts/main.R")   # runs the full pull → enrich → diff → output pipeline
```

`main.R` must be run with the working directory at the project root (relative paths like `data/` and
`output/` depend on it). It auto-detects the most recent `data/opportunities_*.RData` snapshot to diff
against unless `IMMUTER_DIFF_DATE` is set.

**Dashboard + refresh server** (two-process setup — keep the R session running):

```r
source("scripts/serve.R")   # starts Plumber on http://localhost:8765, exposes /refresh, /status, /snapshots
```

Then open the generated HTML file in `output/` (from `report_all_accounts_html()`) and use its Refresh
button, which calls `/refresh` on the Plumber server, which re-sources `scripts/main.R`.

**Auth** (tokens are cached after first use, so this is normally a one-time step per machine — see
NEW_MACHINE_SETUP.md for full details):

```r
salesforcer::sf_auth()
googlesheets4::gs4_auth(email = "matt.benigni@immuta.com")
```

Required environment variables (set in `.Renviron` via `usethis::edit_r_environ()`):
`LOOKER_CLIENT_ID`, `LOOKER_CLIENT_SECRET`, `PRODUCTBOARD_API_TOKEN` *or*
`PRODUCTBOARD_CLIENT_ID` + `PRODUCTBOARD_CLIENT_SECRET` (registered OAuth app, browser approval on first use,
tokens cached by httr2; only needed for Productboard pulls — check with `pb_check_connection()`), `SLACK_BOT_TOKEN` (only needed to post with `push_slack_snitch(dry_run = FALSE)`). `LOOKER_BASE_URL` and `IMMUTER_PROJECT_ROOT`/`IMMUTER_DIFF_DATE` are optional
overrides, the latter two set automatically by `serve.R`/`api.R`, not by the user.

## Architecture

### The `op_list` object is the spine of the whole package

Almost every function either builds `op_list`, enriches it, or reads from it. It's a plain named list, not
an S4/R6 class, assembled in stages:

1. **`enrich_opportunities()`** (R/enrich_opportunities.R) is the pipeline entry point. It runs several
   Salesforce reports (via `salesforcer::sf_run_report`, report IDs are passed in / defaulted), splits the
   opportunity report into `renewals`, `expansions` and `new_logos` (Record Type `"New Business"`) by
   Record Type, and calls `enrich_renewals()` / `enrich_expansions()` to join account context onto the
   first two. `enrich_new_logos()` only filters to `fiscal.period` and adds numeric fields — new logos are
   prospects, mostly absent from the account report, so the dashboard shows them from their own
   `NEW_LOGOS_DATA` payload (`.build_new_logos_json()`) rather than hanging them off `ACCOUNTS`. It always builds `op_list$consumption` (via
   `op_list_consumption()`), optionally pulls live Looker metrics into it (`looker = TRUE`, requires
   `load_op_list_raw()` from R/looker_utils.R + R/op_list_consumption.R), and optionally builds
   `op_list$product_board` (`productboard = TRUE`, requires R/productboard_utils.R +
   R/productboard_insights.R + R/op_list_product_board.R). `pb_pull_insights()` pulls all notes
   with relationships inline plus companies/users/features as entity types, caching to
   `data/productboard/pb_insights_<date>.rds`; `pb_match_accounts()` joins companies to Salesforce
   by Account ID, then by normalised name.

2. **`enrich_list_object()`** (R/enrich_list_object.R, the largest/most complex function in the package)
   takes the freshly built list, loads the previous run's saved `.RData` snapshot for a given `diff_date`,
   and diffs every component (accounts, renewals, expansions, new_logos, csm_table, saas_pipeline) to detect adds/
   removes/field changes/CSM reassignments. It appends `changes`, `changes_detail`, plus the legacy
   `renewal_changes` and `customer_ops_tracker` elements for backward compatibility. This diff-against-
   snapshot pattern is how the package surfaces "what changed since last run" without a database.

3. A family of `output_*()` / `report_*()` functions consume the finished `op_list` to produce Google
   Sheets writes, reactable/HTML tables, and per-account Markdown/HTML/RTF summaries.
   These are read-only with respect to `op_list` — none of them mutate it.

Because of this structure, when adding a new data source, the pattern to follow is: fetch/shape it in its
own `enrich_*`/`op_list_*` helper, attach it as a new top-level key in `op_list` from within
`enrich_opportunities()`, then add diffing support in `enrich_list_object()` if "what changed" tracking
matters for it, then add an `output_*`/`report_*` consumer.

### Execution model: scripts/ vs R/

Only `R/` is the package proper (exported via `NAMESPACE`, documented via roxygen into `man/`).
`scripts/main.R` is explicitly *not* part of the package namespace — it's the interactively-run
orchestration script that supplies config (Salesforce report IDs, Google Sheet URLs, fiscal periods),
calls `devtools::load_all()`, then calls into the package's exported functions in sequence
(`enrich_opportunities()` → `enrich_list_object()` → various `output_*`/`report_*` calls → save new
snapshot). `scripts/serve.R` + `scripts/api.R` are a thin Plumber wrapper that re-sources `main.R` on
demand for the dashboard's Refresh button; they communicate the project root via the
`IMMUTER_PROJECT_ROOT` env var since Plumber's working directory isn't guaranteed to be the project root.

### Adoption ratings come from metrics, not Salesforce

`PBAC Adoption`, `Request Adoption` and `Comply Adoption` on `op_list$accounts` are overwritten in
`enrich_opportunities()` by `apply_metric_adoption()` (R/op_list_salesforce_updates.R) with the metrics
ratings: PBAC from `consumption$pbac_adoption`, Request from Looker dashboard 1009, Comply from look 1814
feature flags (`comply_enabled` → Amber, `access_summary_export` → Red, else Black; +1 product adoption
point per flag). The hand-maintained originals are kept as `<field> (SFDC)`, and
`op_list$salesforce_updates` lists every field where SFDC disagrees by colour (written to the
`salesforce_updates` tab of the op_list inspector sheet in main.R). Agentic Governance is still SFDC.

### Fiscal calendar

Immuta's fiscal year starts in February. `date_to_fiscal_quarter()` (R/date_to_fiscal_quarter.R) is the
canonical conversion — the label uses the calendar year the fiscal year *ends* in (e.g. March 2026 is
`"Q1-2027"`). Fiscal period constants/plan numbers live in `load_constants()` (R/load_constants.R,
`arr_index` ARR tier breakpoints and `churn_plan` by fiscal period) — update this when the churn plan
changes rather than hardcoding numbers elsewhere.

### Formatting/rendering helpers are centralized and unexported

R/utils_internal.R holds the shared, non-exported helpers (`safe_val`, `fmt_usd`, `fmt_date`, `fmt_days`,
`fmt_bool`, `fmt_risk`, `build_field`, `html_escape`, `html_li`) used across the
`report_*`/`output_*` functions to keep Markdown/HTML rendering consistent
(e.g. every missing value renders as an em-dash, not blank or `NA`). New report/output functions should
reuse these rather than reimplementing formatting inline.

### External integrations and where their logic lives

- **Salesforce**: via `salesforcer`, report-ID based (`sf_run_report()`), not SOQL queries directly.
  Always call the internal wrapper `.sf_run_report()` (R/extract_usd_amount.R), never
  `salesforcer::sf_run_report()` directly: it runs `convert_usd_columns()`, which turns every
  `"USD 792,500.00"` column into a number, so currency in `op_list` is always numeric. Parse any
  other currency value with `extract_usd_amount()` rather than ad-hoc `gsub()`.
  `enrich_list_object()` applies the same conversion to old snapshots before diffing them.
- **Looker**: R/looker_utils.R (`looker_auth()`, `run_looker_look()`) — OAuth client-credentials flow,
  looks are referenced by numeric ID (see `.OP_LIST_LOOKS` in R/op_list_consumption.R).
- **Productboard**: R/productboard_utils.R + R/productboard_insights.R + R/op_list_product_board.R
  (v2 API; companies and users are entity types under `/v2/entities`, not their own endpoints).
  One-off corpus EDA: reports/productboard_insights_eda.Rmd. Company name matching between
  Productboard and Salesforce is fuzzy (case/punctuation/suffix-insensitive; see `.pb_normalize_name()`)
  since the two systems' names aren't guaranteed identical; unmatched companies are tracked explicitly
  rather than silently dropped.
- **Google Sheets**: via `googlesheets4`, functions named `output_gsheet_*`.
- **Slack**: `data/input/slack_channels.csv` is a hand-maintained dictionary of each customer's internal
  Slack channel (`Account Name`, `Account ID 18`, `slack_channel`, `slack_channel_id`), read by
  `op_list_slack_channels()` and joined onto `op_list$accounts` in `enrich_opportunities()` (matched on
  `Account ID 18` first, name second). `push_slack_snitch()` (R/push_slack_snitch.R) takes
  `report_next_step_snitch()` output and posts one message per flagged account to its channel via raw
  `httr2` calls to `chat.postMessage` (bot token `SLACK_BOT_TOKEN`), @-mentioning the CSM from
  `data/input/slack_users.csv`. Message wording lives in `data/input/snitch_slack_template.md` (Markdown,
  `{{placeholders}}`, optional `if`/`end` comment blocks) — edit the template/CSVs, not the code. It is
  dry-run by default and is run by hand, not from `main.R`. The alternative route with no bot token
  is `output_dust_snitch()` (R/output_dust_snitch.R): it builds the same messages via the shared
  `.snitch_build()` and writes `output/dust_snitch/snitch_messages_{test,live}.json` for a Dust agent to
  post with Dust's Slack tool (posting as the user). `test_channel = "#tmp-cs-test"` redirects all
  messages there and pings the tester instead of CSMs. Agent setup + instructions: DUST_SNITCH_AGENT.md.
- **Contacts**: two Salesforce reports, both read by `op_list_contacts()` (R/op_list_contacts.R).
  `00OVV000006TwOX2A0` (executive buyer / champion) -> `op_list$contacts`, rolled up onto
  `op_list$accounts$executive_buyer` / `$champion` (shown on the dashboard and the health-summary PNGs).
  `00OVV000007LaNp2AK` (incident response, `role = "incident_response"`) -> `op_list$incident_response_contacts`,
  the mail-merge list; kept separate so it never feeds the role rollup.
- **Agentic profile**: report `00OVV000007LHjN2AW` is pulled raw into `op_list$agentic_profile` by
  `enrich_opportunities()` (`sf_agentic_report`). `.ao_build()` (R/report_agentic_owner_html.R) rolls it up
  per account and drops churned accounts (Account Risk Verbally Confirmed / Will Churn); both
  `report_agentic_owner_html()` and the dashboard's Adoption > Agentic Engagement Overview tab (`AGENTIC_DATA`, via
  `.build_agentic_json()`) use it. The by-CSM agentic table is on Engagement > CS Coverage. The tab's CSS, panel
  markup and JS live once in R/agentic_overview_assets.R (`.AGX_CSS` / `.AGX_PANEL` / `.AGX_JS`) and are filled into
  both the dashboard and `output/agentic_owner_summary.html`, so edit them there, not in either page template.
  Agentic PoV projects (report `00OVV000007QLj72AG`) come in as `op_list$agentic_pov` via `op_list_agentic_pov()`
  (`sf_agentic_pov_report`): the account is read from the project name "PoV - <Account>" and matched to
  `accounts` (exact, normalised, unique prefix; unmatched kept with `matched = FALSE`). They ride in
  `AGENTIC_DATA$povs` and show as the "Agentic PoVs" card, a PoV pill + block on each account card, a KPI, a
  "Has an agentic PoV" filter, and columns on the Removed for churn and CS Coverage by-CSM tables.
  `.accounts_add_obo_target()` turns the same list into `op_list$accounts$OBO_revenue_TGT` (logical: TRUE = account
  has a PoV project, i.e. an OBO session revenue target; NA if the PoV pull failed), shown as an "OBO target" tag
  on the dashboard account cards.
  `enrich_list_object()` also diffs the agentic rollup (`.diff_agentic()`, R/diff_agentic.R) into
  `changes_detail$agentic`: accounts added / removed from the Coverage tab, per-account agentic field changes, and
  the previous rollup (`prev`, `prev_povs`). The page recounts `prev` under the current filters to put ▲/▼ deltas
  on every roll-up number (KPIs, funnel, status bars), lists the changes in a "What changed" card, and the
  dashboard's Changes tab gets an "Agentic Engagement" section from the same `agxChangesHtml()`.
- **Customer tenure**: `customer_since` / `months_as_customer` on `accounts` come from report `00OVV000007MTSL2A4`
  (`op_list_customer_tenure()`, earliest contract start). That report currently returns only current contracts, so
  `.accounts_add_vitally_tenure()` overrides it with Vitally's `Vitally Days as Customer` (opportunity report)
  wherever Vitally is longer; `accounts$tenure_source` records which won. Remove the stopgap once the report carries
  full contract history.
- **Local inputs**: `data/input/` holds hand-maintained files read on every run (critical_technologies.json,
  slack_channels.csv, slack_users.csv, snitch_slack_template.md) — they are inputs, unlike the generated `output/`.
  `obo_pocs.csv` (active On-Behalf-Of POCs + completion estimate, `FY28-Q1` style) feeds
  `reports/agentic_revenue_analysis.Rmd` only; it is not read by the pipeline.
- **Data dictionary**: `report_op_list_dictionary()` (R/report_op_list_dictionary.R, run from `main.R`) writes
  `output/op_list_dictionary.html` (+ `.csv`): every op_list object and field with source, type, fill rate, example and
  description. Structure comes from `op_list`; descriptions and sources from `data/input/op_list_dictionary.csv`
  (`object`, `field`, `description`, `source`; `*` wildcards). When adding a new op_list key or field, add a row there.

### `reports/`, `working_scratch/` and `output/`

`reports/` holds every standalone Rmarkdown report (FY28 churn forecast HTML + PDF, agentic revenue analysis,
adoption risk by region PDF (`adoption_risk_by_region.Rmd`: Black/Red `product_adoption_rating`, a lifecycle Stage 1 page,
then one page per region),
account transition review, Productboard EDA, the legacy CustOps report). Each one finds the project root itself
(`..` from `reports/`, or a walk up to DESCRIPTION) and knits its output next to the source unless its YAML says
otherwise; shared model code stays in `scripts/` (e.g. `scripts/FY28_churn_forecast_model.R`). The Rmds not
there are in `inst/guide/`: `immuta_accounts_guide.Rmd` (`report_user_guide()`), `cs_doctrine.Rmd` (the CS
Doctrine 2.0, a hand-copied, unsynced version of the Google Doc; `report_cs_doctrine()`) and `csm_reporting.Rmd`
(that doc's CSM reporting requirements, split out; `report_csm_reporting()`). All are loaded with
`system.file()`, so they must stay in `inst/`, and share `c360_doc.css` + `c360_doc_head.html` (topbar, hero, one
`::: {.card #sec-...}` per section; the left `.sidenav` is built at load time from each card's h2/h3, so it needs no
editing; `.embedded` mode when framed). `report_all_accounts_html()` embeds all three
knitted pages as base64 in the dashboard's **Documentation** tab (sub-tabs CS Doctrine, CSM Reporting, User Guide; iframes, plus a link to the CSD Confluence space), so `main.R`
knits them first.

`working_scratch/` holds prototype/debug scripts (not sourced by the pipeline) — treat as scratch, not
as documentation of current behavior. `output/` is generated artifacts (HTML dashboards, account briefs) and is
git-ignored (it carries customer data and changes every run); don't put anything there that can't be regenerated, and
don't hand-edit files there, regenerate them from the R functions that produce them
(`report_all_accounts_html()`, `output_markdown_accounts()`, etc.).
