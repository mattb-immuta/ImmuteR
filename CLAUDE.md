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
`LOOKER_CLIENT_ID`, `LOOKER_CLIENT_SECRET`, `PRODUCTBOARD_API_TOKEN` (only needed if
`productboard = TRUE`), `SLACK_BOT_TOKEN` (only needed to post with `push_slack_snitch(dry_run = FALSE)`). `LOOKER_BASE_URL` and `IMMUTER_PROJECT_ROOT`/`IMMUTER_DIFF_DATE` are optional
overrides, the latter two set automatically by `serve.R`/`api.R`, not by the user.

## Architecture

### The `op_list` object is the spine of the whole package

Almost every function either builds `op_list`, enriches it, or reads from it. It's a plain named list, not
an S4/R6 class, assembled in stages:

1. **`enrich_opportunities()`** (R/enrich_opportunities.R) is the pipeline entry point. It runs several
   Salesforce reports (via `salesforcer::sf_run_report`, report IDs are passed in / defaulted), splits the
   opportunity report into `renewals` and `expansions` by Record Type, and calls `enrich_renewals()` /
   `enrich_expansions()` to join account context onto each. It always builds `op_list$consumption` (via
   `op_list_consumption()`), optionally pulls live Looker metrics into it (`looker = TRUE`, requires
   `load_op_list_raw()` from R/looker_utils.R + R/op_list_consumption.R), and optionally builds
   `op_list$product_board` (`productboard = TRUE`, requires R/productboard_utils.R +
   R/op_list_product_board.R — this is the slow path, ~1 API call per Productboard note).

2. **`enrich_list_object()`** (R/enrich_list_object.R, the largest/most complex function in the package)
   takes the freshly built list, loads the previous run's saved `.RData` snapshot for a given `diff_date`,
   and diffs every component (accounts, renewals, expansions, csm_table, saas_pipeline) to detect adds/
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
- **Looker**: R/looker_utils.R (`looker_auth()`, `run_looker_look()`) — OAuth client-credentials flow,
  looks are referenced by numeric ID (see `.OP_LIST_LOOKS` in R/op_list_consumption.R).
- **Productboard**: R/productboard_utils.R + R/op_list_product_board.R — company name matching between
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
  dry-run by default and is run by hand, not from `main.R`.
- **Local inputs**: `data/input/` holds hand-maintained files read on every run (critical_technologies.json,
  slack_channels.csv, slack_users.csv, snitch_slack_template.md) — they are inputs, unlike the generated `output/`.

### `working_scratch/` and `output/`

`working_scratch/` holds prototype/debug scripts (not sourced by the pipeline) — treat as scratch, not
as documentation of current behavior. `output/` and `reports/` are generated artifacts (HTML dashboards,
account briefs, Rmarkdown renders); don't hand-edit files there, regenerate them from the R functions
that produce them (`report_all_accounts_html()`, `output_markdown_accounts()`, etc.).
