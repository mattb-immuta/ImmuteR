# ImmuteR

An R package that fuses Salesforce, Looker (via `idex`), and Google Sheets to support Immuta's customer success workflows. It pulls and enriches opportunity, renewal, expansion, and consumption data; generates risk signals; and pushes outputs to Google Sheets, Rmarkdown reports, and the HTML dashboard.

---

## Installation

```r
# Install devtools if needed
install.packages("devtools")

# Install all package dependencies
devtools::install_deps()
```

See [NEW_MACHINE_SETUP.md](NEW_MACHINE_SETUP.md) for a full walkthrough including auth setup for Salesforce, Google Sheets, and Looker.

---

## Prerequisites

| Requirement | Notes |
|---|---|
| R 4.4+ | [cran.r-project.org](https://cran.r-project.org/) |
| RStudio | Open project via `ImmuteR.Rproj` |
| Salesforce access | Token cached in `.httr-oauth-salesforcer`; re-auth with `sf_auth()` |
| Google account | Auth via `gs4_auth(email = "...")`; token cached in `~/.config/gargle/` |
| Looker credentials | Set `LOOKER_CLIENT_ID` and `LOOKER_CLIENT_SECRET` in `.Renviron` |
| `idex` CLI | Required for consumption data; invoked via `uvx idex` |

---

## How It Works

The typical workflow follows three stages:

**1. Pull & Enrich** — `enrich_opportunities()` is the main entry point. It queries several Salesforce reports, enriches renewals and expansions with account-level context, and optionally pulls Looker consumption data via the `idex` CLI. The result is a named list (`op_list`) that flows into all downstream functions.

**2. Diff & Detect Changes** — `enrich_list_object()` compares the current `op_list` against a saved snapshot (`.RData`) to surface what changed: accounts added or removed, risk changes, CSM reassignments, etc.

**3. Output** — A family of `output_*` and `report_*` functions push results to Google Sheets, generate Rmarkdown/HTML tables, and build per-account summaries.

---

## Key Functions

### Data Pull & Enrichment

| Function | Description |
|---|---|
| `enrich_opportunities()` | Main entry point. Pulls Salesforce reports, enriches renewals and expansions, and returns an `op_list` with elements: `renewals`, `expansions`, `accounts`, `active_orders`, `csm_table`, `saas_pipeline`, and `orders`. |
| `enrich_renewals()` | Joins account attributes onto the renewal table; derives `rev_lost_day` and `churn_forecast_period`. |
| `enrich_expansions()` | Joins account attributes onto expansions; derives expected revenue, deal size, and `consumption_risk`. |
| `enrich_consumption()` | Reads idex parquet exports (users, queries, datasources, policies) into a metrics summary keyed on account name. Optionally re-runs the `idex` CLI to refresh. |
| `enrich_list_object()` | Diffs `op_list` against a prior saved snapshot; appends `changes`, `changes_detail`, `renewal_changes`, and `customer_ops_tracker`. |
| `load_constants()` | Returns ARR tier breakpoints and the company churn plan by fiscal period. |

### Account Summaries

| Function | Description |
|---|---|
| `output_account_summary()` | Builds a structured summary (account metadata, open renewals/expansions, architecture/SaaS status, active products) for a single named account. |
| `summarize_account()` | Similar to above; includes adoption metrics (consumption rates, user counts, policy usage). |
| `report_account_summary()` | Renders an account summary to HTML, RTF, or Markdown. HTML can be opened in a browser and copied into any rich-text tool. |
| `report_all_accounts()` | Returns a data frame of all accounts with ARR, CSM, region, and concatenated churn quarters. |
| `report_all_accounts_html()` | Generates a self-contained HTML dashboard of all accounts with search, region filter, A–Z jump bar, and expandable detail cards. |
| `output_markdown_accounts()` | Writes per-account `.md` files to `output/account_briefs/`. |

### Google Sheets Outputs

| Function | Description |
|---|---|
| `output_gsheet_mbpipeline()` | Writes renewals and expansions to the MB pipeline Google Sheet. |
| `output_gsheet_customer_ops_tracker()` | Writes the customer ops tracker (renewals + risk signals) to Google Sheets. |
| `output_gsheet_BU_staging()` | Writes per-quarter renewal tabs and a change log to the BU staging sheet. |

### Rmarkdown / HTML Table Outputs

| Function | Description |
|---|---|
| `output_renewals_rmd()` | Returns a `reactable` HTML table of renewals for a given churn period. |
| `output_adoption_risk_rmd()` | Returns a `reactable` HTML table of renewals filtered by adoption risk signals. |
| `output_customer_ops_tracker()` | Returns a merged renewal + account table for the customer ops tracker report. |
| `output_expansion_tracker()` | Returns a merged expansion + account table for the expansion tracker report. |
| `output_BU_summary()` | Returns a data frame comparing churn forecast vs. company plan by fiscal period. |
| `generate_bu_tables()` | Joins expansion totals onto the renewal table to produce the customer ops tracker dataset. |

### Forecasting

| Function | Description |
|---|---|
| `generate_forecast()` | Monte Carlo churn forecast by fiscal quarter. "Expected to Churn" and "At Risk Renewal" accounts are treated as Bernoulli variables with configurable churn probabilities; "Closed Lost" and "Verbally Confirmed" are treated as certain. Returns `n` simulations per quarter. |

### Utilities

| Function | Description |
|---|---|
| `date_to_fiscal_quarter()` | Converts dates to Immuta fiscal quarter labels (FY starts February; e.g. March 2026 → `"Q1-2027"`). |
| `extract_usd_amount()` | Strips Salesforce currency formatting (`"USD 1,250,000"` → `1250000`). |
| `get_active_orders()` | Fetches active Salesforce orders for a given account name. |
| `get_products_by_order_number()` | Fetches line items for a given Salesforce order number. |

---

## Fiscal Calendar

Immuta's fiscal year begins in **February**. Quarter labels follow the year in which the fiscal year *ends*:

| Fiscal Quarter | Calendar Months | Example |
|---|---|---|
| Q1 | Feb – Apr | March 2026 → Q1-2027 |
| Q2 | May – Jul | June 2026 → Q2-2027 |
| Q3 | Aug – Oct | September 2026 → Q3-2027 |
| Q4 | Nov – Jan | December 2026 → Q4-2027 |

---

## Package Structure

```
ImmuteR/
├── R/                        # All exported functions
│   ├── enrich_*.R            # Data pull and enrichment
│   ├── output_*.R            # Output functions (Sheets, Rmd, HTML)
│   ├── report_*.R            # Account summary renderers
│   ├── generate_*.R          # Forecast and BU table generators
│   ├── push_*.R              # Google Drive publishing
│   └── utils_internal.R      # Internal helpers (not exported)
├── man/                      # Roxygen-generated documentation
├── working_scratch/          # Scratch scripts and prototypes
├── shiny/app.R               # Shiny app
├── DESCRIPTION               # Package metadata and dependencies
├── NEW_MACHINE_SETUP.md      # Setup guide for a new machine
└── README.md                 # This file
```

---

## Dependencies

Core imports: `googlesheets4`, `data.table`, `salesforcer`, `dplyr`, `stringr`, `reactable`, `htmltools`, `gt`, `scales`, `arrow`, `tidyr`, `purrr`, `rmarkdown`, `httr2`

Suggested: `devtools`, `testthat`, `kableExtra`, `plotly`, `knitr`, `plumber`
