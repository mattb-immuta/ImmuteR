# op_list.json: field guide for agents

This file explains `op_list.json`, the full data dump from Immuta's Customer
Success pipeline (ImmuteR). It is written for an AI agent, such as a Dust agent,
that reads the JSON from the shared Google Drive folder and answers questions
about customer accounts, renewals, churn risk, expansions, consumption and
escalations.

**Agents: read this file before you read `op_list.json`.** Field names have
spaces, punctuation and suffixes such as `.new` and `.old`. Use the names
exactly as written here. Do not guess or rename them.

---

## 1. Where the files are and how fresh they are

- Both files are in the `current_state/` subfolder of the shared ImmuteR Drive
  folder:
  - `current_state/op_list.json`: the data.
  - `current_state/OP_LIST_JSON.md`: this guide.
- Every pipeline run (`scripts/main.R`) overwrites both files. They keep the
  same Drive file IDs, so links to them keep working.
- `updated` (top-level string, such as `"2026-10-15 11:22:42"`) is the time of
  the run. Always tell the user how old the data is when you answer.
- `dates` (string) gives the run date and the comparison snapshot, for example
  `"Run Date: 2026-10-15 , Comparison Date: 2026-10-09.RData"`. Everything under
  `changes` and `changes_detail` is measured against that comparison date.
- The file is about 2.6 MB. If your tools only show you search fragments and not
  the whole file, say so. Do not present a sum, count or "all accounts" list as
  complete unless you have seen the whole table.

---

## 2. Shape and conventions

The file is one JSON object. Most top-level keys are **tables**: arrays of row
objects that all have the same keys. A few keys are nested objects that hold
more tables (`consumption`, `changes_detail`, `constants`), and a few are
scalars (`updated`, `dates`).

| Convention | What it means |
|---|---|
| `"-"` | Salesforce had no value. Treat it as missing, the same as `null`. |
| `null` | The value is missing or not computed. It is **not** zero and **not** false. |
| Dates | ISO `YYYY-MM-DD` strings. Timestamps in `escalations` are ISO datetimes. |
| Money | Plain numbers in USD everywhere (`Amount`, `Contract ARR Rollup`, `Churn Forecast`, `ARR`, `Total`, `Expected to Churn`, order prices, and so on). Salesforce's `"USD 792,500.00"` strings are converted when the data is pulled. A blank amount is `null`. Format money yourself when you display it. |
| Booleans | JSON `true`/`false` (`Is SaaS Customer`, `multiplatform`, and so on). |
| Picklist ratings | Strings that start with a colour word: `Black`, `Red`, `Amber` or `Green`, followed by `:` or `-` and a label. Case varies (`"amber: moderate adoption"`). Match on the colour word, case-insensitively. From worst to best: Black, Red, Amber, Green. |
| Empty tables | `[]`. This means nothing qualified. It does not mean data is missing. |

### Join keys

- **`Account ID`** is the 15-character Salesforce account ID, such as
  `0011K00002A9Goo`. This is the main key between `accounts`, `renewals`,
  `expansions`, `forecast`, `support` and `consumption.low_consumption`.
- **`Account ID 18`** is the 18-character form of the same ID. It appears on
  `accounts`, `renewals` and `expansions`.
- **`Account Name`** is the fallback key, and the **only** key for most
  `consumption.*` tables, `escalations`, `saas_pipeline` and `orders`. Match
  names exactly first. If that fails, match case-insensitively.
- **`Opportunity ID`** identifies an opportunity, such as `006VV00000V5BSR`. It
  joins `renewals` ↔ `forecast` ↔ `customer_ops_tracker`.
- **`key`** identifies a Jira escalation, such as `CUSTX-1234`.

### Fiscal calendar

Immuta's fiscal year starts on **1 February**. Quarter labels look like
`Qn-YYYY`, where YYYY is the calendar year in which the fiscal year **ends**:

| Calendar months | Label (FY ending Jan 2027) |
|---|---|
| Feb–Apr 2026 | `Q1-2027` |
| May–Jul 2026 | `Q2-2027` |
| Aug–Oct 2026 | `Q3-2027` |
| Nov 2026–Jan 2027 | `Q4-2027` |

So 16 October 2026 falls in `Q3-2027`.

**Watch out:** in `renewals`, `Fiscal Period` is `null` for every row outside
the fiscal periods the pipeline is configured to track (about half the rows).
To bucket renewals by quarter, use **`churn_forecast_period`**: the fiscal
quarter of the day after the contract ends, which is when the revenue would be
lost. Every renewal has one.

---

## 3. Top-level tables

Row counts are approximate and change from run to run.

### `accounts` (~70 rows): one row per active customer account

This is the main account table. Use it for "tell me about account X" and for
any per-account roll-up.

| Field | Meaning |
|---|---|
| `Account ID`, `Account ID 18`, `Account Name` | Identity. |
| `Contract ARR Rollup` | Current contract ARR in USD (number). |
| `Region` | One of `US - W`, `US - E`, `EMEA`, `APJ`, `Public Sector`. |
| `Customer Success Manager`, `Account Owner`, `Field CTO` | The account team. |
| `Is SaaS Customer` | `true` for SaaS, `false` for self-managed. |
| `Customer on Unsupported Version` | Self-managed account on an unsupported release. |
| `Lifecycle Stage` | `S1: Installation and Config.` → `S2. MVP in prod…` → `S3: greater than 75% license consumption` → `S4: Multi-Sales Play Adoption`. |
| `PBAC Adoption`, `Comply Adoption`, `Request Adoption`, `Agentic Governance Adoption` | Salesforce picklist ratings, set by hand. Each one starts with a colour word. |
| `request_adoption_looker_stage`, `request_adoption_looker` | The Request adoption stage measured from Looker usage (`tried`/`adopted`/`retained`), and the rating derived from it. **Prefer these to the hand-set `Request Adoption`.** |
| `request_adoption_match` | Whether the hand-set picklist and the Looker-derived rating agree. |
| `comply_adoption_looker_stage`, `comply_adoption_looker`, `comply_adoption_match`, `comply_export_enabled`, `comply_export_usage` | The same idea for Comply. |
| `product_adoption` | Score from 0 to 10: PBAC complexity score (0–7) plus Request points (3 for retained, 1 for adopted, otherwise 0). |
| `product_adoption_rating` | Overall health stoplight that combines `product_adoption` with the consumption ratio, such as `green: strong adoption`, `amber: moderate adoption`, `Red: High Risk - Adoption`, `Red: High Risk - Consumption`, `Black: Critical Churn Risk`. **This is the best single health signal.** |
| `multiplatform` | More than one data platform in Production. |
| `obo_sessions`, `iam`, `ext_catalog` | On-behalf-of session usage, the identity provider, and the external catalog. |
| `executive_buyer`, `champion` | Contact names. When there are several, they are separated by `"; "`. `null` means nobody holds the role. |
| `Executive Buyer Relationship`/`Influence`, `Champion Relationship`/`Influence` | Strong, neutral or weak, from Salesforce. |
| `Date of last EBR`, `Date of next EBR`, `EBR Note`, `Date Last QBR`, `Date Next QBR`, `QBR Note` | Business review dates and notes. Notes often start with an `M/D -` date stamp. |
| `Agentic Owner - Customer`, `Title`, `Customer Agentic Vision`, `Agentic Architecture` | AI/agentic notes. These can contain `<br>` and newlines. |
| `customer_since`, `months_as_customer` | Tenure. |
| `slack_channel`, `slack_channel_id` | The customer's internal Slack channel. |
| `account_url`, `consumption_url`, `customer_central_url` | Links to Salesforce, the Looker consumption dashboard and Customer Central. Include them in answers. |

### `renewals` (~90 rows): open renewal opportunities

Each row has all the account fields above, plus:

| Field | Meaning |
|---|---|
| `Opportunity ID`, `Opportunity Name`, `opportunity_url` | Identity and link. |
| `Amount` | Renewal amount in USD (number, can be `null`). |
| `Churn Forecast` | USD the CSM forecasts will **churn** (number). `0` means no churn is expected. |
| `Account Risk` | `Expecting to Renew`, `Verbally Confirmed`, `At Risk Renewal`, `Expected to Churn` or `Closed Won`. |
| `Stage` | `0. Not Yet Started`, `1. Conversation Started`, … `5. Paperwork Executed`. |
| `Days in Current Stage` | Number. |
| `Current Contract End Date`, `Close Date`, `rev_lost_day` | Contract end, expected close, and contract end + 1 day. |
| `churn_forecast_period` | Fiscal quarter of `rev_lost_day`. **Use this to bucket by quarter.** |
| `Churn Quarter`, `Fiscal Period` | From Salesforce. `Fiscal Period` is often `null` (see section 2). |
| `Next Step` | Free-text status, usually with an `M/D -` date stamp. Quote it when someone asks for status. |
| `Priority` | `Tier I`/`II`/`III` or `"-"`. |
| `Adoption Phase`, `RD Comment`, `Probability (%)` | `Probability (%)` is a string here, such as `"0%"`. |

### `expansions` (~20 rows): open upsell opportunities in the tracked periods

The fields are the same as for renewals, plus:

- `expected_revenue` = `Amount` × probability.
- `Probability (%)` is a **number from 0 to 1** here, unlike in renewals.
- `days_until_close` counts from the run date.
- `consumption_risk` is `true` when the deal closes within 120 days and
  consumption is below 60%.

### `new_logos` (~40 rows): new-business prospects

These are opportunities that don't belong to existing customers yet. Most of
them **do not appear in `accounts`**. Fields: `Opportunity Name`, `Account Name`,
`Amount`, `Probability (%)` (0–1), `expected_revenue`, `Stage`, `Close Date`,
`days_until_close`, `Opportunity Owner`, `Region`, and
`opportunity_type = "new_logo"`.

### `forecast` (~90 rows): the churn-forecast view of renewals

There is one row per renewal opportunity, joined on `Opportunity ID`. Fields:
`Account Risk`, `Churn Forecast`, `Expected to Churn`, `ARR`, `Total` (all USD
numbers), `CS Notes`, `Next Step`, `Stage`, `Close Date`, `Churn Quarter`,
`Fiscal Period`.

### `customer_ops_tracker` (~90 rows)

A legacy, slimmed-down list of renewals with `total_amount` and
`active_expansions`. Prefer `renewals`.

### `csm_table` (~70 rows) and `csm_coverage` (~9 rows)

- `csm_table`: which CSM owns each account, with ARR, owner, Field CTO and
  region.
- `csm_coverage`: one row per CSM, with `accounts`, `arr`,
  `pbac_complexity` (total complexity score), `s1_onboarding` (number of
  accounts still in S1), `request_retained`, `active_request` and
  `ongoing_agentic`.

### `support` (~70 rows)

Support load per account: `cases_90d`, `cases_360d`, `cases_total`,
`escalations_90d`, `escalations_360d`, `escalations_open`.

### `escalations` (~60 rows): open CUSTX Jira escalations

Fields: `key`, `summary`, `severity` (`Highest` > `High` > `Medium` > `Low`;
`severity_rank` sorts on this), `status`, `status_category`, `assignee`,
`created`, `updated`, `age_days`, `url`, `Account Name`, `Region`, `csm`.

### `saas_pipeline` (~20 rows)

Self-managed customers migrating to SaaS: `SaaS Migration Pipeline Stage`,
`Est. Date to Move Out of Stage (SM)`, `Estimated SaaS Migration Period`,
`SaaS Migration Pipeline Notes`.

### `orders` / `active_orders` (~450 rows)

The product lines on active orders: `Product: Product Name`, `Quantity`,
`Unit Price`, `Total Price`, and contract end dates. In `active_orders` the
account column is called `Account Name: Account Name`.

### `contacts` (~50 rows) and `incident_response_contacts` (~220 rows)

Normalised fields: `contact_name`, `contact_email`, `contact_title`,
`contact_account_id`, `contact_account_name`, `contact_role`,
`is_executive_buyer`, `is_champion` and `is_incident_response`. Only `contacts`
feeds the `executive_buyer` and `champion` fields on `accounts`.

### `customer_tenure` (~70 rows)

`tenure_account_id`, `customer_since`, `months_as_customer`. These values are
already joined onto `accounts`.

### `constants`

- `arr_index`: ARR tier breakpoints in USD, such as `[0, 150000, 500000]`.
- `churn_plan`: the company's churn plan by fiscal quarter
  (`fiscal.period`, `company_plan` in USD). Compare it with the sum of
  `renewals.Churn Forecast` for the same `churn_forecast_period`.

---

## 4. `consumption`: product usage from Looker (Production instances only)

Most of these tables are keyed by `Salesforce Accounts Account Name` and have
**one row per instance**. Use `low_consumption` when you want one row per
account.

| Table | Contents |
|---|---|
| `low_consumption` | **One row per account** (keyed by `Account ID`). `Registered Users`, `Licensed Users` (`0` means an ELA with unlimited seats), `Consumption Ratio` (registered ÷ licensed, a number), `Data Sources Count`, `Active Policies (Total)`, `Immuta Version`, `platform_prod`, `platform_dev`, `multiplatform`, `connections_*`. `Low Consumption` is `true` when the account has no Production instance, a ratio below 0.25, or zero active policies. `null` means unknown. |
| `pbac_adoption` | One row per account: `multi_platform`, `dns_policies`, `sdd`, `domains`, `guardrail` (each 0/1), `complexity_score`, `consumption_ratio`, `request_stage`, `request_points`, `product_adoption`, `product_adoption_rating`. |
| `users` | Seat usage compared with contracted seats, by instance. |
| `policy_metrics` | Active global policies by type, by instance. |
| `data_sources` | Data source counts by platform (Snowflake, Databricks, …), by instance. |
| `sdd_consumption` | Sensitive data discovery: tag counts and `Days since last SDD run`. |
| `obo_sessions` | On-behalf-of / agent session metrics. |
| `comply` | Feature flags and integrations per instance. |
| `metrics` | `platform_count` per account. |
| `connections` | Connections migration progress: `connections_upgraded` / `connections_total`. |

---

## 5. What changed since the last run

- **`changes`** gives one summary row per component, with `Added`, `Removed`,
  `Changed` and `Total_Moves`. Start here for "what changed this week?".
- **`changes_detail`** holds the detail. `changes_detail.diff_date` is the
  comparison date. Each component (`accounts`, `renewals`, `expansions`,
  `new_logos`, `csm_table`, `saas_pipeline`, `escalations`, `consumption`,
  `forecast`) has:
  - `added` / `removed`: full rows that appeared or disappeared.
  - `field_changes`: one row per changed record. For every changed field there
    is a pair of columns, **`<field>.new`** and **`<field>.old`**, plus boolean
    flags such as `risk_changed`, `forecast_changed`, `amount_changed`,
    `date_changed` and `stage_changed`. `forecast_delta` is the new churn
    forecast minus the old one.
  - `csm_table.reassignments`: accounts that changed CSM.
  - `consumption.field_changes` also has `user_drop_pct`, `user_drop`,
    `ds_drop` and `pol_drop`. `consumption.all_deltas` has data-source and
    policy deltas for every account.
- **`renewal_changes`** is a legacy summary of renewal risk and forecast moves.

---

## 6. Where to look for common questions

| Question | Where to look |
|---|---|
| "How is account X doing?" | `accounts` (rating, lifecycle, adoption, QBR) + `renewals` (by `Account ID`) + `support` + `escalations` (by `Account Name`) + `consumption.low_consumption`. |
| "What's at risk this quarter?" | `renewals` where `churn_forecast_period` is the current quarter and `Account Risk` is `At Risk Renewal` or `Expected to Churn`. Sum `Churn Forecast`. |
| "How do we compare to the churn plan?" | `constants.churn_plan` against summed `renewals.Churn Forecast` by `churn_forecast_period`. |
| "Which accounts have low usage?" | `consumption.low_consumption` where `Low Consumption == true`, or `accounts.product_adoption_rating` starting with Red or Black. |
| "What's in [CSM]'s book?" | `csm_coverage` for the totals, then `accounts` filtered on `Customer Success Manager`. |
| "What changed since last week?" | `changes`, then `changes_detail.<component>`. |
| "Open escalations for X?" | `escalations` filtered on `Account Name`, sorted by `severity_rank` and then `age_days`. |
| "Expansion pipeline?" | `expansions` (sum `Amount` and `expected_revenue`). |
| "Who is the exec buyer/champion?" | `accounts.executive_buyer` / `champion`, then `contacts` for titles and emails. |

### How to answer

- Name accounts exactly as `Account Name` has them, and include
  `account_url` or `opportunity_url` when they are relevant.
- Format money as USD with thousands separators.
- Quote `Next Step` and QBR/EBR notes as written. Do not paraphrase status into
  something more certain than the note says.
- State the `updated` timestamp. If you filtered or sampled the data, say so.
- This data is internal customer information. Do not repeat contact emails or
  ARR outside the conversation that asked for them.

---

## 7. How the files get here (for humans)

`scripts/main.R` writes `output/current_state/op_list.json` with
`jsonlite::write_json(op_list, auto_unbox = TRUE, na = "null")`. It copies this
guide from the repo root (`OP_LIST_JSON.md`) to
`output/current_state/OP_LIST_JSON.md`, then `push_output_to_drive()` uploads
both files to the shared folder. To change this guide, edit `OP_LIST_JSON.md`
at the repo root, not the copy in `output/`. Section 3 lists the tables as of
October 2026. If a new top-level key is added to `op_list` in
`enrich_opportunities()`, add it here too.
