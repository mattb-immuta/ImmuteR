# Dust frame payload — field reference

Generated 2026-09-30 09:44 from op_list. 72 accounts, 5 regions, 10 CSMs.

Six files: `accounts.json`, `changes.json`, `consumption.json`,
`escalations.json`, `saas.json` and this dictionary.

Values are **presentation-ready**: render them as-is. The `*_raw` fields are
the only ones to compute with — sort, filter and plot on those.

## `accounts.json`

An array of these objects, sorted by `name`.

| Field | Type | Example | Notes |
|---|---|---|---|
| `id` | string | `Acorns_Grow_Incorporated` | Slug of the account name. Safe for DOM ids and React keys. |
| `name` | string | `Acorns Grow Incorporated` | Account name. The join key everywhere else. |
| `account_id` | string | `0011K00002A9Goo` | Salesforce 15-character account ID. |
| `customer_central_url` | string | `https://customer-central.immuta.support/custo…` | Link to Customer Central, or "" when absent. |
| `region` | string | `US - W` | One of the five regions. A facet. |
| `arr` | string | `$205,000` | Contract ARR, pre-formatted ("$205,000"). |
| `arr_raw` | number | `205000` | Contract ARR as a number. Sort and size on this, not `arr`. |
| `csm` | string | `John Timberlake` | Customer Success Manager. A facet. |
| `account_owner` | string | `Cory Phillips` |  |
| `field_cto` | string | `Samantha Hamernick` |  |
| `consumption_link` | string | `<a class="consumption-link" href="https://imm…` | A full <a> tag, not a URL. Insert as HTML or skip it. |
| `is_saas` | boolean | `TRUE` | TRUE for SaaS, FALSE for self-managed. |
| `unsupported` | boolean | `FALSE` | Self-managed account running an unsupported version. |
| `churn_summary` | string | `Q2-2028` | Comma-separated fiscal quarters with churn-forecast risk. |
| `lifecycle_stage` | string | `S3: greater than 75% license consumption` | Salesforce lifecycle picklist. |
| `pbac_adoption` | string | `Amber: Production consumption at greater than…` | Salesforce PBAC picklist. Fallback only — prefer `pbac_rating`. |
| `pbac_rating` | string | `amber: moderate adoption` | Looker-derived PBAC rating, "<colour>: <label>". The one to show. |
| `pbac_score` | number | `3` | Weighted complexity score, 0-7. The scatter's y axis. |
| `pbac_features` | string | `SDD · Domains · Data + subscription policies` | Complexity signals earned, joined with " · ". |
| `comply_adoption` | string | `Red - Feature not enabled` | Salesforce Comply picklist. Colour word leads the string. |
| `request_adoption` | string | `Amber: Use case in production, enabling data …` | Salesforce Request picklist. Hand-maintained. |
| `request_adoption_derived` | string | `Amber: Use case in production, enabling data …` | Looker-derived Request tier. Prefer this over the picklist. |
| `request_status` | string | `adopted` | tried / adopted / retained / none — the Looker stage. |
| `agentic_governance_adoption` | string | `Red: Customer expressed interest in POC or de…` | Salesforce Agentic picklist. |
| `platform_html` | string | `Databricks` | Platforms as HTML; dev-only ones are wrapped in a grey span. |
| `multiplatform` | boolean | `FALSE` | More than one platform in Production. |
| `registered_users` | string | `221` | Formatted count, or an em-dash. |
| `licensed_users` | string | `210` | Formatted count. "0" means an ELA (unlimited seats). |
| `consumption_ratio` | string | `105.2%` | Pre-formatted percentage, or an em-dash. |
| `consumption_ratio_raw` | number | `1.05238095238095` | The ratio as a number, or null. The scatter's x axis. |
| `immuta_version` | string | `saas` | Deployed version, "saas" for SaaS accounts. |
| `inc_90d` | string | `3` | Support cases in the last 90 days. |
| `inc_360d` | string | `14` | Support cases in the last 360 days. |
| `custx_90d` | string | `2` | Escalations in the last 90 days. |
| `custx_open` | string | `1` | Escalations currently open. |
| `custx_90d_raw` | number | `2` |  |
| `custx_open_raw` | number | `1` |  |
| `connections` | string | `29,010 / 29,010` | Connections migration per Production instance, "12 / 40". "None" means nothing upgraded; several instances are comma-separated and named. |
| `connections_raw` | number | `1` | Upgraded over total across the account, or null. Sort on this. |
| `executive_buyer` | string | `—` | Contact name(s) from the Salesforce contacts report; several are "; "-separated. Em-dash when nobody holds the role. |
| `buyer_relationship` | string | `—` | The executive buyer's relationship. From the account report. Strong / neutral / weak. |
| `buyer_influence` | string | `—` | The executive buyer's influence. From the account report. Strong / neutral / weak. |
| `champion` | string | `—` | Contact name(s) from the Salesforce contacts report; several are "; "-separated. Em-dash when nobody holds the role. |
| `champion_relationship` | string | `—` | The champion's relationship. From the account report. Strong / neutral / weak. |
| `champion_influence` | string | `—` | The champion's influence. From the account report. Strong / neutral / weak. |
| `last_qbr` | string | `July 22, 2026` | Formatted date, or an em-dash. |
| `next_qbr` | string | `—` | Formatted date, or an em-dash. |
| `qbr_note` | string | `Onsite Q1.  Remote Q2 QBR on 7/22. Targeting …` | Free text from Salesforce. |
| `renewals` | array[1] | see below | Array of open renewal opportunities. Often empty. |
| `expansions` | array[0] | see below | Array of open expansion opportunities. Often empty. |
| `products` | array[5] | see below | Array of {name, quantity} from the active order. |

## Nested shapes

```json
{"renewals":[{"name":"Cummins - DBX+SYN - Q4FY27 Renewal","url":"https://immuta.lightning.force.com/lightning/r/Opportunity/006VH000003Wdvl/view","amount":"$550,032","amount_raw":550032,"end_date":"January 21, 2027","end_date_raw":"2027-01-21","churn_quarter":"Q4-2027","risk":"At Risk Renewal","next_steps":"-"}],"expansions":[{"name":"Cummins - Expansion","url":"https://immuta.lightning.force.com/lightning/r/Opportunity/006VV00000Pg6E6/view","owner":"Jonathan King","amount":"$325,000","amount_raw":325000,"fiscal_period":"Q4-2027","close_date":"January 22, 2027","close_date_raw":"2027-01-22","next_steps":"9/29 - We are meeting with Anu on October 6."}],"products":[{"name":"Connector - Databricks","quantity":"1"}]}
```

## Rating strings

Every adoption rating is `"<colour>: <label>"` or `"<Colour> - <label>"`,
where colour is one of `green`, `amber` (sometimes `yellow`), `red`, `black`.
Read the colour off the front for the stoplight; the rest is the definition,
which belongs in a tooltip rather than the cell. Severity order is
green > amber > red > black.

## PBAC rating grid

`pbac_rating` is not stored per axis — it is `pbac_score` crossed with
`consumption_ratio_raw`:

|                | ratio < 0.25 | 0.25–0.75 | ratio ≥ 0.75 |
|---|---|---|---|
| **score ≥ 4** | red   | amber | green |
| **score < 4** | black | red   | amber |

A null `consumption_ratio_raw` means the figure is unavailable, which is not
the same as zero: such an account reaches red on complexity alone and no
further. `licensed_users` of `"0"` is an ELA, already scored as a ratio of 1.

## `escalations.json`

`{ has_data: boolean, rows: [...] }`. One row per **open** CUSTX Jira issue,
already sorted by severity then age. `has_data: false` with an empty `rows`
means the pipeline ran without Jira credentials — that is a different empty
state from "your filters match nothing", and the two must read differently.

| Field | Type | Example | Notes |
|---|---|---|---|
| `key` | string | `CUSTX-3068` | Jira issue key. The diff key in `changes.json`. |
| `summary` | string | `P1: Shell - Data sources are locked down afte…` | Issue title. Usually leads with the customer name. |
| `severity` | string | `Highest` | Jira priority name, instance-specific (`P1`, `Sev 1`, `Highest`). |
| `status` | string | `Waiting for Customer` | Workflow status name. A local facet. |
| `status_category` | string | `In Progress` | Jira status category. Always not-Done — open issues only. |
| `assignee` | string | `Claire Foster` | Jira assignee display name, or null when unassigned. |
| `created` | string | `2026-04-17` | Date only, `YYYY-MM-DD`. |
| `updated` | string | `2026-09-29` | Date only, `YYYY-MM-DD`. |
| `Account Name` | string | `Shell` | Joined to an account, or null when no match was found. |
| `url` | string | `https://immuta.atlassian.net/browse/CUSTX-3068` | Browse URL for the issue. Render the key as a link to it. |
| `age_days` | number | `166` | Days since `created`. Flag at 90+; that is the stale threshold. |
| `severity_rank` | number | `1` | Normalised urgency, 1 (most) to 5, 9 = unrecognised. Sort on this; clamp to 1-4 for colour. |
| `Region` | string | `EMEA` | From the account, not from Jira. Null when unmatched. |
| `csm` | string | `Noha Badawy` | From the account, not from Jira. Null when unmatched. |

Unlike the account records these values are **raw, not pre-formatted**: a
missing account is `null`, not an em-dash. Escalations come straight from
Jira rather than through the account record builder, so this one file is the
exception to the presentation-ready rule — supply your own placeholder here.

Sort on `severity_rank` (ascending), then `age_days` (descending). Do not
sort on the `severity` label: it is whatever the Jira instance calls its
priorities, so it is not alphabetically ordered by urgency.

## `saas.json`

`{ has_data: boolean, rows: [...] }`. The SaaS migration pipeline, one row
per account in flight, with `csm` joined on from the account record.

| Field | Type | Example | Notes |
|---|---|---|---|
| `Account Name` | string | `Activision Blizzard` |  |
| `SaaS Migration Pipeline Stage` | string | `2 - Migration in Progress` |  |
| `Est. Date to Move Out of Stage (SM)` | string | `2026-10-16` |  |
| `Estimated SaaS Migration Period` | string | `Q3-2027` |  |
| `SaaS Migration Pipeline Notes` | string | `9/29 - Onboarding data, users,  and polices. …` |  |
| `Region` | string | `US - W` |  |

## `changes.json`

`{ has_data, diff_date, summary, <component>... }`. Each component is
`{ added, removed, field_changes }` — except `csm_table`, which has
`reassignments` in place of `field_changes`. The components are:

`accounts`, `renewals`, `expansions`, `forecast`, `csm_table`,
`saas_pipeline`, `escalations`.

`field_changes` rows carry paired `<field>.old` / `<field>.new` columns;
render them as `old → new`. The `escalations` component is keyed on the Jira
issue `key`, so `added` is an escalation **raised** since the last snapshot
and `removed` is one that **closed** — a close is good news, not a deletion,
and should not be styled as a loss.

