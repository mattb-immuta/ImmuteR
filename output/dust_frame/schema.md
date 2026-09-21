# Dust frame payload — field reference

Generated 2026-09-21 10:38 from op_list. 72 accounts, 5 regions, 10 CSMs.

`accounts.json` is an array of these objects, sorted by `name`.
Values are **presentation-ready**: render them as-is. The `*_raw` fields are
the only ones to compute with — sort, filter and plot on those.

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
| `is_saas` | boolean | `True` | TRUE for SaaS, FALSE for self-managed. |
| `unsupported` | boolean | `False` | Self-managed account running an unsupported version. |
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
| `multiplatform` | boolean | `False` | More than one platform in Production. |
| `registered_users` | string | `218` | Formatted count, or an em-dash. |
| `licensed_users` | string | `210` | Formatted count. "0" means an ELA (unlimited seats). |
| `consumption_ratio` | string | `103.8%` | Pre-formatted percentage, or an em-dash. |
| `consumption_ratio_raw` | number | `1.03809523809524` | The ratio as a number, or null. The scatter's x axis. |
| `immuta_version` | string | `saas` | Deployed version, "saas" for SaaS accounts. |
| `inc_90d` | string | `2` | Support cases in the last 90 days. |
| `inc_360d` | string | `13` | Support cases in the last 360 days. |
| `custx_90d` | string | `2` | Escalations in the last 90 days. |
| `custx_open` | string | `1` | Escalations currently open. |
| `custx_90d_raw` | number | `2` |  |
| `custx_open_raw` | number | `1` |  |
| `last_qbr` | string | `July 22, 2026` | Formatted date, or an em-dash. |
| `next_qbr` | string | `—` | Formatted date, or an em-dash. |
| `qbr_note` | string | `Onsite Q1.  Remote Q2 QBR on 7/22. Targeting …` | Free text from Salesforce. |
| `renewals` | array[1] | see below | Array of open renewal opportunities. Often empty. |
| `expansions` | array[0] | see below | Array of open expansion opportunities. Often empty. |
| `products` | array[5] | see below | Array of {name, quantity} from the active order. |

## Nested shapes

```json
{
 "renewals": [
  {
   "name": "Cummins - DBX+SYN - Q4FY27 Renewal",
   "url": "https://immuta.lightning.force.com/lightning/r/Opportunity/006VH000003Wdvl/view",
   "amount": "$550,032",
   "amount_raw": 550032,
   "end_date": "January 21, 2027",
   "end_date_raw": "2027-01-21",
   "churn_quarter": "Q4-2027",
   "risk": "At Risk Renewal",
   "next_steps": "-"
  }
 ],
 "expansions": [
  {
   "name": "Cummins - Expansion",
   "url": "https://immuta.lightning.force.com/lightning/r/Opportunity/006VV00000Pg6E6/view",
   "owner": "Jonathan King",
   "amount": "$325,000",
   "amount_raw": 325000,
   "fiscal_period": "Q4-2027",
   "close_date": "January 22, 2027",
   "close_date_raw": "2027-01-22",
   "next_steps": "9/16 - We are meeting with Anu on October 6."
  }
 ],
 "products": [
  {
   "name": "Connector - Databricks",
   "quantity": "1"
  }
 ]
}
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
