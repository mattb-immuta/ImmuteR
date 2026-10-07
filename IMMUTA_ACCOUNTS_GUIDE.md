# Immuta Customer Accounts — Reader's Guide

`immuta_accounts.html` is a single, self-contained web page that brings together everything Customer Success tracks about each Immuta customer: contract and renewal data from Salesforce, product usage from Looker, support history, open escalations from Jira, and the SaaS migration pipeline. It's meant to answer three questions:

1. **Who are our customers and what's moving?** (Accounts and Engagement)
2. **What are they actually using?** (Adoption)
3. **How is it running?** (Performance)

This guide explains how to read the page. You don't need any special tools, just open the file in a browser.

---

## Getting the page and knowing how fresh it is

- **Updated date (top right).** This is when the underlying data was pulled, not when the file was opened or saved. If it looks old, the numbers are old.
- **Download Update.** Downloads the latest published copy from Google Drive. Use this to get current data.
- **Refresh.** Re-runs the data pipeline. This only works on the owner's machine (it needs a local R process running), so other readers should use **Download Update** instead. If you click Refresh without that process running, a pop-up explains what's needed. You can close it.
- **Compare to.** Shows which earlier snapshot the **Changes** tab is compared against. The newest snapshot is selected by default. Picking a different date only takes effect when the owner refreshes the page.

The page works offline once downloaded. All data is embedded in the file.

---

## Controls that work everywhere

| Control | What it does |
|---|---|
| **Search box** | Filters by account name on every tab. On Escalations it also matches the issue summary and Jira key. |
| **Region / CSM pills** | Narrow the view to one region or one Customer Success Manager. These selections are shared. If you pick "EMEA" on Accounts, it stays selected when you move to Renewals, Adoption, and so on. |
| **Clickable account names** | In any table, clicking an account name takes you to that account's detail card on the Accounts tab and opens it. |
| **Sortable columns** | Click a column header to sort, and click it again to reverse. |
| **Count badges** | The numbers on each tab show how many rows are in view after your filters are applied. |

### Conventions

- **— (em-dash)** means the value is missing or not set in the source system. It doesn't mean zero.
- **Stoplight colors** are used for every rating on the page:
  🟢 **Green** (healthy) → 🟡 **Amber** (moderate) → 🔴 **Red** (at risk) → ⚫ **Black** (no meaningful adoption).
- **Hover over** a rating badge to see the full definition text from the source system.
- **Fiscal quarters.** Immuta's fiscal year starts in **February**, and quarters are named for the calendar year the fiscal year *ends* in. For example, March 2026 falls in **Q1-2027**, and Nov 2026 to Jan 2027 is **Q4-2027**.

---

## 1. Accounts and Engagement

### Accounts

One card for every customer account. You can sort them **Name (A–Z)** (with an A–Z jump bar) or **End Date** (grouped by the fiscal quarter of the account's next renewal, with the ARR renewing in each quarter).

**Card header (collapsed view):**

- **Four-square glyph.** A quick read of adoption: PBAC, Request, Comply, and Agentic, each colored by its stoplight rating. Hover a square to see its label.
- **Account name.** An **(M)** after the name means the account runs Immuta on more than one platform in Production.
- **ARR.** Contract ARR rollup.
- **Links.** *Current Consumption ↗* (Looker) and *Customer Central ↗*.
- **Tags.** Region, deployment (**SaaS**, **SM: Supported**, or **SM: Unsupported**, where SM = self-managed), and the churn quarter(s) of open renewals.

Click a card to expand it. It has four tabs:

**Overview** has five columns:

| Column | Contents |
|---|---|
| Account basics | Contract ARR, Region, CSM, Account Owner, Field CTO |
| Adoption: evidence | Lifecycle Stage, PBAC rating with its evidence (platforms in use, PBAC features in use, complexity score out of 7), Registered vs. Licensed users, and the **consumption ratio** (registered ÷ licensed). The ratio is colored red below 25%, amber from 25% to 50%, and green at 50% or above. |
| Adoption: ratings | A small PBAC plot showing where this account sits (see the PBAC section below), plus the Request, Agentic, and Comply ratings |
| Performance | Deployment type, Immuta version, Connections migration progress, support incidents in the last 90 and 360 days, CustX escalations in the last 90 days, open escalations (highlighted if any), and a tile for each open escalation linking to Jira |
| Engagement | Executive Buyer and Champion, each with relationship strength and influence (Strong, Neutral, or Weak), last and next QBR dates, and the latest QBR note |

On the Platforms line, platforms shown in **grey** exist only in Dev or Test, not Production.

**Architecture** shows SaaS or self-hosted, an *Unsupported Version* warning where it applies, and the active products and quantities on order.

**Renewals** lists open renewal opportunities with amount, contract end date, churn quarter, and next steps. Opportunity names link to Salesforce.

**Expansions** lists open expansion opportunities with owner, amount, fiscal period, and next steps.

### Renewals

A single table of every open renewal across all accounts. Summary figures at the top show the number of open renewals, total ARR, and the number of accounts. By default it's sorted by **End Date** and grouped under churn-quarter headings with subtotals. Sorting by any other column gives a flat list. The **Risk** column is colored High, Moderate, or Low.

### Expansions

The same layout for open expansion opportunities, with summary figures for the number of expansions, pipeline value, and number of accounts. When sorted by Close Date, rows are grouped by fiscal quarter.

### Changes

Shows what's different since the snapshot named in **Compare to**. The header gives the comparison date and a count for each area. Each section can be collapsed and uses badges: **+ added**, **− removed**, **~ changed**.

| Section | What it tracks |
|---|---|
| Accounts | New or dropped accounts, and changes to ARR, CSM, owner, Field CTO, region, adoption phase, or Request stage |
| Renewals | New or closed renewals, and changes to risk, churn forecast, amount, or close date |
| Expansions | New or closed expansions, and changes to amount or fiscal period |
| Churn Forecast | Opportunities entering or leaving the churn forecast, field changes, and the **net change in forecast churn dollars** (an increase is shown in red) |
| CSM Assignments | Accounts reassigned between CSMs |
| SaaS Pipeline | Migrations added, removed, or updated |
| Escalations | Jira escalations raised, closed, or changed in severity or status |

Changed values are shown as **old → new**.

> In the Churn Forecast section, "added" or "removed" usually means a deal's churn quarter moved into or out of the forecast window. It doesn't necessarily mean a deal was created or deleted.

---

## 2. Adoption

There's one sub-tab per product module. The Region and CSM pills at the top apply to all four.

### PBAC (Policy-Based Access Control)

PBAC is rated on two separate measures and then combined, because login counts alone don't say much about adoption.

**The scatter plot:**

- **X axis: consumption ratio**, registered users ÷ licensed users. Accounts above 1.25 are placed in a separate column at the right edge.
- **Y axis: complexity score** (0–7), built from five yes/no signals:

  | Signal | Points |
  |---|---|
  | Multi-platform (more than one platform in Production) | 3 |
  | Both data and subscription policies active | 1 |
  | Sensitive Data Discovery run in the last 90 days | 1 |
  | Domains in use (governance is delegated) | 1 |
  | At least one active guardrail policy | 1 |

  A score of **4 or more** counts as "complex." Only Production instances count.

- **Background shading** shows the rating each region of the plot earns:

  | | Ratio < 0.25 | 0.25 – 0.75 | Ratio ≥ 0.75 |
  |---|---|---|---|
  | **Score ≥ 4** | Red: High Risk - *Consumption* | Amber | Green |
  | **Score < 4** | Black: Critical Churn Risk | Red: High Risk - *Adoption* | Amber |

**Reading a point:**

- **Size** reflects contract ARR.
- **Square** markers are accounts still in Installation and Config. (lifecycle S1), where low usage is expected.
- **Dashed outline** means the consumption ratio is unknown. The point is placed at zero only so it can be drawn.
- **Heavy outline** means the ratio is above the axis maximum.
- **Dimmed** points fall outside your current filters. They stay on the plot so you can see where your selection sits relative to the whole customer base.
- **Hover** over a point to see its details in the side panel. **Click** to pin it there.

**What the two reds mean:**

- **High Risk - Consumption** (high complexity, few users): the platform is set up but people aren't using it. This calls for enablement.
- **High Risk - Adoption** (low complexity, some users): people are using it, but only for simple work. This calls for deepening use cases.
- **Black: Critical Churn Risk** accounts have cleared neither threshold and need the most urgent attention.

**How missing data is handled:**

- A missing complexity signal counts as zero. For example, no Sensitive Data Discovery run means no point for that signal.
- A missing ratio stays unknown and isn't treated as zero. On complexity alone, such an account can reach red at most.
- **Zero licensed users means an ELA (unlimited seats).** The ratio is treated as 100%.

The full method is written out below the plot on the page itself.

### Request, Comply, Agentic

These are tables of rated accounts, with summary figures showing the number of accounts, total ARR, and how many are green, amber, red, or black. Sorting by the rating column orders accounts by severity.

- **Request** is rated from Looker usage stages (*None → Tried → Adopted → Retained*). The rating entered in Salesforce is shown next to it, so you can spot where the two disagree.
- **Comply** and **Agentic** (Agentic Governance) come from the Salesforce account record.

If a tab says "no data," either the data wasn't pulled in this refresh or your filters exclude every rated account. The message tells you which.

---

## 3. Performance

### Self-Managed

All customers who aren't on SaaS, with their Immuta version, **support status** (Supported, Unsupported, or Unknown), lifecycle stage, registered and licensed users, consumption ratio, data source count, and active policy count. You can filter by support status and lifecycle stage.

Rows that are **highlighted** are flagged **Low Consumption**, meaning at least one of these is true:

- The account has no Production instance.
- The consumption ratio is below 25%.
- The Production instance has zero active policies.

### SaaS Migration

A board of self-managed customers moving to SaaS, with **one column per estimated migration quarter**. The current quarter is marked **· now**, and migrations without a scheduled quarter appear in an **Unscheduled** column.

- Each card shows the account, region, CSM, pipeline stage (*Pipeline → Having conversations → Verbal commitment → Migration in progress*), and the date it's expected to move to the next stage. That date turns red with **(past)** if it has passed. Click a card to read its notes.
- **Assumed churn** tags mark customers who are expected to leave rather than migrate.
- **TGT SaaS %** at the top of each column is the projected share of customers on SaaS by the end of that quarter:
  *(SaaS customers today + migrations due by that quarter) ÷ (all customers − expected churn by that quarter)*.
  It follows the Region and CSM filters but ignores the search box.
- The summary figures at the top show **SaaS today** (the current share) and how many migrations are at each stage.

### Escalations

Open CustX Jira issues, sorted by severity and then by age (oldest first). You can filter by severity, status, region, and CSM. Each row shows the Jira key (linked), summary, account, status, assignee, and when it was opened. Issues open for **90 days or more** are highlighted as stale. "No account matched" means the Jira issue couldn't be tied to a Salesforce account.

---

## Where the data comes from

| Area | Source |
|---|---|
| Accounts, renewals, expansions, products, contacts, QBRs, churn forecast, SaaS pipeline | Salesforce reports |
| Users, consumption ratio, platforms, PBAC signals, Request stage, versions, connections | Looker |
| Incident counts (90d / 360d) | Support data warehouse (BigQuery) |
| Escalations | Jira (CUSTX project) |
| "What changed" | A comparison with the saved snapshot from an earlier run |

If a figure looks wrong, check the source system first. The page displays the data as it was pulled and doesn't edit it. For questions or corrections, contact Matt Benigni.
