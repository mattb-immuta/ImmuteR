# Dust agent: rebuild the accounts dashboard as a frame

An experiment. `report_all_accounts_html()` produces `output/immuta_accounts.html`
— a hand-written single-file dashboard, roughly 3,000 lines of R holding HTML,
CSS and JS. This asks a Dust agent to produce the same thing as a **frame** (an
interactive React view) from the same data, so the two can be compared
side by side.

The question being tested is not "can an LLM write React". It is whether an
agent handed presentation-ready data and a written spec lands close enough to a
deliberately-designed dashboard that the R generator stops earning its keep.

## What Dust needs

**Capability:** the agent needs the **Create Frames** skill (Agent Builder →
Add capabilities → Create Frames).

**Files:** upload the six files from `output/dust_frame/`, written by
`output_dust_payload(op_list)` on every pipeline run:

| File | Contents | Size |
|---|---|---|
| `accounts.json` | One object per account, sorted by name | ~180 KB / 72 accounts |
| `changes.json` | Snapshot diff — what moved since the last run | ~12 KB |
| `consumption.json` | Consumption rows, for the Self-Managed tab | ~500 KB |
| `escalations.json` | Open CUSTX Jira issues, one row each | ~15 KB |
| `saas.json` | SaaS migration pipeline, one row per account in flight | ~8 KB |
| `schema.md` | Field dictionary + a worked example | ~9 KB |

Upload `schema.md` too, and tell the agent to read it first. An agent handed
180 KB of JSON will skim it and invent field names; the dictionary is what stops
that. All six are well inside Dust's 50 MB upload limit, and `accounts.json` is
around 47k tokens — large but not unreasonable for a single context.

`accounts.json` is the same array the R page embeds as its `ACCOUNTS` constant,
from the same builder (`.rah_account_records()`). `escalations.json` and
`saas.json` likewise come from `.build_escalations_json()` and
`.build_saas_json()`, the same functions that fill the page's `ESCALATIONS_DATA`
and `SAAS_DATA`. The comparison is therefore clean: both surfaces render
identical records, so every difference is a rendering difference.

`escalations.json` can legitimately be empty. The Jira pull needs `JIRA_EMAIL`
and `JIRA_TOKEN` in `.Renviron`; without them `enrich_opportunities()` warns and
carries on, and the file lands as `{ has_data: false, rows: [] }`. Check that
before concluding the agent dropped the tab.

## The one invariant

**The data is already formatted. Render it, do not re-derive it.**

`arr` is `"$205,000"`. `consumption_ratio` is `"103.8%"`. Missing values are
already em-dashes (`—`), not `null`, `NA` or blank. `platform_html` is HTML.
The `*_raw` fields exist for sorting, filtering and plotting and are the only
numbers worth computing with.

An agent that re-formats currency, recomputes ratios, or renders `NA` where the
payload said `—` has failed the test regardless of how the layout looks, because
the numbers on screen have stopped agreeing with every other artefact the
pipeline produces.

**`escalations.json` is the single exception** and is called out as such in
`schema.md`: it comes straight from Jira rather than through the account record
builder, so its missing values are real `null`s and the agent supplies its own
placeholder. Everything else follows the rule.

## Agent instructions

Paste this into the Dust agent's instructions.

---

You build an interactive dashboard as a frame, from JSON files given to you in
the conversation. Read `schema.md` before writing any code.

**Data contract**

- `accounts.json` — array of account objects, already sorted by `name`.
- `changes.json` — the snapshot diff.
- `consumption.json` — consumption rows, joined on `Account Name`.
- `escalations.json` — `{ has_data, rows }`, open Jira issues.
- `saas.json` — `{ has_data, rows }`, the SaaS migration pipeline.

Every string value in `accounts.json`, `changes.json` and `consumption.json` is
presentation-ready. Render it as given. Never reformat currency, never recompute
a percentage, never substitute your own placeholder for a missing value — the
payload already uses an em-dash (`—`) for those. Sort, filter and plot on the
`*_raw` fields only.

`escalations.json` is the exception: its values are raw, and a missing account,
region, CSM or assignee is `null`. Supply your own placeholder there.

Never invent a field name. If something you want is not in `schema.md`, it does
not exist; leave that element out rather than guessing.

**Layout**

A top bar with the title, a search box, and three page tabs. Each page has its
own row of sub-tabs beneath, and each sub-tab shows a count:

1. **Engagement** — the default. Sub-tabs: Accounts, Renewals, Expansions,
   Changes.
2. **Adoption** — sub-tabs: PBAC, Request, Comply, Agentic.
3. **Performance** — sub-tabs: Self-Managed, SaaS Migration, Escalations.

Each page remembers which sub-tab it was last on, so returning to a page lands
where you left rather than resetting to the first sub-tab.

- **Accounts** — a left sidebar with two facet groups, Region and CSM, each a
  row of pills with an "All" option. Main area is one collapsible card per
  account, alphabetical.
- **Renewals** — flat sortable table of every account's `renewals`, flattened,
  with the account name on each row.
- **Expansions** — the same for `expansions`.
- **Changes** — what moved since the previous snapshot, as collapsible sections,
  one per component (see below).
- **Self-Managed** — accounts where `is_saas` is false, with their consumption
  figures.
- **SaaS Migration** — `saas.json` laid out as one column per estimated fiscal
  quarter. The question this answers is "what lands when", so the quarter is the
  axis and the migration stage is something a card carries, not the other way
  round.
- **Escalations** — see its own section below.

The search box and the Region/CSM facets are global: a selection made on one
sub-tab still applies when you switch to another, on any page.

**Account card**

Collapsed, a card shows: account name, region, CSM, ARR, and a deployment badge
(`SaaS`, or `SM: Supported` / `SM: Unsupported` from `is_saas` and
`unsupported`). Expanded, it opens on an Overview tab alongside Architecture,
Renewals and Expansions tabs. The Overview is **five columns**:

1. **Commercial and account team** — `arr`, `region`, `csm`, `account_owner`,
   `field_cto`. No heading.
2. **Adoption — evidence.** `lifecycle_stage`, then the PBAC row: a coloured dot
   labelled from `pbac_rating`, with `platform_html` (insert as HTML),
   `pbac_features` and `pbac_score` out of 7 indented beneath it behind a rule.
   Then `registered_users`, `licensed_users`, `consumption_ratio`.
3. **Adoption — ratings.** A per-account PBAC plot (below), then the Request,
   Agentic and Comply stoplight rows.
4. **Performance** — the deployment badge, `immuta_version`, `inc_90d`,
   `inc_360d`, `custx_90d`, `custx_open`, then one small tile per open
   escalation.
5. **Engagement** — two contact roles, then `last_qbr`, `next_qbr`, `qbr_note`.
   Each role is a name row with its relationship and influence indented
   beneath it, behind a rule, the same way the PBAC evidence is indented:
   `executive_buyer` with `buyer_relationship` and `buyer_influence`, then
   `champion` with `champion_relationship` and `champion_influence`.

Adoption is deliberately split across two columns rather than compressed into
one: what was measured on the left, what was concluded from it on the right.
Keep both headed "Adoption" so they read as one block.

**The per-account PBAC plot** is the Adoption tab's scatter at card size with
only this account drawn: same six bands, same axes, one point. Compute the
bands with the same rule as the big plot rather than copying a colour table —
a small chart that quietly disagrees with the large one is worse than no chart.
The same outlier rules apply: a ratio above 1.25 pins to the right lane, a null
ratio draws at the left edge with a dashed outline. Hold the point a radius
inside the frame so an account at score 7 or ratio 0 is not bisected by the
border. Accounts failing `hasPbac` get a short "no PBAC rating" line, not an
empty box.

**The escalation tiles** come from `escalations.json` filtered on
`Account Name`, sorted the way the Escalations tab sorts. Each tile is a link
to `url` showing the severity chip, `key`, `age_days` (red at 90+) and a
two-line-clamped summary. An account whose `custx_open_raw` is non-zero but has
no rows means the Jira pull did not run — say so rather than rendering nothing,
because "no tiles" otherwise reads as "nothing open".

**The six contact-role fields are not in Salesforce yet.** `executive_buyer`,
`buyer_relationship`, `buyer_influence`, `champion`, `champion_relationship`
and `champion_influence` are already in the account record and arrive as
em-dashes until the fields exist upstream. Render both roles now; they will
fill themselves in. Render them with one shared function rather than two — the
roles carry identical fields and must stay comparable, and a third role is
likely. The four strength fields will carry `strong` / `neutral` / `weak` —
map those onto the green / amber / red stoplight the rest of the card already
uses, and show an unrecognised value as plain text rather than dropping it.

**Stoplights**

Every adoption rating is a string beginning with a colour word — `green`,
`amber` (occasionally `yellow`, treat as amber), `red`, `black`. Read the colour
off the front and render a filled dot in it. The rest of the string is the
picklist definition: put it in a `title` tooltip, or on a second line, not in
the cell. Severity order is green > amber > red > black; a rating column sorts
in that order, never alphabetically.

Two modules are rated from Looker rather than Salesforce and should prefer the
derived field, falling back to the picklist when it is an em-dash:

- **PBAC** — use `pbac_rating`, labelled with its own text ("Moderate
  adoption"). Falls back to `pbac_adoption`.
- **Request** — use `request_adoption_derived`, labelled with `request_status`
  capitalised ("Retained"). Falls back to `request_adoption`.

The fallback is **per row**, not per account: a run without Looker data should
render exactly as it did before the derived fields existed, with the Salesforce
picklist and its definition line and no evidence block.

Comply and Agentic are Salesforce picklists; label those with the colour word.

**Adoption tab**

- **PBAC** — a scatter plot, then a written explanation beneath it.
  - x = `consumption_ratio_raw`, axis 0 to 1.25. Anything above 1.25 is drawn in
    a pinned column at the right edge with a heavier outline; the axis must not
    stretch to fit it, because the ratio reaches 100 on a few accounts.
  - y = `pbac_score`, 0 to 7.
  - Shade the plot background into six regions using the grid in `schema.md`:
    columns split at 0.25 and 0.75, rows split at score 4. Compute each region's
    colour with the same rule, so the shading cannot disagree with
    `pbac_rating`.
  - Points: radius 1 to 3 scaled on `sqrt(arr_raw)`, filled with the rating
    colour, outlined in near-black. A null `consumption_ratio_raw` draws at
    x = 0 with a dashed outline — it is a placeholder, not a measurement.
  - Jitter points deterministically from the account name (`pbac_score` is an
    integer and most accounts share a handful of values, so they hide each
    other). The same account must land in the same place on every render.
  - A summary panel to the right of the plot, not a tooltip: hovering a point
    fills it, clicking pins it so it survives the cursor moving away. Show the
    account name, rating, ARR, the three consumption figures, the score,
    platforms and features.
  - Out-of-facet points stay on the plot, dimmed, rather than disappearing.
- **Request, Comply, Agentic** — sortable tables: Account, Region, CSM,
  Lifecycle Stage, ARR, Rating. For Request, add the Salesforce picklist as a
  second stoplight beside the Looker one, so disagreements are visible. Above
  each table, a strip counting accounts, total ARR, and each colour.

**Escalations tab**

`escalations.json`, one row per open CUSTX Jira issue. This is a triage view:
what is on fire, for whom, and for how long.

Rows are already sorted by severity then age. Re-sort after filtering on
`severity_rank` ascending, then `age_days` descending. **Do not sort on the
`severity` label** — it is whatever the Jira instance calls its priorities
(`P1`, `Sev 1`, `Highest` all occur), so it is not alphabetically ordered by
urgency.

`severity_rank` runs 1 (most urgent) to 5, with 9 for a priority name the
normaliser did not recognise. Sort on the full range, but **clamp to 1–4 when
picking a colour** — there are four severity colours, and ranks 5 and 9 share
the least-urgent one. A colour map keyed on 1–4 without a clamp will drop or
mis-colour a `Lowest`-priority row.

Each row: a severity chip, the summary, the age, then a metadata line.

```
[P1]  Subscription policy not applying to newly registered data sources   210d open
      CUSTX-3382 ↗ · ADP · Triage · US - E · Lauren Lorenzo · Priya N. · opened 2026-02-24
```

- The severity chip is coloured off `severity_rank`, 1 being most urgent, and
  the row carries a matching left border.
- The age reads `"<n>d open"`. **90 days or more is stale** — flag it in red.
  That threshold is the point of the column; an escalation nobody has closed in
  a quarter is a different problem from a new one.
- `key` is a link to `url`, opening in a new tab.
- The account name links through to that account's card, the way every other
  account reference in the dashboard does.
- An escalation with no account is **not** hidden. Render "no account matched"
  in a muted style. The account is resolved by fuzzy matching against Salesforce
  names, so an unmatched row is a match failure worth seeing, not an absence.

Facets: severity and status are **local** pills, built from the values actually
present in the currently-facetted rows — so a status pill never offers a
combination that yields nothing. Region and CSM reuse the global pills. Search
matches account name, summary and issue key.

A stats strip above the list: open count, distinct customers, oldest age in
days, then a count per severity in rank order.

**Changes tab**

`changes.json` carries seven components: `accounts`, `renewals`, `expansions`,
`forecast`, `csm_table`, `saas_pipeline`, `escalations`. Render each as a
collapsible section with `+n` / `−n` / `~n` badges in the header. Every
component is `{ added, removed, field_changes }` except `csm_table`, which has
`reassignments` instead. `field_changes` rows carry paired `<field>.old` and
`<field>.new` columns; render them as `old → new`.

The `escalations` component is keyed on the Jira issue key, so:

- `added` is an escalation **raised** since the last snapshot.
- `removed` is one that **closed**. That is good news — do not style it as a
  loss the way a removed renewal is styled.
- `field_changes` is a severity or status move on an issue open in both runs. A
  P3 escalating to P1 matters as much as a new one being raised, so give it
  equal weight.

The escalations diff returns empty on the first run after the feature was added,
because the previous snapshot has no escalations to compare against. That is
deliberate — it stops a first run reporting thirty spurious additions. An empty
escalations section is therefore not necessarily a bug.

**Empty states**

Distinguish "no data was loaded" from "your filters match nothing" — they send
someone looking in completely different places. Say which.

For escalations specifically, `has_data: false` means the pipeline ran without
Jira credentials. Say so: *"No escalation data — set JIRA_EMAIL and JIRA_TOKEN
in .Renviron and re-run the pipeline."* That is actionable; "no results" is not.

**Build**

Check your code in the sandbox and fix errors before publishing. Use React with
inline SVG for the scatter; do not pull in a charting library for it, since the
banded background and the pinned outlier column are not things a chart library
will give you cleanly. When you are done, say which sub-tabs you fully
implemented, which you simplified, and what you left out.

---

## Scoring the result

Compare against `output/immuta_accounts.html` open in another window. The
interesting failures, in roughly the order they tend to show up:

1. **Does it finish all ten sub-tabs?** Expect Accounts and one or two others to
   be real and the rest to be stubs. That is the honest headline result.
2. **Are the numbers still right?** Spot-check three accounts against the R
   page. Reformatted currency and recomputed ratios are the common failure, and
   the one that would make a frame unsafe to circulate.
3. **Do the stoplights read the colour word, or guess?** A hardcoded map from
   picklist sentence to colour breaks the moment Salesforce edits a picklist.
4. **Is the PBAC scatter's shading computed or hardcoded?** Hardcoded region
   colours are the subtler version of the same bug: they will silently disagree
   with `pbac_rating` the first time a threshold moves.
5. **Do the facets stay global across pages, not just across sub-tabs?** Per-tab
   filter state is the easy implementation and the wrong one.
6. **The outliers.** Accounts with a ratio above 1.25 (15 of them) and accounts
   with a null ratio (3) are where a naive plot falls apart — either the axis
   stretches to 100 and everything collapses onto the y-axis, or the nulls get
   silently plotted at zero as though they were measured.
7. **Does Escalations sort on `severity_rank` or on the label?** Sorting `P1`,
   `P2`, `P3` alphabetically happens to look right, which is what makes this
   worth checking — it will only break when the instance uses `Highest` /
   `High`, and by then nobody is looking.
8. **Are unmatched escalations visible?** Dropping the rows with a null account
   is the tidy-looking implementation and it hides the exact failure the
   matching logic was written to surface.
9. **Is a closed escalation framed as good news?** The generic diff renderer
   styles `removed` as a loss. Escalations invert that, and an agent reusing one
   renderer across all seven components will get it wrong.

Points 6 and 9 are the ones worth watching. Neither is a React problem; both are
whether the agent read the spec and understood why the rule was there.

## Related

- `R/output_dust_payload.R` — writes the payload
- `R/report_all_accounts_html.R` — the R generator being compared against;
  `.rah_account_records()`, `.build_escalations_json()` and `.build_saas_json()`
  are the shared builders
- `R/op_list_escalations.R` — the Jira pull behind the Escalations tab
- `R/op_list_pbac_adoption.R` — where the PBAC rating grid is defined
- `R/enrich_list_object.R` — `.diff_escalations()` builds the Changes component
- `DUST_SLIDES_AGENT.md` — the same pattern for the Slides deck
