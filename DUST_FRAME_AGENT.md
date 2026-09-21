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

**Files:** upload the three JSON files from `output/dust_frame/`, written by
`output_dust_payload(op_list)` on every pipeline run:

| File | Contents | Size |
|---|---|---|
| `accounts.json` | One object per account, sorted by name | ~180 KB / 72 accounts |
| `changes.json` | Snapshot diff — what moved since the last run | ~11 KB |
| `consumption.json` | Consumption rows, for the Self-Managed tab | ~500 KB |
| `schema.md` | Field dictionary + a worked example | ~6 KB |

Upload `schema.md` too, and tell the agent to read it first. An agent handed
180 KB of JSON will skim it and invent field names; the dictionary is what stops
that. All four are well inside Dust's 50 MB upload limit, and `accounts.json` is
around 47k tokens — large but not unreasonable for a single context.

`accounts.json` is the same array the R page embeds as its `ACCOUNTS` constant,
from the same builder (`.rah_account_records()`). The comparison is therefore
clean: both surfaces render identical records, so every difference is a
rendering difference.

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

## Agent instructions

Paste this into the Dust agent's instructions.

---

You build an interactive dashboard as a frame, from JSON files given to you in
the conversation. Read `schema.md` before writing any code.

**Data contract**

- `accounts.json` — array of account objects, already sorted by `name`.
- `changes.json` — the snapshot diff.
- `consumption.json` — consumption rows, joined on `Account Name`.

Every string value is presentation-ready. Render it as given. Never reformat
currency, never recompute a percentage, never substitute your own placeholder
for a missing value — the payload already uses an em-dash (`—`) for those. Sort,
filter and plot on the `*_raw` fields only.

Never invent a field name. If something you want is not in `schema.md`, it does
not exist; leave that element out rather than guessing.

**Layout**

A top bar with the title, a search box, and six page tabs, each showing a count:

1. **Accounts** — the default. A left sidebar with two facet groups, Region and
   CSM, each a row of pills with an "All" option. Main area is one collapsible
   card per account, alphabetical.
2. **Renewals** — flat sortable table of every account's `renewals`, flattened,
   with the account name on each row.
3. **Expansions** — the same for `expansions`.
4. **Changes** — what moved since the previous snapshot, grouped by kind
   (added / removed / field changes).
5. **Adoption** — four sub-tabs: PBAC, Request, Comply, Agentic.
6. **Self-Managed** — accounts where `is_saas` is false, with their consumption
   figures.

The search box and the Region/CSM facets are global: a selection made on one
tab still applies when you switch to another.

**Account card**

Collapsed, a card shows: account name, region, CSM, ARR, and a deployment badge
(`SaaS`, or `SM: Supported` / `SM: Unsupported` from `is_saas` and
`unsupported`). Expanded, it opens on an Overview tab alongside Renewals and
Expansions tabs, with three blocks in the Overview:

- **Adoption** — one row per module (PBAC, Comply, Request, Agentic), each a
  coloured dot plus a label. Under PBAC, list `platform_html` (insert as HTML)
  and `pbac_features`.
- **Performance** — `registered_users`, `licensed_users`, `consumption_ratio`,
  `inc_90d`, `inc_360d`, `custx_90d`, `custx_open`, `immuta_version`.
- **Engagement** — `last_qbr`, `next_qbr`, `qbr_note`.

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

**Empty states**

Distinguish "no data was loaded" from "your filters match nothing" — they send
someone looking in completely different places. Say which.

**Build**

Check your code in the sandbox and fix errors before publishing. Use React with
inline SVG for the scatter; do not pull in a charting library for it, since the
banded background and the pinned outlier column are not things a chart library
will give you cleanly. When you are done, say which of the six tabs you fully
implemented, which you simplified, and what you left out.

---

## Scoring the result

Compare against `output/immuta_accounts.html` open in another window. The
interesting failures, in roughly the order they tend to show up:

1. **Does it finish all six tabs?** Expect Accounts and one or two others to be
   real and the rest to be stubs. That is the honest headline result.
2. **Are the numbers still right?** Spot-check three accounts against the R
   page. Reformatted currency and recomputed ratios are the common failure, and
   the one that would make a frame unsafe to circulate.
3. **Do the stoplights read the colour word, or guess?** A hardcoded map from
   picklist sentence to colour breaks the moment Salesforce edits a picklist.
4. **Is the PBAC scatter's shading computed or hardcoded?** Hardcoded region
   colours are the subtler version of the same bug: they will silently disagree
   with `pbac_rating` the first time a threshold moves.
5. **Do the facets stay global across tabs?** Per-tab filter state is the easy
   implementation and the wrong one.
6. **The outliers.** Accounts with a ratio above 1.25 (15 of them) and accounts
   with a null ratio (3) are where a naive plot falls apart — either the axis
   stretches to 100 and everything collapses onto the y-axis, or the nulls get
   silently plotted at zero as though they were measured.

Point 6 is the one worth watching. It is not a React problem; it is whether the
agent read the spec and understood why the rule was there.

## Related

- `R/output_dust_payload.R` — writes the payload
- `R/report_all_accounts_html.R` — the R generator being compared against;
  `.rah_account_records()` is the shared builder
- `R/op_list_pbac_adoption.R` — where the PBAC rating grid is defined
- `DUST_SLIDES_AGENT.md` — the same pattern for the Slides deck
