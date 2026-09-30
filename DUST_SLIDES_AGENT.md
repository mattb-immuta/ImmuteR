# Dust agent: regional success-meeting slides

How to drive the Google Slides deck from Dust instead of from R, using the JSON
that `output_slide_payload_json()` writes to `output/slide_payloads/`.

## Why Dust rather than the R generator

`output_slides_regional_review()` calls the Slides API directly, which requires
an OAuth client from an Immuta-owned Google Cloud project with the Slides API
enabled. googlesheets4's built-in client belongs to Posit's project
(`603366585132`), where Slides is not enabled and never will be by us.

Dust's Google Drive connector carries its own OAuth app with write scopes for
Docs, Sheets, and Slides. Connecting Drive to Dust sidesteps the GCP project
entirely. The R generator stays in the codebase for whenever a project does
land — both paths write the same shapes, because both read the same object IDs
out of the payload.

## The one invariant

**CSM comment boxes must survive regeneration.** They are the only content in
the deck that does not come from `op_list`, so if a run clobbers them the
commentary is gone with no way to rebuild it.

Every payload carries a `slide_ids` block:

```json
"slide_ids": {
  "slide":           "s_acorns_48213",
  "title":           "t_acorns_48213",
  "arr":             "r_acorns_48213",
  "meta":            "d_acorns_48213",
  "header":          ["h1_acorns_48213", "h2_acorns_48213", "h3_acorns_48213"],
  "metrics":         ["m1_acorns_48213", "m2_acorns_48213", "m3_acorns_48213"],
  "comment":         ["c1_acorns_48213", "c2_acorns_48213", "c3_acorns_48213"],
  "writable":        ["t_…", "r_…", "d_…", "h1_…", "h2_…", "h3_…", "m1_…", "m2_…", "m3_…"],
  "never_overwrite": ["c1_acorns_48213", "c2_acorns_48213", "c3_acorns_48213"]
}
```

The IDs are derived from the account name, so they are stable across runs. The
agent never invents an object ID — it reads them from the payload. That is what
keeps an LLM in the loop from being a risk: the agent decides *whether* to
create or update, not *what to call things*.

## Agent instructions

Paste this into the Dust agent's instructions. It needs the Google Drive
connector with write permissions, and the payload JSON as input.

---

You maintain a Google Slides deck of account review slides. You are given a JSON
payload with an `accounts` array. The target presentation ID is provided in the
conversation.

Follow these rules exactly.

1. **Fetch the presentation first.** List every existing `objectId` before making
   any change.

2. **Never generate your own object IDs.** Use only the values in each account's
   `slide_ids` block.

3. **For each account in `accounts`:**
   - If `slide_ids.slide` is **not** in the presentation, create a slide with
     that exact `objectId` using the `BLANK` predefined layout, then create the
     text boxes listed in `slide_ids.writable` and `slide_ids.comment`. Leave
     every `comment` box empty.
   - If `slide_ids.slide` **is** already in the presentation, do not create
     anything. Update text only.

4. **Only ever write text into the object IDs listed in `slide_ids.writable`.**
   The IDs in `slide_ids.never_overwrite` hold text a human wrote. Do not
   insert, delete, replace, restyle, move, or resize them. If one is missing
   from an existing slide, create it empty — do not populate it.

5. **Text content per shape:**
   - `title` — `account.name`
   - `arr` — `account.arr`
   - `meta` — `AE: {ae}   |   CSM: {csm}   |   FCTO: {fcto}   |   Next Renewal: {next_renewal}`
   - `header[0..2]` — literally `Adoption`, `Performance`, `Engagement`
   - `metrics[0]` — the `adoption` block: each module as `Label: Status` on one
     line, then
     `Platforms: {platforms}    Features: {features_text}`, then a blank line,
     then
     `Registered: {registered}    Licensed: {licensed}    Ratio: {ratio}`, then
     `Lifecycle: {lifecycle_stage}`

     A module's `status` is the text beside its dot. PBAC and Request are
     rated from Looker and say what they are — `Moderate adoption`,
     `Retained` — while Comply and Agentic are Salesforce picklists and say
     their colour word. `source` on each module names which it came from
     (`looker` or `salesforce`); a run without Looker falls every module back
     to `salesforce`. Platforms and features are the evidence behind the PBAC
     rating, which is why they sit under the module line.
   - `metrics[1]` — the `performance` block. If `windowed` is true, show
     `Cases 60d` / `Cases 180d` with escalation counts. If false, show
     `Support conversations (all time): {cases_total}` and note that the
     60/180-day split needs a case-date field in Looker look 1776. Then
     `Deployment`, `Version`, `Status`.
   - `metrics[2]` — the `engagement` block: `Last QBR`, `Next QBR`, then the
     note.

6. **Never delete a slide.** An account that has left the region keeps its slide
   until a human removes it.

7. When finished, report how many slides you created and how many you updated,
   and confirm you did not write to any `never_overwrite` ID.

---

## Testing

Run against a **copy** of the deck the first few times. The failure mode that
matters is not a broken layout — it is an agent that recreates slides instead of
updating them, which silently destroys commentary. Check specifically:

1. Run once against an empty deck. Confirm one slide per account.
2. Type something into two or three comment boxes by hand.
3. Change a number upstream (or just re-run) and run again.
4. Confirm the typed text is still there and the metrics moved.

Step 4 is the whole test. If it fails, the Dust path is not safe to point at the
live deck.

## Related

- `R/account_slide_payload.R` — builds the payload, emits `slide_ids`
- `R/output_slides_regional_review.R` — the direct-API equivalent
- `R/slides_utils.R` — `.slide_uid()` / `.slide_ids()`, where the IDs come from
