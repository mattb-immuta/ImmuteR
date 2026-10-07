#' Write the dashboard payload as JSON for a Dust frame
#'
#' Emits the same data \code{\link{report_all_accounts_html}} embeds in
#' \code{output/immuta_accounts.html}, as standalone JSON files a Dust agent can
#' be handed to build a "frame" (an interactive React view) from. Both read
#' \code{.rah_account_records()}, so the frame and the R-generated page are
#' rendering byte-identical records — any difference between the two is a
#' difference in the \emph{rendering}, which is the whole point of the exercise.
#'
#' Six files land in \code{dir}:
#'
#' \describe{
#'   \item{\code{accounts.json}}{One object per account, sorted by name — the
#'     page's \code{ACCOUNTS} array. Around 180 KB for 72 accounts, comfortably
#'     inside both Dust's 50 MB upload limit and an agent's context window.}
#'   \item{\code{changes.json}}{The snapshot diff, i.e. the page's
#'     \code{CHANGES_DATA} (\code{\link{enrich_list_object}} output). Carries an
#'     \code{escalations} component alongside the opportunity ones.}
#'   \item{\code{consumption.json}}{The consumption rows, i.e. the page's
#'     \code{CONSUMPTION_DATA}. The Self-Managed tab reads from this.}
#'   \item{\code{escalations.json}}{Open CUSTX Jira issues, i.e. the page's
#'     \code{ESCALATIONS_DATA} (\code{\link{op_list_escalations}}). Empty with
#'     \code{has_data = FALSE} when the Jira credentials are absent.}
#'   \item{\code{saas.json}}{The SaaS migration pipeline, i.e. the page's
#'     \code{SAAS_DATA}, with CSM joined on from accounts.}
#'   \item{\code{schema.md}}{A field dictionary plus one fully worked example
#'     record. Written so the agent can code against the shape without reading
#'     all 72 records first — which is what stops it inventing field names.}
#' }
#'
#' The JSON is deliberately presentation-ready rather than raw: currency is
#' already formatted, missing values are already em-dashes, platforms arrive as
#' HTML. A frame should render these strings, not re-derive them — the
#' \code{*_raw} fields exist for sorting and plotting, and are the only numbers
#' worth computing with.
#'
#' See \code{DUST_FRAME_AGENT.md} at the project root for the agent
#' instructions these files are meant to accompany.
#'
#' @param op_list Named list from \code{enrich_opportunities()} /
#'   \code{enrich_list_object()}.
#' @param dir Character. Directory to write into, created if absent. Defaults
#'   to \code{"output/dust_frame"}.
#'
#' @return Invisibly, a named character vector of the paths written.
#' @examples
#' \dontrun{
#'   output_dust_payload(op_list)
#' }
#' @export
output_dust_payload <- function(op_list, dir = "output/dust_frame") {

  if (is.null(op_list$accounts)) {
    stop("`op_list$accounts` is NULL. Run enrich_opportunities() first.")
  }

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  records <- .rah_account_records(op_list)

  paths <- c(
    accounts    = file.path(dir, "accounts.json"),
    changes     = file.path(dir, "changes.json"),
    consumption = file.path(dir, "consumption.json"),
    escalations = file.path(dir, "escalations.json"),
    saas        = file.path(dir, "saas.json"),
    schema      = file.path(dir, "schema.md")
  )

  # Every file is written by the same builder the HTML page uses, so the frame
  # and the page cannot disagree about what the data says. .to_json() is the
  # package's own serialiser — the frame sees exactly the escaping and number
  # formatting the page does.
  writeLines(.to_json(records),                  paths[["accounts"]],    useBytes = TRUE)
  writeLines(.build_changes_json(op_list),       paths[["changes"]],     useBytes = TRUE)
  writeLines(.build_consumption_json(op_list),   paths[["consumption"]], useBytes = TRUE)
  writeLines(.build_escalations_json(op_list),   paths[["escalations"]], useBytes = TRUE)
  writeLines(.build_saas_json(op_list),          paths[["saas"]],        useBytes = TRUE)
  writeLines(.dust_schema_md(records, op_list),  paths[["schema"]],      useBytes = TRUE)

  # Escalations are the one component that can legitimately be absent (no Jira
  # credentials), so say so at write time rather than leaving the agent to
  # discover an empty tab.
  n_esc <- if (is.data.frame(op_list$escalations)) nrow(op_list$escalations) else 0L

  message(".. wrote Dust frame payload to ", dir, "/ (",
          length(records), " accounts, ",
          round(file.size(paths[["accounts"]]) / 1024), " KB; ",
          n_esc, " escalation(s)",
          if (n_esc == 0) " — escalations.json is an empty state" else "", ")")

  invisible(paths)
}


# ---- Internal: the field dictionary ------------------------------------------
# An agent handed 180 KB of JSON will skim it and guess at field names. A short
# dictionary in front of one real record is what keeps it honest, so this is
# generated from the records themselves rather than hand-maintained — a field
# added upstream shows up here on the next run instead of going undocumented.

#' @noRd
.dust_schema_md <- function(records, op_list) {

  # Field notes worth stating, keyed by field name. Anything not listed gets its
  # type and example only — the name carries the rest.
  notes <- c(
    id                          = "Slug of the account name. Safe for DOM ids and React keys.",
    name                        = "Account name. The join key everywhere else.",
    account_id                  = "Salesforce 15-character account ID.",
    customer_central_url        = "Link to Customer Central, or \"\" when absent.",
    region                      = "One of the five regions. A facet.",
    arr                         = "Contract ARR, pre-formatted (\"$205,000\").",
    arr_raw                     = "Contract ARR as a number. Sort and size on this, not `arr`.",
    csm                         = "Customer Success Manager. A facet.",
    consumption_link            = "A full <a> tag, not a URL. Insert as HTML or skip it.",
    is_saas                     = "TRUE for SaaS, FALSE for self-managed.",
    unsupported                 = "Self-managed account running an unsupported version.",
    churn_summary               = "Comma-separated fiscal quarters with churn-forecast risk.",
    lifecycle_stage             = "Salesforce lifecycle picklist.",
    pbac_adoption               = "Salesforce PBAC picklist. Fallback only — prefer `pbac_rating`.",
    pbac_rating                 = "Looker-derived PBAC rating, \"<colour>: <label>\". The one to show.",
    pbac_score                  = "Weighted PBAC complexity score, 0-7. The account card mini plot's y axis.",
    product_adoption            = "pbac_score + Request points (retained 3, adopted 1), 0-10. The Adoption page scatter's y axis.",
    product_rating              = "product_adoption crossed with consumption ratio, \"<colour>: <label>\" (threshold 4 / 0.25 / 0.75).",
    request_points              = "Request adoption points behind product_adoption: 3 retained, 1 adopted, 0 otherwise.",
    pbac_features               = "Complexity signals earned, joined with \" · \".",
    comply_adoption             = "Salesforce Comply picklist. Colour word leads the string.",
    request_adoption            = "Salesforce Request picklist. Hand-maintained.",
    request_adoption_derived    = "Looker-derived Request tier. Prefer this over the picklist.",
    request_status              = "tried / adopted / retained / none — the Looker stage.",
    agentic_governance_adoption = "Salesforce Agentic picklist.",
    platform_html               = "Platforms as HTML; dev-only ones are wrapped in a grey span.",
    multiplatform               = "More than one platform in Production.",
    registered_users            = "Formatted count, or an em-dash.",
    licensed_users              = "Formatted count. \"0\" means an ELA (unlimited seats).",
    consumption_ratio           = "Pre-formatted percentage, or an em-dash.",
    consumption_ratio_raw       = "The ratio as a number, or null. The scatter's x axis.",
    immuta_version              = "Deployed version, \"saas\" for SaaS accounts.",
    connections                 = "Connections migration per Production instance, \"12 / 40\". \"None\" means nothing upgraded; several instances are comma-separated and named.",
    connections_raw             = "Upgraded over total across the account, or null. Sort on this.",
    inc_90d                     = "Support cases in the last 90 days.",
    inc_360d                    = "Support cases in the last 360 days.",
    custx_90d                   = "Escalations in the last 90 days.",
    custx_open                  = "Escalations currently open.",
    executive_buyer             = "Contact name(s) from the Salesforce contacts report; several are \"; \"-separated. Em-dash when nobody holds the role.",
    buyer_relationship          = "The executive buyer's relationship. From the account report. Strong / neutral / weak.",
    buyer_influence             = "The executive buyer's influence. From the account report. Strong / neutral / weak.",
    champion                    = "Contact name(s) from the Salesforce contacts report; several are \"; \"-separated. Em-dash when nobody holds the role.",
    champion_relationship       = "The champion's relationship. From the account report. Strong / neutral / weak.",
    champion_influence          = "The champion's influence. From the account report. Strong / neutral / weak.",
    last_qbr                    = "Formatted date, or an em-dash.",
    next_qbr                    = "Formatted date, or an em-dash.",
    qbr_note                    = "Free text from Salesforce.",
    renewals                    = "Array of open renewal opportunities. Often empty.",
    expansions                  = "Array of open expansion opportunities. Often empty.",
    products                    = "Array of {name, quantity} from the active order."
  )

  first <- records[[1]]

  .type_of <- function(v) {
    if (is.list(v))        return(paste0("array[", length(v), "]"))
    if (is.logical(v))     return("boolean")
    if (is.numeric(v))     return("number")
    "string"
  }

  .example <- function(v) {
    if (is.list(v)) return("see below")
    s <- paste(as.character(v), collapse = " ")
    if (is.na(s) || !nzchar(s)) return("\"\"")
    if (nchar(s) > 46) s <- paste0(substr(s, 1, 45), "…")
    paste0("`", gsub("\\|", "\\\\|", s), "`")
  }

  rows <- vapply(names(first), function(k) {
    paste0("| `", k, "` | ", .type_of(first[[k]]), " | ", .example(first[[k]]),
           " | ", if (k %in% names(notes)) notes[[k]] else "", " |")
  }, character(1))

  # A record with renewals AND expansions on it, so the nested shapes are both
  # visible — the first account alphabetically usually has neither.
  richest <- records[[which.max(vapply(records,
    function(r) length(r$renewals) + length(r$expansions) + length(r$products),
    integer(1)))]]

  n_regions <- length(unique(vapply(records, function(r) r$region, character(1))))
  n_csms    <- length(unique(vapply(records, function(r) r$csm,    character(1))))

  # The side files are documented from the data frames themselves for the same
  # reason the account table is: a column added upstream turns up here on the
  # next run instead of going undocumented and being guessed at.
  .side_table <- function(df, notes = character(0)) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return("_No rows in the current payload — the file is an empty state._")
    }
    c("| Field | Type | Example | Notes |",
      "|---|---|---|---|",
      vapply(names(df), function(k) {
        v <- df[[k]][1]
        paste0("| `", k, "` | ",
               if (is.logical(v)) "boolean" else if (is.numeric(v)) "number" else "string",
               " | ", .example(v), " | ",
               if (k %in% names(notes)) notes[[k]] else "", " |")
      }, character(1)))
  }

  esc_notes <- c(
    key             = "Jira issue key. The diff key in `changes.json`.",
    url             = "Browse URL for the issue. Render the key as a link to it.",
    summary         = "Issue title. Usually leads with the customer name.",
    severity        = "Jira priority name, instance-specific (`P1`, `Sev 1`, `Highest`).",
    severity_rank   = "Normalised urgency, 1 (most) to 5, 9 = unrecognised. Sort on this; clamp to 1-4 for colour.",
    status          = "Workflow status name. A local facet.",
    status_category = "Jira status category. Always not-Done — open issues only.",
    age_days        = "Days since `created`. Flag at 90+; that is the stale threshold.",
    `Account Name`  = "Joined to an account, or null when no match was found.",
    Region          = "From the account, not from Jira. Null when unmatched.",
    csm             = "From the account, not from Jira. Null when unmatched.",
    assignee        = "Jira assignee display name, or null when unassigned.",
    created         = "Date only, `YYYY-MM-DD`.",
    updated         = "Date only, `YYYY-MM-DD`."
  )

  esc_rows  <- .side_table(op_list$escalations, esc_notes)
  saas_rows <- .side_table(op_list$saas_pipeline)

  paste(c(
    "# Dust frame payload — field reference",
    "",
    paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), " from op_list. ",
           length(records), " accounts, ", n_regions, " regions, ", n_csms, " CSMs."),
    "",
    "Six files: `accounts.json`, `changes.json`, `consumption.json`,",
    "`escalations.json`, `saas.json` and this dictionary.",
    "",
    "Values are **presentation-ready**: render them as-is. The `*_raw` fields are",
    "the only ones to compute with — sort, filter and plot on those.",
    "",
    "## `accounts.json`",
    "",
    "An array of these objects, sorted by `name`.",
    "",
    "| Field | Type | Example | Notes |",
    "|---|---|---|---|",
    rows,
    "",
    "## Nested shapes",
    "",
    "```json",
    .to_json(list(
      renewals   = if (length(richest$renewals)   > 0) richest$renewals[1]   else list(),
      expansions = if (length(richest$expansions) > 0) richest$expansions[1] else list(),
      products   = if (length(richest$products)   > 0) richest$products[1]   else list()
    )),
    "```",
    "",
    "## Rating strings",
    "",
    "Every adoption rating is `\"<colour>: <label>\"` or `\"<Colour> - <label>\"`,",
    "where colour is one of `green`, `amber` (sometimes `yellow`), `red`, `black`.",
    "Read the colour off the front for the stoplight; the rest is the definition,",
    "which belongs in a tooltip rather than the cell. Severity order is",
    "green > amber > red > black.",
    "",
    "## PBAC rating grid",
    "",
    "`pbac_rating` is not stored per axis — it is `pbac_score` crossed with",
    "`consumption_ratio_raw`:",
    "",
    "|                | ratio < 0.25 | 0.25–0.75 | ratio ≥ 0.75 |",
    "|---|---|---|---|",
    "| **score ≥ 4** | red   | amber | green |",
    "| **score < 4** | black | red   | amber |",
    "",
    "A null `consumption_ratio_raw` means the figure is unavailable, which is not",
    "the same as zero: such an account reaches red on complexity alone and no",
    "further. `licensed_users` of `\"0\"` is an ELA, already scored as a ratio of 1.",
    "",
    "## `escalations.json`",
    "",
    "`{ has_data: boolean, rows: [...] }`. One row per **open** CUSTX Jira issue,",
    "already sorted by severity then age. `has_data: false` with an empty `rows`",
    "means the pipeline ran without Jira credentials — that is a different empty",
    "state from \"your filters match nothing\", and the two must read differently.",
    "",
    esc_rows,
    "",
    "Unlike the account records these values are **raw, not pre-formatted**: a",
    "missing account is `null`, not an em-dash. Escalations come straight from",
    "Jira rather than through the account record builder, so this one file is the",
    "exception to the presentation-ready rule — supply your own placeholder here.",
    "",
    "Sort on `severity_rank` (ascending), then `age_days` (descending). Do not",
    "sort on the `severity` label: it is whatever the Jira instance calls its",
    "priorities, so it is not alphabetically ordered by urgency.",
    "",
    "## `saas.json`",
    "",
    "`{ has_data: boolean, rows: [...] }`. The SaaS migration pipeline, one row",
    "per account in flight, with `csm` joined on from the account record.",
    "",
    saas_rows,
    "",
    "## `changes.json`",
    "",
    "`{ has_data, diff_date, summary, <component>... }`. Each component is",
    "`{ added, removed, field_changes }` — except `csm_table`, which has",
    "`reassignments` in place of `field_changes`. The components are:",
    "",
    "`accounts`, `renewals`, `expansions`, `forecast`, `csm_table`,",
    "`saas_pipeline`, `escalations`.",
    "",
    "`field_changes` rows carry paired `<field>.old` / `<field>.new` columns;",
    "render them as `old → new`. The `escalations` component is keyed on the Jira",
    "issue `key`, so `added` is an escalation **raised** since the last snapshot",
    "and `removed` is one that **closed** — a close is good news, not a deletion,",
    "and should not be styled as a loss.",
    ""
  ), collapse = "\n")
}
