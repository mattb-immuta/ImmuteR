#' Diff an op_list snapshot against a prior save and append change summaries.
#'
#' Loads the saved \code{.RData} snapshot for \code{diff_date}, compares every
#' component of \code{new_list} against the prior snapshot, and appends a
#' comprehensive \code{changes} summary as well as the legacy
#' \code{renewal_changes} and \code{customer_ops_tracker} elements.
#'
#' @param new_list Named list returned by the current run of
#'   \code{enrich_opportunities()}.
#' @param diff_date A \code{Date} or character string (e.g. \code{"2026-04-01"})
#'   identifying the prior snapshot to compare against. The file
#'   \code{<data_dir>/opportunities_<diff_date>.RData} must exist and contain
#'   an object named \code{op_list}.
#' @param data_dir Character. Path to the directory holding saved \code{.RData}
#'   snapshots. Defaults to \code{"data/"}.
#'
#' @return \code{new_list} with four additional elements:
#' \describe{
#'   \item{changes}{Data frame summarising detected changes across all
#'     op_list components (accounts, renewals, expansions, csm_table,
#'     saas_pipeline, consumption, forecast). One row per component with columns
#'     \code{Component}, \code{Added}, \code{Removed}, \code{Changed},
#'     and \code{Total_Moves}.}
#'   \item{changes_detail}{Named list with the full diff for each component,
#'     each containing data frames for \code{added}, \code{removed}, and
#'     \code{field_changes} (or \code{reassignments} for csm_table).
#'     Also contains \code{diff_date}. The \code{consumption} component has
#'     \code{field_changes} (accounts, keyed by Account ID, present in both
#'     snapshots where Registered Users dropped more than 10\%, or Data
#'     Sources Count or Active Policies (Total) decreased at all) and
#'     \code{all_deltas} (Changes in Data Sources Count / Changes in Active
#'     Policies for every account present in both snapshots, not just the
#'     flagged ones) — see \code{op_list_consumption()} for how those columns
#'     are derived. The \code{forecast} component (from
#'     \code{\link{op_list_forecast}}, keyed by Opportunity ID) has
#'     \code{added}, \code{removed} and \code{field_changes}; the latter
#'     carries \code{.new}/\code{.old} pairs for Churn Quarter, Churn Forecast,
#'     Account Risk, Amount, Stage and the close dates, a matching
#'     \code{*_changed} boolean per field, and \code{forecast_delta} (new minus
#'     old forecast dollars).}
#'   \item{renewal_changes}{Data frame of renewals where Account Risk or Churn
#'     Forecast changed (retained for backward compatibility).}
#'   \item{customer_ops_tracker}{Joined renewal + expansion table for use in
#'     \code{output_customer_ops_tracker()} (retained for backward compatibility).}
#' }
#'
#' @examples
#' \dontrun{
#'   op_list <- enrich_list_object(op_list, diff_date = "2026-04-01")
#' }
#' @export
enrich_list_object <- function(new_list, diff_date, data_dir = "data/") {

  diff_date <- as.Date(diff_date)

  # --- Load the prior snapshot -------------------------------------------------
  rdata_path <- file.path(data_dir,
                          paste0("opportunities_", format(diff_date, "%Y-%m-%d"), ".RData"))
  if (!file.exists(rdata_path)) {
    stop("No snapshot found for diff_date ", diff_date, "\nExpected: ", rdata_path)
  }

  tmp_env <- new.env(parent = emptyenv())
  load(rdata_path, envir = tmp_env)

  if (!exists("op_list", envir = tmp_env)) {
    stop("Loaded RData file does not contain an object named 'op_list': ", rdata_path)
  }
  old_list <- tmp_env$op_list
  rm(tmp_env)

  message(".. diffing against snapshot from ", diff_date)

  changes <- list(diff_date = diff_date)

  # ── 1. Accounts ──────────────────────────────────────────────────────────────
  changes$accounts <- .diff_accounts(new_list$accounts, old_list$accounts)

  # ── 2. Renewals ──────────────────────────────────────────────────────────────
  changes$renewals <- .diff_renewals(new_list$renewals, old_list$renewals)

  # ── 3. Expansions ────────────────────────────────────────────────────────────
  changes$expansions <- .diff_expansions(new_list$expansions, old_list$expansions)

  # ── 4. CSM table ─────────────────────────────────────────────────────────────
  changes$csm_table <- .diff_csm(new_list$csm_table, old_list$csm_table)

  # ── 5. SaaS pipeline ─────────────────────────────────────────────────────────
  changes$saas_pipeline <- .diff_saas(new_list$saas_pipeline, old_list$saas_pipeline)

  # ── 5b. Escalations ──────────────────────────────────────────────────────────
  # Keyed on the Jira issue key, so "added" is an escalation raised since the
  # last run and "removed" is one that closed — both worth seeing.
  changes$escalations <- .diff_escalations(new_list$escalations, old_list$escalations)

  # ── 6. Consumption ───────────────────────────────────────────────────────────
  changes$consumption <- .diff_consumption(new_list$consumption$low_consumption,
                                           old_list$consumption$low_consumption)

  # ── 7. Renewal churn forecast ────────────────────────────────────────────────
  # Snapshots saved before op_list$forecast existed simply have no forecast
  # element, and .diff_forecast() returns an empty list for that case.
  changes$forecast <- .diff_forecast(new_list$forecast, old_list$forecast)

  # Surface the compact summary as $changes; full diff detail at $changes_detail
  change_summary               <- .build_change_summary(changes)
  new_list[["changes"]]        <- change_summary
  new_list[["changes_detail"]] <- changes

  # --- Legacy: renewal_changes (backward compat) --------------------------------
  new_list[["renewal_changes"]] <- .legacy_renewal_changes(
    new_list$renewals, old_list$renewals
  )

  # --- Legacy: customer_ops_tracker (backward compat) --------------------------
  new_list[["customer_ops_tracker"]] <- .build_customer_ops_tracker(new_list)

  message(".. enrich_list_object complete")
  return(new_list)
}


# ── Component diff helpers ────────────────────────────────────────────────────

.diff_accounts <- function(new_df, old_df) {
  if (is.null(new_df) || is.null(old_df)) return(list())

  key  <- "Account ID"
  new_ids <- new_df[[key]]
  old_ids <- old_df[[key]]

  added   <- new_df[new_ids %in% setdiff(new_ids, old_ids), , drop = FALSE]
  removed <- old_df[old_ids %in% setdiff(old_ids, new_ids), , drop = FALSE]

  # Field changes for accounts present in both
  both <- merge(
    new_df[, intersect(c("Account ID", "Account Name", "Contract ARR Rollup",
                          "Customer Success Manager", "Account Owner",
                          "Field CTO", "Region", "Adoption Phase",
                          "request_adoption_looker_stage"), names(new_df))],
    old_df[, intersect(c("Account ID", "Contract ARR Rollup",
                          "Customer Success Manager", "Account Owner",
                          "Field CTO", "Region", "Adoption Phase",
                          "request_adoption_looker_stage"), names(old_df))],
    by = "Account ID", suffixes = c(".new", ".old")
  )

  # The Looker stage is watched on the stage value, not the full picklist
  # sentence, so a reworded picklist string can't register as a change.
  watch_fields <- c("Contract ARR Rollup", "Customer Success Manager",
                    "Account Owner", "Field CTO", "Region", "Adoption Phase",
                    "request_adoption_looker_stage")

  # .field_changed() returns all-FALSE for a field that isn't on both sides,
  # which is the case for request_adoption_looker_stage against any snapshot
  # taken before it existed (or from a looker = FALSE run). That matters: a
  # one-sided column is NOT suffixed by merge(), so a name-based filter would
  # keep it, both suffixed lookups would be NULL, and the resulting logical(0)
  # would collapse the whole Reduce() to logical(0) — silently reporting zero
  # account field changes. The `init` covers watch_fields being empty too.
  change_rows <- Reduce(
    `|`,
    lapply(watch_fields, function(f) .field_changed(both, f)),
    init = rep(FALSE, nrow(both))
  )

  field_changes <- both[change_rows, , drop = FALSE]

  msg_parts <- c(
    if (nrow(added) > 0)   paste(nrow(added),   "new account(s)"),
    if (nrow(removed) > 0) paste(nrow(removed), "removed account(s)"),
    if (nrow(field_changes) > 0) paste(nrow(field_changes), "account(s) with field changes")
  )
  if (length(msg_parts)) message("  accounts: ", paste(msg_parts, collapse = ", "))

  list(added = added, removed = removed, field_changes = field_changes)
}


.diff_renewals <- function(new_df, old_df) {
  if (is.null(new_df) || is.null(old_df)) return(list())

  key     <- "Opportunity ID"
  new_ids <- new_df[[key]]
  old_ids <- old_df[[key]]

  added   <- new_df[new_ids %in% setdiff(new_ids, old_ids), , drop = FALSE]
  removed <- old_df[old_ids %in% setdiff(old_ids, new_ids), , drop = FALSE]

  # Coerce Churn Forecast to numeric for comparison
  new_df$`Churn Forecast` <- suppressWarnings(as.numeric(new_df$`Churn Forecast`))
  old_df$`Churn Forecast` <- suppressWarnings(as.numeric(old_df$`Churn Forecast`))
  new_df$`Churn Forecast`[is.na(new_df$`Churn Forecast`)] <- 0
  old_df$`Churn Forecast`[is.na(old_df$`Churn Forecast`)] <- 0

  watch_new <- intersect(c("Opportunity ID", "Opportunity Name", "Account Name",
                            "Account Risk", "Churn Forecast", "Amount",
                            "Close Date", "Next Step", "churn_forecast_period"),
                          names(new_df))
  watch_old <- intersect(c("Opportunity ID", "Account Risk", "Churn Forecast",
                            "Amount", "Close Date", "churn_forecast_period"),
                          names(old_df))

  both <- merge(new_df[, watch_new], old_df[, watch_old],
                by = "Opportunity ID", suffixes = c(".new", ".old"))

  risk_changed     <- .field_changed(both, "Account Risk")
  forecast_changed <- .field_changed(both, "Churn Forecast")
  amount_changed   <- .field_changed(both, "Amount")
  date_changed     <- .field_changed(both, "Close Date")

  any_changed <- risk_changed | forecast_changed | amount_changed | date_changed

  field_changes <- both[any_changed, , drop = FALSE]
  field_changes$risk_changed     <- risk_changed[any_changed]
  field_changes$forecast_changed <- forecast_changed[any_changed]
  field_changes$amount_changed   <- amount_changed[any_changed]
  field_changes$date_changed     <- date_changed[any_changed]

  if ("Churn Forecast.new" %in% names(field_changes) &&
      "Churn Forecast.old" %in% names(field_changes)) {
    field_changes$forecast_delta <-
      field_changes$`Churn Forecast.new` - field_changes$`Churn Forecast.old`
  }

  msg_parts <- c(
    if (nrow(added) > 0)        paste(nrow(added),        "new renewal(s)"),
    if (nrow(removed) > 0)      paste(nrow(removed),      "closed/removed renewal(s)"),
    if (sum(risk_changed) > 0)  paste(sum(risk_changed),  "risk change(s)"),
    if (sum(forecast_changed) > 0) paste(sum(forecast_changed), "forecast change(s)"),
    if (sum(amount_changed) > 0) paste(sum(amount_changed), "amount change(s)")
  )
  if (length(msg_parts)) message("  renewals: ", paste(msg_parts, collapse = ", "))

  list(added = added, removed = removed, field_changes = field_changes)
}


.diff_expansions <- function(new_df, old_df) {
  if (is.null(new_df) || is.null(old_df)) return(list())

  key     <- "Opportunity ID"
  new_ids <- new_df[[key]]
  old_ids <- old_df[[key]]

  added   <- new_df[new_ids %in% setdiff(new_ids, old_ids), , drop = FALSE]
  removed <- old_df[old_ids %in% setdiff(old_ids, new_ids), , drop = FALSE]

  watch_new <- intersect(c("Opportunity ID", "Opportunity Name", "Account Name",
                            "Amount", "Fiscal Period", "Close Date",
                            "Opportunity Owner"),
                          names(new_df))
  watch_old <- intersect(c("Opportunity ID", "Amount", "Fiscal Period", "Close Date"),
                          names(old_df))

  both <- merge(new_df[, watch_new], old_df[, watch_old],
                by = "Opportunity ID", suffixes = c(".new", ".old"))

  amount_changed <- .field_changed(both, "Amount")
  period_changed <- .field_changed(both, "Fiscal Period")
  any_changed    <- amount_changed | period_changed

  field_changes <- both[any_changed, , drop = FALSE]
  field_changes$amount_changed <- amount_changed[any_changed]
  field_changes$period_changed <- period_changed[any_changed]

  msg_parts <- c(
    if (nrow(added) > 0)          paste(nrow(added),          "new expansion(s)"),
    if (nrow(removed) > 0)        paste(nrow(removed),        "closed/removed expansion(s)"),
    if (sum(amount_changed) > 0)  paste(sum(amount_changed),  "amount change(s)"),
    if (sum(period_changed) > 0)  paste(sum(period_changed),  "fiscal period change(s)")
  )
  if (length(msg_parts)) message("  expansions: ", paste(msg_parts, collapse = ", "))

  list(added = added, removed = removed, field_changes = field_changes)
}


.diff_csm <- function(new_df, old_df) {
  if (is.null(new_df) || is.null(old_df)) return(list())

  # CSM report key is Account ID (or Account Name if ID not available)
  key <- if ("Account ID" %in% names(new_df) && "Account ID" %in% names(old_df)) {
    "Account ID"
  } else if ("Account Name" %in% names(new_df) && "Account Name" %in% names(old_df)) {
    "Account Name"
  } else {
    return(list())
  }

  csm_col <- intersect(names(new_df),
                       c("Customer Success Manager", "CSM", "CSM Name", "Owner"))
  if (length(csm_col) == 0) return(list())
  csm_col <- csm_col[1]

  new_ids <- new_df[[key]]
  old_ids <- old_df[[key]]

  added   <- new_df[new_ids %in% setdiff(new_ids, old_ids), , drop = FALSE]
  removed <- old_df[old_ids %in% setdiff(old_ids, new_ids), , drop = FALSE]

  both <- merge(
    new_df[, c(key, csm_col)],
    old_df[, c(key, csm_col)],
    by = key, suffixes = c(".new", ".old")
  )

  csm_changed <- .field_changed(both, csm_col)
  reassignments <- both[csm_changed, , drop = FALSE]

  msg_parts <- c(
    if (nrow(added) > 0)          paste(nrow(added),         "new assignment(s)"),
    if (nrow(removed) > 0)        paste(nrow(removed),       "removed assignment(s)"),
    if (nrow(reassignments) > 0)  paste(nrow(reassignments), "CSM reassignment(s)")
  )
  if (length(msg_parts)) message("  csm_table: ", paste(msg_parts, collapse = ", "))

  list(added = added, removed = removed, reassignments = reassignments)
}


# Escalations diff, keyed on the Jira issue key. Unlike the other components
# there is no snapshot before op_list$escalations existed, so a NULL old side
# means "first run with this data", not "everything is new" — returning an empty
# list keeps the first run from reporting 30-odd spurious additions.
.diff_escalations <- function(new_df, old_df) {
  if (is.null(new_df) || is.null(old_df) ||
      !is.data.frame(new_df) || !is.data.frame(old_df) ||
      !("key" %in% names(new_df)) || !("key" %in% names(old_df))) {
    return(list())
  }

  added   <- new_df[!(new_df$key %in% old_df$key), , drop = FALSE]
  removed <- old_df[!(old_df$key %in% new_df$key), , drop = FALSE]

  # Severity and status moves on issues open in both runs — a P3 escalating to
  # P1 matters as much as a new one being raised.
  field_changes <- data.frame()
  both_keys <- intersect(new_df$key, old_df$key)
  if (length(both_keys) > 0) {
    cols <- intersect(c("severity", "status"), intersect(names(new_df), names(old_df)))
    if (length(cols) > 0) {
      both <- merge(new_df[new_df$key %in% both_keys, c("key", "Account Name", cols)],
                    old_df[old_df$key %in% both_keys, c("key", cols)],
                    by = "key", suffixes = c(".new", ".old"))
      changed <- Reduce(`|`, lapply(cols, function(cc) .field_changed(both, cc)))
      field_changes <- both[changed, , drop = FALSE]
    }
  }

  list(added = added, removed = removed, field_changes = field_changes)
}

.diff_saas <- function(new_df, old_df) {
  if (is.null(new_df) || is.null(old_df)) return(list())

  key <- if ("Account ID" %in% names(new_df) && "Account ID" %in% names(old_df)) {
    "Account ID"
  } else if ("Account Name" %in% names(new_df) && "Account Name" %in% names(old_df)) {
    "Account Name"
  } else {
    return(list())
  }

  new_ids <- new_df[[key]]
  old_ids <- old_df[[key]]

  added   <- new_df[new_ids %in% setdiff(new_ids, old_ids), , drop = FALSE]
  removed <- old_df[old_ids %in% setdiff(old_ids, new_ids), , drop = FALSE]

  # Stage changes if available
  stage_col <- intersect(names(new_df), c("Stage", "Stage Name", "Migration Stage"))
  field_changes <- data.frame()
  if (length(stage_col) > 0) {
    sc <- stage_col[1]
    both <- merge(new_df[, c(key, sc)], old_df[, c(key, sc)],
                  by = key, suffixes = c(".new", ".old"))
    changed       <- .field_changed(both, sc)
    field_changes <- both[changed, , drop = FALSE]
  }

  msg_parts <- c(
    if (nrow(added) > 0)        paste(nrow(added),        "new account(s) in SaaS pipeline"),
    if (nrow(removed) > 0)      paste(nrow(removed),      "account(s) exited SaaS pipeline"),
    if (nrow(field_changes) > 0) paste(nrow(field_changes), "stage change(s)")
  )
  if (length(msg_parts)) message("  saas_pipeline: ", paste(msg_parts, collapse = ", "))

  list(added = added, removed = removed, field_changes = field_changes)
}


.diff_consumption <- function(new_df, old_df, user_drop_threshold = 0.10) {
  if (is.null(new_df) || is.null(old_df)) return(list())

  key <- if ("Account ID" %in% names(new_df) && "Account ID" %in% names(old_df)) {
    "Account ID"
  } else if ("Account Name" %in% names(new_df) && "Account Name" %in% names(old_df)) {
    "Account Name"
  } else {
    return(list())
  }

  watch_new <- intersect(c(key, "Account Name", "Registered Users",
                            "Data Sources Count", "Active Policies (Total)"),
                          names(new_df))
  watch_old <- intersect(c(key, "Registered Users",
                            "Data Sources Count", "Active Policies (Total)"),
                          names(old_df))

  # Need the metric columns on both sides to compare anything
  if (!all(c("Registered Users", "Data Sources Count", "Active Policies (Total)") %in% watch_new) ||
      !all(c("Registered Users", "Data Sources Count", "Active Policies (Total)") %in% watch_old)) {
    return(list())
  }

  both <- merge(new_df[, watch_new], old_df[, watch_old],
                by = key, suffixes = c(".new", ".old"))

  num <- function(x) suppressWarnings(as.numeric(x))

  # If an account had a value at diff_date but has none now (e.g. it lost its
  # Production instance entirely — see op_list_consumption()'s "no Production
  # instance" rule), treat the current value as 0 rather than "unmeasurable":
  # going to nothing is the biggest possible drop, not a data gap to ignore.
  zero_if_lost <- function(new_val, old_val) ifelse(is.na(new_val) & !is.na(old_val), 0, new_val)

  users_new <- zero_if_lost(num(both$`Registered Users.new`), num(both$`Registered Users.old`))
  users_old <- num(both$`Registered Users.old`)
  user_drop_pct <- ifelse(!is.na(users_old) & users_old > 0,
                          (users_old - users_new) / users_old, NA_real_)
  user_drop <- !is.na(user_drop_pct) & user_drop_pct > user_drop_threshold

  ds_new <- zero_if_lost(num(both$`Data Sources Count.new`), num(both$`Data Sources Count.old`))
  ds_old <- num(both$`Data Sources Count.old`)
  ds_drop <- !is.na(ds_new) & !is.na(ds_old) & ds_new < ds_old

  pol_new <- zero_if_lost(num(both$`Active Policies (Total).new`), num(both$`Active Policies (Total).old`))
  pol_old <- num(both$`Active Policies (Total).old`)
  pol_drop <- !is.na(pol_new) & !is.na(pol_old) & pol_new < pol_old

  any_flag <- user_drop | ds_drop | pol_drop

  field_changes <- both[any_flag, , drop = FALSE]
  field_changes$user_drop_pct <- round(user_drop_pct[any_flag] * 100, 1)
  field_changes$user_drop     <- user_drop[any_flag]
  field_changes$ds_drop       <- ds_drop[any_flag]
  field_changes$pol_drop      <- pol_drop[any_flag]

  msg_parts <- c(
    if (sum(user_drop) > 0) paste(sum(user_drop), paste0("user count drop(s) >", user_drop_threshold * 100, "%")),
    if (sum(ds_drop) > 0)   paste(sum(ds_drop),   "data source drop(s)"),
    if (sum(pol_drop) > 0)  paste(sum(pol_drop),  "policy drop(s)")
  )
  if (length(msg_parts)) message("  consumption: ", paste(msg_parts, collapse = ", "))

  # Per-account deltas for *every* account present in both snapshots (not just
  # flagged drops) — used to show a "Changes in ..." column for every account
  # on the Consumption tab, not only the ones that declined.
  all_deltas <- stats::setNames(
    data.frame(both[[key]], ds_new - ds_old, pol_new - pol_old, stringsAsFactors = FALSE),
    c(key, "Changes in Data Sources Count", "Changes in Active Policies")
  )

  list(field_changes = field_changes, all_deltas = all_deltas)
}


.diff_forecast <- function(new_df, old_df) {
  # NULL, not list(): a snapshot predating op_list$forecast has nothing to diff,
  # and .build_change_summary() drops NULL components rather than emitting a
  # phantom "forecast 0 0 0 0" row and a dead stat on the dashboard header.
  if (is.null(new_df) || is.null(old_df))               return(NULL)
  if (!is.data.frame(new_df) || !is.data.frame(old_df)) return(NULL)

  key <- "Opportunity ID"
  if (!key %in% names(new_df) || !key %in% names(old_df)) return(NULL)

  new_ids <- as.character(new_df[[key]])
  old_ids <- as.character(old_df[[key]])

  added   <- new_df[new_ids %in% setdiff(new_ids, old_ids), , drop = FALSE]
  removed <- old_df[old_ids %in% setdiff(old_ids, new_ids), , drop = FALSE]

  # Blank forecast == zero forecast churn (op_list_forecast() already does this,
  # but older snapshots may predate that, so normalise both sides here too).
  norm_num <- function(df, col) {
    if (!col %in% names(df)) return(df)
    df[[col]] <- suppressWarnings(as.numeric(gsub("[$,]", "", as.character(df[[col]]))))
    df[[col]][is.na(df[[col]])] <- 0
    df
  }
  new_df <- norm_num(new_df, "Churn Forecast")
  old_df <- norm_num(old_df, "Churn Forecast")

  # Identity columns come from the new side only; watched fields come from both
  # and get .new/.old suffixes via merge().
  #
  # Every field listed here must also be rendered by renderForecastChanges() in
  # report_all_accounts_html.R — a field that counts toward any_changed but has
  # no column produces a row on the dashboard with every cell dashed out. That
  # rules out Customer Success Manager (already reported under CSM Assignments)
  # and Close Date Current Contract (Close Date already covers date slippage).
  watched <- c("Churn Quarter", "Churn Forecast", "Account Risk", "Amount",
               "Stage", "Close Date")
  watched <- intersect(intersect(watched, names(new_df)), names(old_df))
  if (length(watched) == 0) return(NULL)

  ident <- intersect(c(key, "Opportunity Name", "Account Name", "Account ID",
                       "Region", "Next Step", "opportunity_url"),
                     names(new_df))

  both <- merge(new_df[, unique(c(ident, watched)), drop = FALSE],
                old_df[, unique(c(key, watched)), drop = FALSE],
                by = key, suffixes = c(".new", ".old"))

  # .fc_changed(), not the shared .field_changed(): most of this report's rows
  # have no Churn Quarter set, and .field_changed() ignores any comparison
  # where either side is NA — so a quarter being *assigned* (blank -> Q3-2027),
  # which is precisely the forecast move worth seeing, would go unreported.
  flags <- stats::setNames(
    lapply(watched, function(f) .fc_changed(both, f)),
    watched
  )
  any_changed <- Reduce(`|`, flags)

  field_changes <- both[any_changed, , drop = FALSE]

  # One boolean per watched field, named the way the dashboard expects
  # (quarter_changed, forecast_changed, risk_changed, ...) so a column can be
  # blanked out when that particular field didn't move.
  flag_names <- c("Churn Quarter"  = "quarter_changed",
                  "Churn Forecast" = "forecast_changed",
                  "Account Risk"   = "risk_changed",
                  "Amount"         = "amount_changed",
                  "Stage"          = "stage_changed",
                  "Close Date"     = "date_changed")
  for (f in watched) {
    field_changes[[unname(flag_names[f])]] <- flags[[f]][any_changed]
  }

  if (all(c("Churn Forecast.new", "Churn Forecast.old") %in% names(field_changes))) {
    field_changes$forecast_delta <-
      suppressWarnings(as.numeric(field_changes$`Churn Forecast.new`)) -
      suppressWarnings(as.numeric(field_changes$`Churn Forecast.old`))
  }

  msg_parts <- c(
    if (nrow(added) > 0)   paste(nrow(added),   "new forecast row(s)"),
    if (nrow(removed) > 0) paste(nrow(removed), "removed forecast row(s)"),
    if (!is.null(flags[["Churn Forecast"]]) && sum(flags[["Churn Forecast"]]) > 0)
      paste(sum(flags[["Churn Forecast"]]), "forecast change(s)"),
    if (!is.null(flags[["Churn Quarter"]]) && sum(flags[["Churn Quarter"]]) > 0)
      paste(sum(flags[["Churn Quarter"]]), "churn quarter move(s)"),
    if (!is.null(flags[["Account Risk"]]) && sum(flags[["Account Risk"]]) > 0)
      paste(sum(flags[["Account Risk"]]), "risk change(s)")
  )
  if (length(msg_parts)) message("  forecast: ", paste(msg_parts, collapse = ", "))

  list(added = added, removed = removed, field_changes = field_changes)
}


# ── Summary table ─────────────────────────────────────────────────────────────

.build_change_summary <- function(changes) {
  components <- c("accounts", "renewals", "expansions", "csm_table", "saas_pipeline",
                   "escalations", "consumption", "forecast")

  rows <- lapply(components, function(comp) {
    ch <- changes[[comp]]
    if (is.null(ch)) return(NULL)

    added   <- if (!is.null(ch$added))        nrow(ch$added)        else 0L
    removed <- if (!is.null(ch$removed))      nrow(ch$removed)      else 0L
    changed <- if (!is.null(ch$field_changes)) nrow(ch$field_changes) else
               if (!is.null(ch$reassignments)) nrow(ch$reassignments) else 0L

    data.frame(
      Component   = comp,
      Added       = added,
      Removed     = removed,
      Changed     = changed,
      Total_Moves = added + removed + changed,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}


# ── Legacy helpers ────────────────────────────────────────────────────────────

.legacy_renewal_changes <- function(new_ren, old_ren) {
  if (is.null(new_ren) || is.null(old_ren)) return(data.frame())

  new_ren$`Churn Forecast`[is.na(new_ren$`Churn Forecast`)] <- 0
  old_ren$`Churn Forecast`[is.na(old_ren$`Churn Forecast`)] <- 0

  new_sel <- new_ren[, intersect(c("Opportunity ID", "Opportunity Name",
                                    "churn_forecast_period", "Account Risk",
                                    "Churn Forecast"), names(new_ren))]
  old_sel <- old_ren[, intersect(c("Opportunity ID", "churn_forecast_period",
                                    "Account Risk", "Churn Forecast"), names(old_ren))]

  both <- merge(new_sel, old_sel, by = "Opportunity ID",
                all.x = TRUE, suffixes = c(".x", ".y"))
  both[is.na(both)] <- FALSE

  risk_changed     <- as.character(both$`Account Risk.x`) != as.character(both$`Account Risk.y`)
  forecast_changed <- as.character(both$`Churn Forecast.x`) != as.character(both$`Churn Forecast.y`)
  any_changed      <- risk_changed | forecast_changed

  message(paste("  renewal_changes (legacy):", sum(risk_changed), "risk,",
                sum(forecast_changed), "forecast,", sum(any_changed), "total"))

  out <- data.table::data.table(
    `Opportunity Name` = both$`Opportunity Name`[any_changed],
    `Fiscal Period`    = both$churn_forecast_period[any_changed],
    `New Account Risk` = both$`Account Risk.x`[any_changed],
    `Old Account Risk` = both$`Account Risk.y`[any_changed],
    `New Forecast`     = both$`Churn Forecast.x`[any_changed],
    `Old Forecast`     = both$`Churn Forecast.y`[any_changed]
  )
  out$forecast_change <- out$`New Forecast` - out$`Old Forecast`
  out
}


.build_customer_ops_tracker <- function(new_list) {
  temp_exp <- new_list$expansions[, intersect(c("Account ID", "Amount"),
                                               names(new_list$expansions))]
  temp_exp <- dplyr::group_by(temp_exp, `Account ID`)
  temp_exp <- dplyr::summarise(temp_exp,
                                total_amount      = sum(Amount, na.rm = TRUE),
                                active_expansions = dplyr::n(),
                                .groups = "drop")

  keep_cols <- intersect(c("Account ID", "Opportunity Name", "Region",
                            "Opportunity Owner", "Customer Success Manager",
                            "Fiscal Period", "churn_forecast_period",
                            "Close Date", "Amount", "Churn Forecast",
                            "Next Step", "Account Risk", "Adoption Phase"),
                          names(new_list$renewals))
  temp_ren <- new_list$renewals[, keep_cols]

  merge(temp_ren, temp_exp, by = "Account ID", all.x = TRUE)
}


# ── Shared micro-helper ───────────────────────────────────────────────────────

.field_changed <- function(df, field) {
  new_col <- paste0(field, ".new")
  old_col <- paste0(field, ".old")
  if (!new_col %in% names(df) || !old_col %in% names(df)) {
    return(rep(FALSE, nrow(df)))
  }
  n <- as.character(df[[new_col]])
  o <- as.character(df[[old_col]])
  !is.na(n) & !is.na(o) & n != o
}


# Like .field_changed(), but treats NA and "" as the same empty value and does
# compare against it — so a field being filled in or cleared counts as a
# change. Used by .diff_forecast(), where blank is the common starting state.
.fc_changed <- function(df, field) {
  new_col <- paste0(field, ".new")
  old_col <- paste0(field, ".old")
  if (!new_col %in% names(df) || !old_col %in% names(df)) {
    return(rep(FALSE, nrow(df)))
  }
  norm <- function(v) {
    v <- trimws(as.character(v))
    v[is.na(v) | v == "NA"] <- ""
    v
  }
  norm(df[[new_col]]) != norm(df[[old_col]])
}
