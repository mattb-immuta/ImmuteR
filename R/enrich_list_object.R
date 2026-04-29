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
#' @return \code{new_list} with three additional elements:
#' \describe{
#'   \item{changes}{Named list with one entry per op_list component
#'     (\code{accounts}, \code{renewals}, \code{expansions}, \code{csm_table},
#'     \code{saas_pipeline}), each containing data frames for additions,
#'     removals, and field-level changes detected since \code{diff_date}.
#'     Also includes a top-level \code{diff_date} and \code{summary} table.}
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

  # ── 6. Summary table (one row per component) ─────────────────────────────────
  changes$summary <- .build_change_summary(changes)

  new_list[["changes"]] <- changes

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
                          "Field CTO", "Region", "Adoption Phase"), names(new_df))],
    old_df[, intersect(c("Account ID", "Contract ARR Rollup",
                          "Customer Success Manager", "Account Owner",
                          "Field CTO", "Region", "Adoption Phase"), names(old_df))],
    by = "Account ID", suffixes = c(".new", ".old")
  )

  watch_fields <- c("Contract ARR Rollup", "Customer Success Manager",
                    "Account Owner", "Field CTO", "Region", "Adoption Phase")
  watch_fields <- watch_fields[watch_fields %in% gsub("\\.new$|\\.old$", "",
                                                       names(both))]

  change_rows <- Reduce(`|`, lapply(watch_fields, function(f) {
    n <- both[[paste0(f, ".new")]]
    o <- both[[paste0(f, ".old")]]
    !is.na(n) & !is.na(o) & as.character(n) != as.character(o)
  }))

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


# ── Summary table ─────────────────────────────────────────────────────────────

.build_change_summary <- function(changes) {
  components <- c("accounts", "renewals", "expansions", "csm_table", "saas_pipeline")

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
