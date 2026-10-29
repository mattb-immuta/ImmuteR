#' Build the renewal churn-forecast child table for op_list.
#'
#' Runs the Salesforce renewal churn forecast report and returns one tidy row per
#' renewal opportunity carrying its churn quarter, churn forecast dollars,
#' account risk, amount, stage and dates. Called automatically by
#' \code{\link{enrich_opportunities}} and stored as \code{op_list$forecast};
#' \code{\link{enrich_list_object}} diffs it against the prior snapshot into
#' \code{op_list$changes_detail$forecast}.
#'
#' @section Why this needs its own cleanup step:
#' The source report is a Salesforce *summary* report grouped by Churn Quarter,
#' not a tabular one. Depending on how it comes back it can carry subtotal and
#' grand-total rows (no \code{Opportunity ID}) mixed in with the detail rows,
#' and the grouping column is populated only on the first row of each group.
#' This function drops the aggregate rows and fills the grouping column down so
#' every detail row knows its own churn quarter — without that, diffing a
#' quarter slip between two runs is meaningless.
#'
#' @param sf_forecast_report Character. Salesforce report ID for the renewal
#'   churn forecast report. Defaults to \code{"00OVV000006hrNN2AY"}.
#' @param opportunity_table Optional data frame — typically the raw opportunity
#'   report from \code{\link{enrich_opportunities}}. When supplied,
#'   \code{Account Name} and \code{Account ID} are joined on
#'   \code{Opportunity ID} so downstream reports can link a forecast row back
#'   to its account (the forecast report itself carries only the opportunity
#'   name). Rows with no match keep \code{NA} rather than being dropped.
#' @param quiet Logical. Suppress the summary message. Defaults to
#'   \code{FALSE}.
#'
#' @return A data frame with one row per renewal opportunity. Always carries
#'   \code{Opportunity ID}; carries \code{Churn Quarter}, \code{Churn Forecast},
#'   \code{Account Risk}, \code{Amount}, \code{Stage}, \code{Close Date},
#'   \code{Close Date Current Contract}, \code{Customer Success Manager},
#'   \code{Region}, \code{Adoption Phase}, \code{Next Step} and
#'   \code{Fiscal Period} when the report supplies them, plus
#'   \code{opportunity_url} and (if \code{opportunity_table} was given)
#'   \code{Account Name} / \code{Account ID}.
#'
#' @examples
#' \dontrun{
#'   salesforcer::sf_auth()
#'   fc <- op_list_forecast()
#' }
#' @export
op_list_forecast <- function(sf_forecast_report = "00OVV000006hrNN2AY",
                             opportunity_table  = NULL,
                             quiet              = FALSE) {

  fc <- .sf_run_report(sf_forecast_report)
  fc <- as.data.frame(fc, check.names = FALSE, stringsAsFactors = FALSE)

  # ── Normalise the grouping column name ──────────────────────────────────────
  # Salesforce/exported headers show it as "Churn Quarter" but can pick up a
  # trailing sort arrow and padding ("Churn Quarter  ↑").
  names(fc) <- trimws(gsub("↑|↓", "", names(fc)))
  cq <- grep("^churn quarter$", tolower(names(fc)))
  if (length(cq) == 1) names(fc)[cq] <- "Churn Quarter"

  # ── Normalise / recover the Opportunity ID ──────────────────────────────────
  # Report layouts label the ID differently ("Opportunity ID 18",
  # "Opportunity: Opportunity ID", ...). If there is no ID column at all, look
  # it up from the opportunity report by Opportunity Name.
  if (!"Opportunity ID" %in% names(fc)) {
    id_col <- names(fc)[grepl("^(opportunity[: ]*)?(opportunity )?id( 18)?$|^opp(ortunity)? id",
                              tolower(names(fc)))]
    if (length(id_col)) {
      names(fc)[names(fc) == id_col[1]] <- "Opportunity ID"
    } else if ("Opportunity Name" %in% names(fc) && !is.null(opportunity_table) &&
               all(c("Opportunity Name", "Opportunity ID") %in% names(opportunity_table))) {
      ot  <- as.data.frame(opportunity_table, check.names = FALSE, stringsAsFactors = FALSE)
      ot  <- ot[!duplicated(trimws(ot[["Opportunity Name"]])), , drop = FALSE]
      idx <- match(trimws(as.character(fc[["Opportunity Name"]])),
                   trimws(as.character(ot[["Opportunity Name"]])))
      fc[["Opportunity ID"]] <- as.character(ot[["Opportunity ID"]])[idx]
      # Summary/total rows have no name, so they stay NA and drop out below.
      n_hit <- sum(!is.na(idx))
      n_opp <- sum(!is.na(fc[["Opportunity Name"]]) &
                   nzchar(trimws(as.character(fc[["Opportunity Name"]]))))
      if (!quiet) message("  forecast: Opportunity ID matched by name for ",
                          n_hit, " of ", n_opp, " row(s)")
    }
  }
  # The opportunity report may carry 15- or 18-character IDs; align to its form.
  if ("Opportunity ID" %in% names(fc) && !is.null(opportunity_table) &&
      "Opportunity ID" %in% names(opportunity_table)) {
    ref_len <- stats::median(nchar(stats::na.omit(
      as.character(opportunity_table[["Opportunity ID"]]))))
    if (isTRUE(ref_len == 15)) {
      fc[["Opportunity ID"]] <- substr(as.character(fc[["Opportunity ID"]]), 1, 15)
    }
  }

  # ── Drop subtotal / grand-total rows ────────────────────────────────────────
  # Aggregate rows have no Opportunity ID; some exports also label them in the
  # grouping column. Detail rows always carry an ID.
  if ("Opportunity ID" %in% names(fc)) {
    oid  <- trimws(as.character(fc[["Opportunity ID"]]))
    keep <- !is.na(oid) & nzchar(oid)
    if ("Churn Quarter" %in% names(fc)) {
      lbl  <- tolower(trimws(as.character(fc[["Churn Quarter"]])))
      keep <- keep & !(lbl %in% c("subtotal", "total", "grand total"))
    }
    fc <- fc[keep, , drop = FALSE]
  } else {
    warning("Forecast report has no 'Opportunity ID' column (and none could ",
            "be recovered by Opportunity Name) — it cannot be diffed or joined ",
            "to accounts. Check report ", sf_forecast_report, ". Columns: ",
            paste(names(fc), collapse = ", "), call. = FALSE)
  }

  # ── Fill the Churn Quarter grouping down ────────────────────────────────────
  if ("Churn Quarter" %in% names(fc) && nrow(fc) > 0) {
    fc[["Churn Quarter"]] <- .fc_fill_down(as.character(fc[["Churn Quarter"]]))
  }

  # ── Currency-converted columns ──────────────────────────────────────────────
  # Multi-currency orgs label the USD figures "Churn Forecast (converted)" /
  # "Amount (converted)". Use those when the plain column is absent (or empty),
  # so the forecast isn't silently $0.
  for (base in c("Churn Forecast", "Amount")) {
    conv <- paste0(base, " (converted)")
    if (conv %in% names(fc)) {
      plain_empty <- !base %in% names(fc) ||
        all(is.na(extract_usd_amount(fc[[base]])))
      if (plain_empty) fc[[base]] <- fc[[conv]]
    }
  }
  if (!"Churn Forecast" %in% names(fc)) {
    warning("Forecast report ", sf_forecast_report, " has no 'Churn Forecast' ",
            "column, so forecast churn reads as $0. Columns: ",
            paste(names(fc), collapse = ", "), call. = FALSE)
  }

  # ── Coerce the numeric metrics ──────────────────────────────────────────────
  for (col in c("Churn Forecast", "Amount", "Vitally Days as Customer")) {
    if (col %in% names(fc)) {
      fc[[col]] <- extract_usd_amount(fc[[col]])
    }
  }
  # A blank churn forecast is zero forecast churn, not unknown churn — the diff
  # and the dashboard both treat it that way, so settle it once here.
  if ("Churn Forecast" %in% names(fc)) {
    fc[["Churn Forecast"]][is.na(fc[["Churn Forecast"]])] <- 0
  }

  # ── Opportunity link, matching enrich_opportunities() ───────────────────────
  if ("Opportunity ID" %in% names(fc) && nrow(fc) > 0) {
    fc$opportunity_url <- paste0(
      "https://immuta.lightning.force.com/lightning/r/Opportunity/",
      as.character(fc[["Opportunity ID"]]), "/view"
    )
  }

  # ── Join account context ────────────────────────────────────────────────────
  # The forecast report carries Opportunity Name but no account, so borrow it
  # from the opportunity report on Opportunity ID.
  if (!is.null(opportunity_table) &&
      "Opportunity ID" %in% names(fc) &&
      "Opportunity ID" %in% names(opportunity_table) &&
      nrow(fc) > 0) {

    acct_cols <- intersect(c("Opportunity ID", "Account Name", "Account ID"),
                           names(opportunity_table))
    if (length(acct_cols) > 1) {
      lk <- unique(as.data.frame(opportunity_table, check.names = FALSE,
                                 stringsAsFactors = FALSE)[, acct_cols, drop = FALSE])
      lk <- lk[!duplicated(lk[["Opportunity ID"]]), , drop = FALSE]
      # Drop any columns the forecast report already supplies so merge() can't
      # append .x/.y suffixes and break the downstream column names.
      dupe <- setdiff(intersect(names(lk), names(fc)), "Opportunity ID")
      if (length(dupe)) lk <- lk[, setdiff(names(lk), dupe), drop = FALSE]
      if (ncol(lk) > 1) {
        fc <- merge(fc, lk, by = "Opportunity ID", all.x = TRUE, sort = FALSE)
      }
    }
  }

  if (!quiet) {
    n_q <- if ("Churn Quarter" %in% names(fc)) {
      length(unique(stats::na.omit(fc[["Churn Quarter"]])))
    } else 0L
    total <- if ("Churn Forecast" %in% names(fc)) {
      sum(fc[["Churn Forecast"]], na.rm = TRUE)
    } else NA_real_
    message("  forecast: ", nrow(fc), " renewal(s) across ", n_q,
            " churn quarter(s)",
            if (!is.na(total)) paste0(", $", format(round(total), big.mark = ",",
                                                    scientific = FALSE),
                                      " forecast churn") else "")
  }

  fc
}


# ── Internal ──────────────────────────────────────────────────────────────────

# Carry the last non-blank value forward — Salesforce summary reports populate
# a grouping column only on the first row of each group.
.fc_fill_down <- function(x) {
  x <- trimws(x)
  x[x %in% c("NA", "")] <- NA_character_
  if (all(is.na(x))) return(x)
  idx <- cumsum(!is.na(x))
  idx[idx == 0] <- NA_integer_
  x[!is.na(x)][idx]
}
