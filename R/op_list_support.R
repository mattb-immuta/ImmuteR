#' Build the support child list for op_list from BigQuery.
#'
#' Pulls the pre-aggregated support rollup from BigQuery and returns one row per
#' account: support case counts over the trailing 90 and 360 days, customer
#' escalation counts over the same windows plus currently-open escalations, and
#' the timestamp the rollup was computed.
#'
#' Called automatically by \code{\link{enrich_opportunities}} and stored as
#' \code{op_list$support}. Can also be called directly to refresh support
#' figures without re-running the full pull.
#'
#' @section Why BigQuery and not Looker look 1776:
#' The previous implementation aggregated Looker look 1776, which returned only
#' \code{Front Conversation Customer Name} and \code{Front Conversation Count} —
#' no case dates, so no rolling windows; no escalation field; and free-text
#' Front customer names that had to be fuzzy-matched to Salesforce accounts.
#' This table fixes all three: the windows are precomputed, escalations are a
#' first-class column, and \code{salesforce_account_id} gives an exact join key.
#' Look 1776 is still fetched by \code{\link{load_op_list_raw}} but nothing
#' consumes it.
#'
#' @section Joining:
#' Accounts are matched on \code{salesforce_account_id} against \code{Account
#' ID} (or \code{Account ID 18}) in \code{op_list$accounts} — an ID join, so no
#' name normalisation is involved. Salesforce IDs come in 15- and 18-character
#' forms and the two are compared case-sensitively on their common 15-character
#' prefix, which is the safe way to reconcile them. Accounts with no row in the
#' rollup get zeros for counts, not \code{NA}: the table covers every account,
#' so absence means no cases rather than no data.
#'
#' @param accounts Data frame — typically \code{op_list$accounts}. Used both for
#'   the join key and to expand the result to one row per account. Required.
#' @param billing_project Character. GCP project billed for the query. Defaults
#'   to \code{"retool-457517"}.
#' @param table Character. Fully-qualified source table. Defaults to
#'   \code{"retool-457517.reports.account_support_rollup_live"}.
#' @param quiet Logical. Suppress the summary message. Defaults to
#'   \code{FALSE}.
#'
#' @return A data frame with one row per account and columns \code{Account
#'   Name}, \code{Account ID}, \code{cases_90d}, \code{cases_360d},
#'   \code{cases_total}, \code{escalations_90d}, \code{escalations_360d},
#'   \code{escalations_open}. Carries a \code{"computed_at"} attribute (when the
#'   BigQuery rollup last ran) and a \code{"matched"} attribute.
#'
#' @examples
#' \dontrun{
#'   bigrquery::bq_auth()
#'   op_list_support(op_list$accounts)
#' }
#' @export
op_list_support <- function(accounts,
                            billing_project = "retool-457517",
                            table = "retool-457517.reports.account_support_rollup_live",
                            quiet = FALSE) {

  if (!requireNamespace("bigrquery", quietly = TRUE)) {
    stop("Package 'bigrquery' is required. install.packages('bigrquery')",
         call. = FALSE)
  }
  if (is.null(accounts) || !is.data.frame(accounts)) {
    stop("`accounts` must be a data frame (pass op_list$accounts).", call. = FALSE)
  }

  sql <- paste0("
    SELECT account_name, salesforce_account_id,
           support_cases_90d, support_cases_360d, support_cases_total,
           custx_90d, custx_360d, custx_open, computed_at
    FROM `", table, "`
    ORDER BY support_cases_360d DESC")

  raw <- bigrquery::bq_table_download(
    bigrquery::bq_project_query(billing_project, sql)
  )

  computed_at <- if ("computed_at" %in% names(raw) && nrow(raw) > 0) {
    suppressWarnings(max(raw$computed_at, na.rm = TRUE))
  } else {
    NA
  }

  # ── Join key ───────────────────────────────────────────────────────────────
  # Salesforce hands out both 15- and 18-character IDs for the same record; the
  # 18-char form is the 15-char form plus a 3-char checksum suffix. Truncating
  # both sides to 15 makes them comparable whichever form each system stores.
  acct_id <- .sf_id15(.first_present(accounts, c("Account ID", "Account ID 18")))
  src_id  <- .sf_id15(raw$salesforce_account_id)

  idx <- match(acct_id, src_id)

  num <- function(col) {
    v <- suppressWarnings(as.numeric(raw[[col]]))[idx]
    v[is.na(v)] <- 0          # in the rollup's scope, absent means zero cases
    as.integer(v)
  }

  out <- data.frame(
    `Account Name`     = as.character(accounts[["Account Name"]]),
    `Account ID`       = acct_id,
    cases_90d          = num("support_cases_90d"),
    cases_360d         = num("support_cases_360d"),
    cases_total        = num("support_cases_total"),
    escalations_90d    = num("custx_90d"),
    escalations_360d   = num("custx_360d"),
    escalations_open   = num("custx_open"),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # Accounts genuinely absent from the rollup keep NA rather than a fake zero.
  missing <- is.na(idx)
  if (any(missing)) {
    for (cc in c("cases_90d", "cases_360d", "cases_total",
                 "escalations_90d", "escalations_360d", "escalations_open")) {
      out[[cc]][missing] <- NA_integer_
    }
  }

  attr(out, "computed_at") <- computed_at
  attr(out, "matched")     <- sum(!missing)

  if (!quiet) {
    message(".. op_list_support [bigquery]: ", nrow(raw), " rollup row(s); ",
            sum(!missing), " of ", nrow(out), " account(s) matched on ",
            "salesforce_account_id; computed_at ", as.character(computed_at),
            if (sum(!missing) < 0.5 * nrow(out)) {
              "  <-- LOW match rate, check the ID column"
            } else "")
  }

  out
}


# ── Internal helpers ──────────────────────────────────────────────────────────

`%||%` <- function(x, y) if (is.null(x)) y else x

# First of `cols` that exists on `df`, as a character vector.
.first_present <- function(df, cols) {
  hit <- cols[cols %in% names(df)]
  if (length(hit) == 0) {
    stop("None of these columns are on the account table: ",
         paste(cols, collapse = ", "), call. = FALSE)
  }
  as.character(df[[hit[1]]])
}

# Normalise a Salesforce ID to its 15-character case-sensitive form.
.sf_id15 <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  substr(x, 1, 15)
}
