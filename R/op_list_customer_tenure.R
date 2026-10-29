#' Pull contract start dates and roll them up to customer tenure per account.
#'
#' Reads the Salesforce contract report (\code{00OVV000007MTSL2A4}: every
#' contract start date for current customers) and keeps the earliest start
#' date per account as the date the account became a customer.
#'
#' Column labels are matched loosely (e.g. \code{Contract Start Date},
#' \code{Start Date}, \code{Contract: Start Date}; \code{Account ID} or
#' \code{Account ID 18}) so a renamed report field doesn't break the pull.
#'
#' @param sf_tenure_report Character. Salesforce report ID. Defaults to
#'   \code{"00OVV000007MTSL2A4"}.
#' @param tenure_table Optional data frame. Supply an already-pulled report to
#'   skip the Salesforce call (useful for testing).
#' @param as_of Date. Reference date for \code{months_as_customer}. Defaults to
#'   today.
#'
#' @return A tibble, one row per account: \code{tenure_account_id}
#'   (15-character Account ID), \code{tenure_account_name},
#'   \code{customer_since} (Date, earliest contract start) and
#'   \code{months_as_customer} (integer, whole months elapsed to \code{as_of}).
#'
#' @export
op_list_customer_tenure <- function(sf_tenure_report = "00OVV000007MTSL2A4",
                                    tenure_table     = NULL,
                                    as_of            = Sys.Date()) {
  ct <- if (is.null(tenure_table)) {
    .sf_run_report(sf_tenure_report)
  } else {
    tenure_table
  }
  ct   <- as.data.frame(ct, check.names = FALSE, stringsAsFactors = FALSE)
  cols <- names(ct)
  n    <- nrow(ct)

  pick_col <- function(candidates, pattern = NULL) {
    hit <- intersect(candidates, cols)
    if (length(hit) == 0 && !is.null(pattern))
      hit <- cols[grepl(pattern, cols, ignore.case = TRUE)]
    if (length(hit) == 0) NA_character_ else hit[1]
  }

  start_col <- pick_col(c("Contract Start Date", "Start Date",
                          "Contract: Contract Start Date", "Contract: Start Date",
                          "Subscription Start Date"),
                        pattern = "start[ _.]*date")
  id_col    <- pick_col(c("Account ID", "Account ID 18", "Account: Account ID",
                          "Account: Account ID 18", "AccountId"))
  name_col  <- pick_col(c("Account Name", "Account: Account Name", "Account"))

  if (is.na(start_col)) {
    stop("Tenure report ", sf_tenure_report, ": no contract start date column. ",
         "Columns: ", paste(cols, collapse = ", "), call. = FALSE)
  }
  if (is.na(id_col) && is.na(name_col)) {
    stop("Tenure report ", sf_tenure_report, ": no Account ID or Account Name ",
         "column. Columns: ", paste(cols, collapse = ", "), call. = FALSE)
  }

  clean <- function(v) {
    v <- trimws(as.character(v))
    v[v %in% c("", "-")] <- NA_character_
    v
  }

  acct_id <- if (is.na(id_col)) rep(NA_character_, n) else clean(ct[[id_col]])
  acct_id <- ifelse(is.na(acct_id), NA_character_, substr(acct_id, 1, 15))
  acct_nm <- if (is.na(name_col)) rep(NA_character_, n) else clean(ct[[name_col]])
  start   <- .tenure_as_date(ct[[start_col]])

  d <- data.frame(id = acct_id, name = acct_nm, start = start,
                  stringsAsFactors = FALSE)
  d <- d[!is.na(d$start) & (!is.na(d$id) | !is.na(d$name)), , drop = FALSE]
  d$key <- ifelse(is.na(d$id), paste0("name:", tolower(d$name)), d$id)

  # Earliest contract per account: sort by start date, keep first row per key.
  d   <- d[order(d$key, d$start), , drop = FALSE]
  first_nonmissing <- function(v, k) {
    x <- tapply(v, k, function(z) { z <- z[!is.na(z)]; if (length(z)) z[1] else NA_character_ })
    unname(x[unique(k)])
  }
  keys <- unique(d$key)
  out  <- data.frame(
    tenure_account_id   = first_nonmissing(d$id,   d$key),
    tenure_account_name = first_nonmissing(d$name, d$key),
    customer_since      = d$start[match(keys, d$key)],
    stringsAsFactors    = FALSE
  )

  out$months_as_customer <- .months_between(out$customer_since, as.Date(as_of))

  message("  customer tenure: ", nrow(out), " accounts from ", n,
          " contract rows (start date column: '", start_col, "')")

  tibble::as_tibble(out)
}


# Character / POSIXct / Date -> Date. Salesforce reports usually return ISO
# dates, but US-style m/d/Y shows up on some formatted report columns.
.tenure_as_date <- function(v) {
  if (inherits(v, "Date"))   return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  v   <- trimws(as.character(v))
  out <- as.Date(v, format = "%Y-%m-%d")
  us  <- is.na(out) & !is.na(v)
  out[us] <- as.Date(v[us], format = "%m/%d/%Y")
  out
}

# Whole calendar months from `from` to `to` (a month counts once its
# day-of-month is reached). Never negative — a future start is 0.
.months_between <- function(from, to) {
  lt_f <- as.POSIXlt(from)
  lt_t <- as.POSIXlt(to)
  m <- (lt_t$year - lt_f$year) * 12 + (lt_t$mon - lt_f$mon) -
       (lt_t$mday < lt_f$mday)
  m <- as.integer(m)
  m[!is.na(m) & m < 0] <- 0L
  m
}


#' Append customer_since / months_as_customer to op_list$accounts.
#'
#' Joins by 15-character Account ID, falling back to Account Name.
#'
#' @return \code{accounts} with \code{customer_since} (Date) and
#'   \code{months_as_customer} (integer) columns, \code{NA} where the account
#'   has no contract in the tenure report.
#' @noRd
.accounts_add_tenure <- function(accounts, tenure) {
  accounts$customer_since     <- NULL
  accounts$months_as_customer <- NULL

  if (is.null(tenure) || nrow(tenure) == 0) {
    accounts$customer_since     <- as.Date(NA)
    accounts$months_as_customer <- NA_integer_
    return(accounts)
  }

  acct_key <- substr(as.character(accounts[["Account ID"]]), 1, 15)
  by_id    <- match(acct_key, tenure$tenure_account_id)
  by_name  <- match(tolower(accounts[["Account Name"]]),
                    tolower(tenure$tenure_account_name))
  idx      <- ifelse(is.na(by_id), by_name, by_id)

  accounts$customer_since     <- tenure$customer_since[idx]
  accounts$months_as_customer <- tenure$months_as_customer[idx]

  message("  customer tenure: months_as_customer on ",
          sum(!is.na(accounts$months_as_customer)), " of ", nrow(accounts),
          " accounts")
  accounts
}


#' Fall back to Vitally's "Days as Customer" where it shows a longer tenure.
#'
#' Stopgap for report 00OVV000007MTSL2A4, which currently returns only current
#' contracts, so its earliest start date is the start of the current term and
#' understates tenure for most accounts. Vitally's \code{Vitally Days as
#' Customer} (on every opportunity row; one value per account) is converted to a
#' start date (\code{as_of} minus the days) and used wherever it gives more
#' months than the contract report, or the report has nothing. Once the report
#' carries full contract history it will win on its own and this falls away.
#'
#' @param accounts \code{op_list$accounts} after \code{.accounts_add_tenure()}.
#' @param opps The opportunity report (any rows with \code{Account ID} and
#'   \code{Vitally Days as Customer}).
#' @param as_of Date. Reference date, as for \code{op_list_customer_tenure()}.
#' @return \code{accounts} with \code{customer_since} / \code{months_as_customer}
#'   updated where Vitally is longer, and \code{tenure_source} ("Contract report",
#'   "Vitally (Days as Customer)", or \code{NA}).
#' @noRd
.accounts_add_vitally_tenure <- function(accounts, opps, as_of = Sys.Date()) {
  if (is.null(accounts$months_as_customer)) accounts$months_as_customer <- NA_integer_
  if (is.null(accounts$customer_since))     accounts$customer_since     <- as.Date(NA)
  accounts$tenure_source <- ifelse(is.na(accounts$months_as_customer), NA_character_, "Contract report")

  vcol <- "Vitally Days as Customer"
  if (!is.data.frame(opps) || !all(c("Account ID", vcol) %in% names(opps))) {
    message("  customer tenure: no Vitally Days as Customer on the opportunity report; contract report only")
    return(accounts)
  }
  days <- suppressWarnings(as.numeric(gsub(",", "", as.character(opps[[vcol]]))))
  ids  <- substr(trimws(as.character(opps$`Account ID`)), 1, 15)
  ok   <- !is.na(days) & days >= 0 & !is.na(ids)
  if (!any(ok)) return(accounts)
  v <- tapply(days[ok], ids[ok], max)

  acct_days <- unname(v[substr(trimws(as.character(accounts$`Account ID`)), 1, 15)])
  as_of     <- as.Date(as_of)
  v_since   <- as_of - round(acct_days)
  v_months  <- .months_between(v_since, as_of)

  use <- !is.na(v_months) & (is.na(accounts$months_as_customer) | v_months > accounts$months_as_customer)
  accounts$customer_since[use]     <- v_since[use]
  accounts$months_as_customer[use] <- v_months[use]
  accounts$tenure_source[use]      <- "Vitally (Days as Customer)"
  message("  customer tenure: Vitally longer than the contract report for ", sum(use), " of ",
          nrow(accounts), " accounts (used instead)")
  accounts
}
