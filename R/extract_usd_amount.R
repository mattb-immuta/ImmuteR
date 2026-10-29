#' Strip Salesforce currency formatting and coerce to numeric.
#'
#' Converts Salesforce currency strings such as \code{"USD 792,500.00"} to
#' numbers. Removes the \code{"USD"} code, a \code{"$"} sign, commas and
#' whitespace. Handles negatives written as \code{"-USD 5"}, \code{"USD -5"} or
#' \code{"(USD 5)"}. Salesforce's blank placeholders (\code{"-"}, \code{"—"},
#' \code{""}) become \code{NA}, as does anything else that isn't a number. A
#' numeric vector is returned unchanged, so it is safe to call twice.
#'
#' @param currency_vector Character (or numeric) vector of Salesforce currency
#'   values, e.g. \code{c("USD 1,250,000.00", "USD 300,000")}.
#'
#' @return Numeric vector of the same length as \code{currency_vector}.
#'
#' @examples
#' extract_usd_amount(c("USD 1,250,000", "USD 300,000.50", "-", "(USD 10)"))
#' # [1] 1250000.0  300000.5        NA      -10.0
#' @export
extract_usd_amount <- function(currency_vector) {
  if (is.numeric(currency_vector)) return(as.numeric(currency_vector))
  x <- trimws(as.character(currency_vector))
  neg <- grepl("^\\(.*\\)$", x) | grepl("^-\\s*(USD|\\$)", x, ignore.case = TRUE)
  x <- gsub("[(),$\\s]|USD", "", x, ignore.case = TRUE, perl = TRUE)
  x[x %in% c("", "-", "—", "–")] <- NA
  out <- suppressWarnings(as.numeric(x))
  out[neg & !is.na(out)] <- -abs(out[neg & !is.na(out)])
  out
}


#' Convert every Salesforce currency column in a data frame to numeric.
#'
#' Finds character columns whose values are all Salesforce currency strings
#' (\code{"USD 792,500.00"}), apart from blank placeholders (\code{"-"},
#' \code{""}, \code{NA}), and converts them with \code{\link{extract_usd_amount}}.
#' A column needs at least one \code{"USD"} value to qualify, so free-text and
#' ID columns are never touched. Every Salesforce report the package pulls goes
#' through this, which is why currency fields across \code{op_list} are numbers.
#'
#' @param df A data frame (or tibble). Anything else is returned unchanged.
#' @param cols Optional character vector of column names to convert regardless
#'   of detection.
#'
#' @return \code{df} with the currency columns converted to numeric.
#'
#' @examples
#' convert_usd_columns(data.frame(ARR = c("USD 1,000.00", "-"), Name = c("a", "b")))
#' @export
convert_usd_columns <- function(df, cols = NULL) {
  if (!is.data.frame(df) || ncol(df) == 0) return(df)
  usd_cols <- names(df)[vapply(df, .is_usd_column, logical(1))]
  for (col in union(usd_cols, intersect(cols, names(df)))) {
    df[[col]] <- extract_usd_amount(df[[col]])
  }
  df
}


# ── Internal ──────────────────────────────────────────────────────────────────

# A column is currency when every non-blank value looks like "USD 1,234.56"
# (optionally negative) and at least one value is present.
.is_usd_column <- function(x) {
  if (!is.character(x) && !is.factor(x)) return(FALSE)
  v <- trimws(as.character(x))
  v <- v[!is.na(v) & !(v %in% c("", "-", "—", "–"))]
  length(v) > 0 &&
    all(grepl("^\\(?-?\\s*USD\\s*-?[0-9][0-9,]*(\\.[0-9]+)?\\)?$", v,
              ignore.case = TRUE))
}

# Run a Salesforce report and convert its currency columns. Use this instead of
# calling salesforcer::sf_run_report() directly.
.sf_run_report <- function(report_id, ...) {
  convert_usd_columns(salesforcer::sf_run_report(report_id, ...))
}

# Apply convert_usd_columns() to every data frame inside a nested list, e.g. a
# whole op_list. Used on prior snapshots saved before the conversion existed,
# so diffs compare numbers with numbers.
.convert_usd_deep <- function(x) {
  if (is.data.frame(x)) return(convert_usd_columns(x))
  if (is.list(x) && length(x) > 0) x[] <- lapply(x, .convert_usd_deep)
  x
}
