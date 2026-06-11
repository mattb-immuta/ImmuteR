# Internal utility helpers shared across ImmuteR reporting functions.
# These are NOT exported — they are for use within the package only.


# --- Value safety -------------------------------------------------------------

#' Coerce a value to character, returning an em-dash placeholder for
#' missing/empty values.
#' @noRd
safe_val <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- as.character(x)
  ifelse(is.na(x) | trimws(x) == "", "\u2014", x)
}


# --- Formatting helpers -------------------------------------------------------

#' Format a numeric value as USD currency, e.g. "$1,234,567".
#' Returns an em-dash for missing/non-numeric values.
#' @noRd
fmt_usd <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("\u2014")
  scales::dollar(x)
}

#' Format a Date or date string as "Month DD, YYYY".
#' Returns an em-dash for missing values.
#' @noRd
fmt_date <- function(x) {
  d <- suppressWarnings(as.Date(x))
  if (is.na(d)) return("\u2014")
  format(d, "%B %d, %Y")
}

#' Format a number of days as "X.X years (N days)".
#' Returns an em-dash for missing values.
#' @noRd
fmt_days <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("\u2014")
  paste0(round(x / 365.25, 1), " years (", x, " days)")
}

#' Format a boolean-ish value as a checkmark "Yes" or plain "No".
#' Returns an em-dash for missing/empty values.
#' @noRd
fmt_bool <- function(x) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || x == ""))) return("\u2014")
  if (x %in% c(TRUE, "Yes", "TRUE", "true", "yes")) return("\U2705 Yes")
  if (x %in% c(FALSE, "No", "FALSE", "false", "no")) return("No")
  as.character(x)
}

#' Format a risk level string with an emoji prefix.
#' Returns an em-dash for missing/empty values.
#' @noRd
fmt_risk <- function(x) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || x == ""))) return("\u2014")
  switch(tolower(as.character(x)),
    "high"     = "\U26A0\UFE0F High",
    "moderate" = "\U1F7E1 Moderate",
    "low"      = "\U2705 Low",
    as.character(x)
  )
}


# --- Markdown field helpers ---------------------------------------------------

#' Build a markdown bullet field: "- **Label:** value".
#' Substitutes an em-dash for blank/NA values.
#' @noRd
build_field <- function(label, value) {
  val <- if (is.null(value) || length(value) == 0 ||
             (length(value) == 1 && (is.na(value) || as.character(value) == ""))) {
    "\u2014"
  } else {
    as.character(value)
  }
  paste0("- **", label, ":** ", val)
}

#' Alias for \code{build_field}; used in markdown report builders.
#' @noRd
asana_field <- build_field


# --- HTML helpers -------------------------------------------------------------

#' Escape special HTML characters in a string.
#' @noRd
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Build an Asana-compatible HTML list item: "<li><strong>Label:</strong> value</li>".
#' @noRd
asana_li <- function(label, value) {
  paste0("<li><strong>", label, ":</strong> ", safe_val(value), "</li>")
}
