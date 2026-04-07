#' Strip Salesforce currency formatting and coerce to numeric.
#'
#' Removes the \code{"USD "} prefix and any comma separators from a character
#' vector of currency strings exported by Salesforce, then coerces to numeric.
#'
#' @param currency_vector Character vector of Salesforce currency strings,
#'   e.g. \code{c("USD 1,250,000", "USD 300,000")}.
#'
#' @return Numeric vector of the same length as \code{currency_vector}.
#'
#' @examples
#' extract_usd_amount(c("USD 1,250,000", "USD 300,000"))
#' # [1] 1250000  300000
#' @export
extract_usd_amount <- function(currency_vector) {
  currency_vector <- sub("^USD\\s*", "", gsub(",", "", currency_vector))
  as.numeric(currency_vector)
}
