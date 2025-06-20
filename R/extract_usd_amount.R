#' convert salesforce currency strings to numeric.
#'
#' @param currency_vector a salesforce currency extract from sf_run.
#' @return a vector translated to numeric.
#' @examples
#' none
#' @export
#'
extract_usd_amount <- function(currency_vector) {
  # Remove "USD " from the beginning of the string
  for(i in 1:length(currency_vector)){currency_vector[i]=sub("^USD\\s*", "", gsub(",", "", currency_vector[i]))}
  currency_vector=as.numeric(currency_vector)
  return(currency_vector)
}
