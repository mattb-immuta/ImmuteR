#' Build a feature-by-account matrix of enabled features from Looker look 1814
#'
#' Splits look 1814's comma-separated \code{Features List} into individual
#' feature flags and pivots them into a matrix: one row per unique feature
#' (row names are the feature names), one column per account, with \code{1}
#' where the feature is enabled on that account and \code{NA} (blank in
#' Sheets) otherwise. An account counts as having a feature enabled if any of
#' its counted instances has it.
#'
#' Columns are resolved by name pattern, like the other look-1814 helpers,
#' because Looker's view prefix varies by explore.
#'
#' @param comply Data frame. Look 1814 (\code{op_list$consumption$comply}).
#' @param production_only Logical. Count only Production instances (the
#'   package convention for Looker metrics). Default \code{TRUE}; ignored when
#'   the look has no Instance Type column.
#'
#' @return A \code{data.frame} with features as row names and account names as
#'   columns, or \code{NULL} when \code{comply} is empty or unusable.
#' @export
op_list_enabled_features <- function(comply, production_only = TRUE) {
  if (is.null(comply) || nrow(comply) == 0) return(NULL)

  acct_col <- .looker_account_col(comply)
  list_col <- .looker_col(comply, "features? list")
  if (is.na(acct_col) || is.na(list_col)) {
    warning("Look 1814: couldn't find account/Features List columns (have: ",
            paste(names(comply), collapse = ", "), ") — enabled_features skipped.",
            call. = FALSE)
    return(NULL)
  }

  type_col <- .looker_col(comply, "instance type")
  if (production_only && !is.na(type_col)) {
    comply <- comply[comply[[type_col]] %in% "Production", , drop = FALSE]
  }

  accts <- trimws(as.character(comply[[acct_col]]))
  feats <- strsplit(as.character(comply[[list_col]]), ",", fixed = TRUE)

  long <- data.frame(
    account = rep(accts, lengths(feats)),
    feature = trimws(unlist(feats)),
    stringsAsFactors = FALSE
  )
  long <- long[!is.na(long$account) & long$account != "" &
               !is.na(long$feature) & long$feature != "", , drop = FALSE]
  if (nrow(long) == 0) return(NULL)

  # table() counts instances; any count > 0 means enabled on the account.
  tab <- table(factor(long$feature, levels = sort(unique(long$feature))),
               factor(long$account, levels = sort(unique(long$account))))
  m <- ifelse(unclass(tab) > 0, 1L, NA_integer_)

  out <- as.data.frame(m, stringsAsFactors = FALSE)
  names(out)     <- colnames(tab)   # keep account names verbatim (no make.names)
  rownames(out)  <- rownames(tab)

  message(".. enabled_features: ", nrow(out), " features x ", ncol(out), " accounts")
  out
}
