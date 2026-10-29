# Derive each account's Comply adoption tier from the feature flags in look
# 1814's Features List. This metrics-based tier REPLACES the hand-maintained
# Salesforce `Comply Adoption` picklist in every output (enrich_opportunities()
# overwrites the column and keeps the original as `Comply Adoption (SFDC)`):
#
#   amber -> `comply_enabled` is on                    (+1 adoption point)
#   red   -> `access_summary_export` is on, no Comply  (+1 adoption point)
#   black -> neither flag is on
#   green -> reserved; nothing is green yet
#
# Points are additive: an account with both flags scores +2 and is Amber.

.COMPLY_ADOPTION_TIERS <- c(
  amber = "Amber - Comply enabled",
  red   = "Red - Access summary export enabled",
  black = "Black - Comply features not enabled"
)

# Feature flags (exact Features List tokens) and the points each is worth.
.COMPLY_FEATURE_FLAGS <- c(export = "access_summary_export",
                           comply = "comply_enabled")
.COMPLY_FEATURE_POINTS <- 1

#' Derive Comply adoption tiers from Looker look 1814's feature flags
#'
#' Reads each account's Features List (look 1814) and assigns a Comply tier,
#' attached by \code{\link{enrich_opportunities}} to \code{op_list$accounts}:
#'
#' \describe{
#'   \item{\strong{Amber}}{\code{comply_enabled} is on.}
#'   \item{\strong{Red}}{\code{access_summary_export} is on (and
#'     \code{comply_enabled} is not).}
#'   \item{\strong{Black}}{Neither flag is on.}
#' }
#' Each flag also adds \code{1} point to the product adoption score
#' (\code{comply_points}, 0-2; see \code{\link{op_list_product_adoption}}).
#'
#' Flags are matched as whole, case-insensitive tokens of the comma-separated
#' Features List. When the look has an Instance Type column only Production
#' rows count; an account has a flag if any Production instance does.
#'
#' @param accounts Data frame. \code{op_list$accounts}; needs
#'   \code{Account Name}.
#' @param comply Data frame. Look 1814 (\code{op_list$consumption$comply}).
#'   \code{NULL} means no Looker data: every account is Black with 0 points.
#' @param export_flag,comply_flag Character. Feature List tokens for the two
#'   flags.
#' @param production_only Logical. Count Production instances only.
#'
#' @return A tibble, one row per row of \code{accounts}: \code{Account Name},
#'   \code{comply_export_enabled} (access_summary_export on),
#'   \code{comply_feature_enabled} (comply_enabled on), \code{comply_points},
#'   \code{comply_adoption_looker_stage} (\code{"amber"}/\code{"red"}/\code{"black"})
#'   and \code{comply_adoption_looker} (picklist-format string).
#' @export
op_list_comply_adoption <- function(accounts,
                                    comply,
                                    export_flag     = .COMPLY_FEATURE_FLAGS[["export"]],
                                    comply_flag     = .COMPLY_FEATURE_FLAGS[["comply"]],
                                    production_only = TRUE) {
  if (is.null(accounts) || !("Account Name" %in% names(accounts))) {
    stop("`accounts` must be op_list$accounts, with an `Account Name` column.")
  }

  name_raw   <- as.character(accounts$`Account Name`)
  name_vec   <- trimws(name_raw)
  export_on  <- rep(FALSE, length(name_vec))
  comply_on  <- rep(FALSE, length(name_vec))

  if (!is.null(comply) && nrow(comply) > 0) {
    acct_col <- .looker_account_col(comply)
    list_col <- .looker_col(comply, "features? list")
    if (is.na(acct_col) || is.na(list_col)) {
      stop("Look 1814: no account-name or Features List column (have: ",
           paste(names(comply), collapse = ", "), ").")
    }

    type_col <- .looker_col(comply, "instance type")
    if (production_only && !is.na(type_col)) {
      comply <- comply[comply[[type_col]] %in% "Production", , drop = FALSE]
    }

    tokens <- lapply(strsplit(tolower(as.character(comply[[list_col]])), ",",
                              fixed = TRUE), trimws)
    has <- function(flag) vapply(tokens, function(t) tolower(flag) %in% t, logical(1))

    agg <- tibble::tibble(
      `Account Name` = trimws(as.character(comply[[acct_col]])),
      .exp           = has(export_flag),
      .cmp           = has(comply_flag)
    ) |>
      dplyr::group_by(`Account Name`) |>
      dplyr::summarise(export = any(.exp), comply = any(.cmp), .groups = "drop")

    i <- match(name_vec, agg$`Account Name`)
    export_on <- !is.na(i) & agg$export[i] %in% TRUE
    comply_on <- !is.na(i) & agg$comply[i] %in% TRUE
  }

  stage <- ifelse(comply_on, "amber", ifelse(export_on, "red", "black"))

  out <- tibble::tibble(
    `Account Name`               = name_raw,
    comply_export_enabled        = export_on,
    comply_feature_enabled       = comply_on,
    comply_points                = .COMPLY_FEATURE_POINTS * (export_on + comply_on),
    comply_adoption_looker_stage = stage,
    comply_adoption_looker       = unname(.COMPLY_ADOPTION_TIERS[stage])
  )

  message(".. comply adoption: ", paste(names(table(stage)), table(stage),
                                        sep = " = ", collapse = ", "))
  out
}
