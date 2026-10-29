# Product adoption: PBAC complexity plus Request and Comply adoption points,
# per account, and the stoplight rating derived from it.
#
# Adds columns to op_list$consumption$pbac_adoption rather than living in a
# table of its own, because it shares that table's grain (one row per account)
# and its second axis (consumption_ratio). Called from enrich_opportunities()
# AFTER the Request adoption derivation, since the Looker request stage lands
# on op_list$accounts there — op_list_consumption() runs too early to see it.

# Request points by stage. Retained is the stage worth building a renewal story
# on, so it carries the weight of a multi-platform deployment; adopted is one
# signal among several, like the single-weight PBAC components.
.REQUEST_ADOPTION_POINTS <- c(retained = 3, adopted = 1)

# Rating threshold on product_adoption. Same value as the PBAC complexity_min,
# applied to the wider score.
.PRODUCT_ADOPTION_MIN <- 4

# Request adoption stage per account: "retained" / "adopted" / "tried" /
# "none", NA when unknown. Uses the Looker-derived stage where an account has
# one and falls back to the Salesforce `Request Adoption` picklist otherwise
# (green = retained, amber = adopted) — the same precedence adoption_rows()
# uses in R/utils_internal.R, so every surface agrees on an account's stage.
.request_stage <- function(accts) {
  n <- nrow(accts)
  looker <- if ("request_adoption_looker_stage" %in% names(accts)) {
    tolower(trimws(as.character(accts$request_adoption_looker_stage)))
  } else rep(NA_character_, n)
  looker[!is.na(looker) & !nzchar(looker)] <- NA

  sfdc <- if ("Request Adoption" %in% names(accts)) {
    c(green = "retained", amber = "adopted", red = "tried",
      black = "none")[.csm_stoplight(accts$`Request Adoption`)]
  } else rep(NA_character_, n)

  unname(ifelse(is.na(looker), sfdc, looker))
}

#' Product adoption score and rating, per account
#'
#' Extends \code{op_list$consumption$pbac_adoption} (see
#' \code{\link{op_list_pbac_adoption}}) with a combined product adoption score:
#' PBAC \code{complexity_score} plus points for Request and Comply adoption.
#'
#' \describe{
#'   \item{\code{request_stage}}{\code{"retained"}, \code{"adopted"},
#'     \code{"tried"}, \code{"none"} or \code{NA}. The Looker-derived
#'     \code{request_adoption_looker_stage} where present, otherwise the
#'     Salesforce \code{Request Adoption} picklist (green = retained,
#'     amber = adopted).}
#'   \item{\code{request_points}}{\code{3} for retained, \code{1} for adopted,
#'     \code{0} otherwise (including unknown).}
#'   \item{\code{comply_points}}{\code{1} each for \code{access_summary_export}
#'     and \code{comply_enabled} in look 1814's Features List (0-2), from
#'     \code{op_list$accounts$comply_points}; \code{0} when absent.}
#'   \item{\code{product_adoption}}{\code{complexity_score + request_points +
#'     comply_points}, 0-12.}
#'   \item{\code{product_adoption_rating}}{The same two-axis stoplight as
#'     \code{pbac_adoption}, with \code{product_adoption} in place of
#'     \code{complexity_score}: threshold \code{4} on the score, \code{0.25} /
#'     \code{0.75} on \code{consumption_ratio}.}
#' }
#'
#' \code{\link{enrich_opportunities}} also copies \code{product_adoption} and
#' \code{product_adoption_rating} onto \code{op_list$accounts}.
#'
#' The PBAC-only \code{complexity_score} and \code{pbac_adoption} columns are
#' left untouched, so the per-account PBAC rating shown elsewhere does not
#' move.
#'
#' @param pbac_adoption Data frame. \code{op_list$consumption$pbac_adoption}.
#' @param accounts Data frame. \code{op_list$accounts}, for the Request stage.
#' @param points Named numeric vector of points per Request stage. Defaults to
#'   \code{.REQUEST_ADOPTION_POINTS} (retained 3, adopted 1).
#' @param adoption_min Numeric. \code{product_adoption} at or above which the
#'   account counts as high adoption for the rating. Default \code{4}.
#'
#' @return \code{pbac_adoption} with the five columns above appended (replaced
#'   if already present).
#' @examples
#' \dontrun{
#'   op_list$consumption$pbac_adoption <- op_list_product_adoption(
#'     op_list$consumption$pbac_adoption, op_list$accounts)
#' }
#' @export
op_list_product_adoption <- function(pbac_adoption,
                                     accounts,
                                     points       = .REQUEST_ADOPTION_POINTS,
                                     adoption_min = .PRODUCT_ADOPTION_MIN) {

  if (is.null(pbac_adoption) || !is.data.frame(pbac_adoption) ||
      nrow(pbac_adoption) == 0) {
    return(pbac_adoption)
  }
  if (is.null(accounts) || !("Account Name" %in% names(accounts))) {
    stop("`accounts` must be op_list$accounts, with an `Account Name` column.")
  }

  accts <- accounts[!duplicated(trimws(as.character(accounts$`Account Name`))), ,
                    drop = FALSE]
  stage <- .request_stage(accts)[
    match(trimws(as.character(pbac_adoption$`Account Name`)),
          trimws(as.character(accts$`Account Name`)))]

  pts <- unname(points[stage])
  pts[is.na(pts)] <- 0

  # Comply points (op_list_comply_adoption(), on accounts). 0 when absent.
  cpts <- if ("comply_points" %in% names(accts)) {
    suppressWarnings(as.numeric(accts$comply_points))[
      match(trimws(as.character(pbac_adoption$`Account Name`)),
            trimws(as.character(accts$`Account Name`)))]
  } else rep(0, nrow(pbac_adoption))
  cpts[is.na(cpts)] <- 0

  out <- pbac_adoption
  out$request_stage           <- stage
  out$request_points          <- pts
  out$comply_points           <- cpts
  out$product_adoption        <- suppressWarnings(as.numeric(out$complexity_score)) + pts + cpts
  out$product_adoption_rating <- .pbac_rating(out$product_adoption,
                                              out$consumption_ratio,
                                              complexity_min = adoption_min)
  out
}
