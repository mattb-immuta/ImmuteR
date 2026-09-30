# PBAC adoption: the complexity/consumption inputs per account, and the
# stoplight rating derived from them.
#
# Each component is a 0/1 signal that the account is doing something more
# demanding than baseline PBAC, drawn from the Looker looks already on
# op_list$consumption plus the Domains dashboard (1053). Multi-platform is
# weighted 3x because running policy across two or more platforms is the
# qualitative jump; the rest are weighted 1.
#
# Stored as op_list$consumption$pbac_adoption by op_list_consumption().

# Component weights. complexity_score is the weighted sum of the 0/1 columns —
# consumption_ratio is carried alongside for context and is NOT scored, since
# it measures seat uptake rather than deployment complexity.
.PBAC_COMPLEXITY_WEIGHTS <- c(
  multi_platform = 3,
  dns_policies   = 1,
  sdd            = 1,
  domains        = 1,
  guardrail      = 1
)

# Account names in a Looker look matching a condition, as a plain character
# vector. Looker's account column is named by .looker_account_col().
.pbac_accounts_where <- function(df, keep) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  col <- .looker_account_col(df)
  if (is.na(col)) return(character(0))
  keep <- keep & !is.na(keep)
  unique(trimws(as.character(df[[col]][keep])))
}

# Production rows only, when the look carries an instance type at all. The
# SDD-active look (1748) is already account-grain and has no such column.
.pbac_production <- function(df, production_only) {
  if (is.null(df) || !isTRUE(production_only)) return(df)
  if (!("Immuta Instances Instance Type" %in% names(df))) return(df)
  df[!is.na(df$`Immuta Instances Instance Type`) &
       df$`Immuta Instances Instance Type` == "Production", , drop = FALSE]
}

.pbac_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))

# Rating labels. Lowercase leading colour word, which fmtStoplight() in
# report_all_accounts_html.R matches case-insensitively, so these render as
# stoplights like the Salesforce picklists do.
#
# Red is split by which axis is failing, because the two call for opposite
# plays: low complexity with some seat use is a depth problem (adoption risk);
# high complexity with almost no seat use is an enablement problem
# (consumption risk).
.PBAC_ADOPTION_RATINGS <- c(
  black           = "Black: Negligible Value",
  red_adoption    = "Red: Critical Adoption Risk",
  red_consumption = "Red: Critical Consumption Risk",
  amber           = "amber: moderate adoption",
  green           = "green: strong adoption"
)

# `x >= threshold`, with NA reading as FALSE. The rules below are written as
# sequential overwrites (each later rule wins over the earlier ones), which is
# only safe if no mask carries NA.
.pbac_ge <- function(x, threshold) !is.na(x) & x >= threshold

# Rating from complexity score and seat consumption. Two axes, each cut into
# three bands, read as: how much is this account asking of PBAC, and how much
# of what they bought are they using?
#
#                    ratio < 0.25              0.25-0.75                  >= 0.75
#   score >= 4       red: consumption risk     amber                      green
#   score <  4       black                     red: adoption risk         amber
#
# A missing ratio is treated as "not yet demonstrated", not as zero: such an
# account can still reach red on complexity alone, but no further.
.pbac_rating <- function(complexity_score,
                         consumption_ratio,
                         ratio_minimal  = 0.25,
                         ratio_moderate = 0.75,
                         complexity_min = 4) {

  r <- .PBAC_ADOPTION_RATINGS

  out <- rep(unname(r["black"]), length(complexity_score))

  complex   <- .pbac_ge(complexity_score,  complexity_min)
  some_use  <- .pbac_ge(consumption_ratio, ratio_minimal)
  heavy_use <- .pbac_ge(consumption_ratio, ratio_moderate)

  out[some_use]              <- r["red_adoption"]
  out[heavy_use]             <- r["amber"]
  out[complex]               <- r["red_consumption"]
  out[complex & some_use]    <- r["amber"]
  out[complex & heavy_use]   <- r["green"]

  unname(out)
}

#' PBAC adoption rating and its inputs, per account
#'
#' Builds the data frame stored as
#' \code{op_list$consumption$pbac_adoption}: one row per account in
#' \code{accounts}, with a 0/1 column per complexity signal, their weighted sum
#' as \code{complexity_score}, the production seat-consumption ratio, and the
#' \code{pbac_adoption} stoplight rating derived from the last two.
#'
#' \describe{
#'   \item{\code{multi_platform}}{Data sources registered on more than one
#'     platform in Production (\code{accounts$multiplatform}, set by
#'     \code{\link{op_list_consumption}}). Weighted \code{3}.}
#'   \item{\code{dns_policies}}{Both data \emph{and} subscription policies
#'     active — the account is governing access and content, not just one.}
#'   \item{\code{sdd}}{Sensitive Data Discovery run within
#'     \code{sdd_days} days.}
#'   \item{\code{domains}}{Listed on the Domains dashboard's retained-accounts
#'     tile (see \code{\link{looker_tile_accounts}}).}
#'   \item{\code{guardrail}}{At least one active guardrail policy.}
#'   \item{\code{complexity_score}}{Weighted sum of the five columns above.}
#'   \item{\code{consumption_ratio}}{Registered users over licensed users in
#'     Production — the same figure the health-summary tiles show (see
#'     \code{\link{generate_slide_tile}}). Deliberately \strong{not} part of
#'     \code{complexity_score}: it measures seat uptake, not deployment
#'     complexity. It is instead the second axis of the rating below.}
#'   \item{\code{pbac_adoption}}{The rating, as a
#'     \code{"<colour>: <label>"} string in the same shape as the Salesforce
#'     adoption picklists, so the existing stoplight renderers read it
#'     unchanged.}
#' }
#'
#' The rating crosses the two axes — how much the account is asking of PBAC,
#' and how much of what they bought they are using:
#'
#' \tabular{llll}{
#'   \tab \strong{ratio < 0.25} \tab \strong{0.25-0.75} \tab \strong{>= 0.75} \cr
#'   \strong{score >= 4} \tab red \tab amber \tab green \cr
#'   \strong{score < 4}  \tab black \tab red \tab amber
#' }
#'
#' An account missing from a look scores \code{0} for that component, not
#' \code{NA} — absence from the SDD look means no SDD run, which is a real
#' zero. \code{consumption_ratio} stays \code{NA} when it can't be computed,
#' since an unknown ratio is not the same as a ratio of zero; for the rating a
#' missing ratio reads as "not yet demonstrated", so such an account can reach
#' red on complexity alone but no further.
#'
#' \strong{ELAs.} A licensed-user count of \code{0} is an Enterprise License
#' Agreement — unlimited seats, no denominator — not missing data, so the ratio
#' is \code{1} rather than \code{NA}. See \code{consumption_ratio()} in
#' R/utils_internal.R, which every caller of this figure shares.
#'
#' @param accounts Data frame. \code{op_list$accounts}; needs
#'   \code{Account Name}, and uses \code{multiplatform} when present.
#' @param low_consumption Data frame or \code{NULL}.
#'   \code{op_list$consumption$low_consumption}, for the consumption ratio.
#' @param sdd_consumption Data frame or \code{NULL}. The SDD-active look
#'   (1748), i.e. \code{op_list$consumption$sdd_consumption}.
#' @param policy_metrics Data frame or \code{NULL}. The policies look (1747),
#'   i.e. \code{op_list$consumption$policy_metrics}.
#' @param domain_accounts Character vector or \code{NULL}. Accounts using
#'   domains, normally \code{looker_tile_accounts(1053, "retained accounts")}.
#' @param sdd_days Numeric. How recent an SDD run has to be to count. Default
#'   \code{90}.
#' @param production_only Logical. Restrict the policy look to Production
#'   instances before testing the policy signals. Default \code{TRUE} —
#'   a guardrail policy on a dev instance is not production complexity, and
#'   this matches how \code{\link{op_list_consumption}} treats every other
#'   policy figure. Set \code{FALSE} to count any instance.
#' @param weights Named numeric vector of component weights. Defaults to
#'   \code{.PBAC_COMPLEXITY_WEIGHTS} (multi_platform 3, the rest 1).
#' @param ratio_minimal Numeric. Consumption ratio at or above which an
#'   account counts as using its seats at all. Default \code{0.25}.
#' @param ratio_moderate Numeric. Consumption ratio at or above which uptake
#'   counts as strong. Default \code{0.75}.
#' @param complexity_min Numeric. \code{complexity_score} at or above which a
#'   deployment counts as complex. Default \code{4}.
#'
#' @return A data frame with \code{Account Name}, the five component columns,
#'   \code{complexity_score}, \code{consumption_ratio} and
#'   \code{pbac_adoption}.
#' @examples
#' \dontrun{
#'   op_list$consumption$pbac_adoption
#'
#'   op_list_pbac_adoption(
#'     accounts        = op_list$accounts,
#'     low_consumption = op_list$consumption$low_consumption,
#'     sdd_consumption = op_list$consumption$sdd_consumption,
#'     policy_metrics  = op_list$consumption$policy_metrics,
#'     domain_accounts = looker_tile_accounts(1053, "retained accounts")
#'   )
#' }
#' @export
op_list_pbac_adoption <- function(accounts,
                                  low_consumption = NULL,
                                  sdd_consumption = NULL,
                                  policy_metrics  = NULL,
                                  domain_accounts = NULL,
                                  sdd_days        = 90,
                                  production_only = TRUE,
                                  weights         = .PBAC_COMPLEXITY_WEIGHTS,
                                  ratio_minimal   = 0.25,
                                  ratio_moderate  = 0.75,
                                  complexity_min  = 4) {

  if (is.null(accounts) || !("Account Name" %in% names(accounts))) {
    stop("`accounts` must be op_list$accounts, with an `Account Name` column.")
  }

  name_raw <- as.character(accounts$`Account Name`)
  name_vec <- trimws(name_raw)

  # ── Account sets from each look ────────────────────────────────────────────
  pol <- .pbac_production(policy_metrics, production_only)

  dns_accounts <- if (!is.null(pol) &&
                      all(c("[Usage Metrics] Active Global Policies: Data",
                            "Active Global Policies: Subscription") %in% names(pol))) {
    .pbac_accounts_where(
      pol,
      .pbac_num(pol$`[Usage Metrics] Active Global Policies: Data`) > 0 &
        .pbac_num(pol$`Active Global Policies: Subscription`) > 0
    )
  } else character(0)

  guardrail_accounts <- if (!is.null(pol) &&
                            "[Usage Metrics] Active Global Policies: Guardrail" %in% names(pol)) {
    .pbac_accounts_where(
      pol,
      .pbac_num(pol$`[Usage Metrics] Active Global Policies: Guardrail`) > 0
    )
  } else character(0)

  sdd_accounts <- if (!is.null(sdd_consumption) &&
                      "Days since last SDD run" %in% names(sdd_consumption)) {
    .pbac_accounts_where(
      sdd_consumption,
      .pbac_num(sdd_consumption$`Days since last SDD run`) <= sdd_days
    )
  } else character(0)

  domain_accounts <- trimws(as.character(domain_accounts %||% character(0)))

  # ── Components ─────────────────────────────────────────────────────────────
  multi <- if ("multiplatform" %in% names(accounts)) {
    as.numeric(!is.na(accounts$multiplatform) & accounts$multiplatform)
  } else {
    rep(0, length(name_vec))
  }

  components <- data.frame(
    multi_platform = multi,
    dns_policies   = as.numeric(name_vec %in% dns_accounts),
    sdd            = as.numeric(name_vec %in% sdd_accounts),
    domains        = as.numeric(name_vec %in% domain_accounts),
    guardrail      = as.numeric(name_vec %in% guardrail_accounts),
    stringsAsFactors = FALSE
  )

  w <- weights[names(components)]
  w[is.na(w)] <- 1

  out <- data.frame(`Account Name` = name_raw, components,
                    check.names = FALSE, stringsAsFactors = FALSE)

  # Weighted sum of the components only — computed before consumption_ratio is
  # attached, so the ratio can never leak into the score.
  out$complexity_score   <- as.numeric(as.matrix(components) %*% as.numeric(w))
  out$consumption_ratio  <- .pbac_consumption_ratio(low_consumption, name_vec)

  out$pbac_adoption <- .pbac_rating(out$complexity_score,
                                    out$consumption_ratio,
                                    ratio_minimal  = ratio_minimal,
                                    ratio_moderate = ratio_moderate,
                                    complexity_min = complexity_min)

  out
}

# Registered / licensed users in Production, matched to `name_vec`. Reads the
# `Consumption Ratio` column op_list_consumption() derives, falling back to
# recomputing it from the two counts — the same fallback generate_slide_tile()
# uses, so the number here and the number on the health summary agree.
.pbac_consumption_ratio <- function(low_consumption, name_vec) {
  if (is.null(low_consumption) || nrow(low_consumption) == 0 ||
      !("Account Name" %in% names(low_consumption))) {
    return(rep(NA_real_, length(name_vec)))
  }

  idx <- match(name_vec, trimws(as.character(low_consumption$`Account Name`)))

  pick <- function(col) {
    if (!(col %in% names(low_consumption))) return(rep(NA_real_, length(idx)))
    .pbac_num(low_consumption[[col]])[idx]
  }

  ratio <- pick("Consumption Ratio")
  reg   <- pick("Registered Users")
  lic   <- pick("Licensed Users")

  # consumption_ratio() carries the ELA rule (Licensed Users of 0 means
  # unlimited seats, so the ratio is 1, not NA) — recomputing it inline here
  # would quietly disagree with the column op_list_consumption() derived.
  need <- is.na(ratio)
  ratio[need] <- consumption_ratio(reg[need], lic[need])
  ratio
}
