# CSM coverage: book-of-business rollup per Customer Success Manager.
#
# Stored as op_list$csm_coverage by enrich_opportunities(). Built from
# op_list$accounts (CSM assignment, ARR, Lifecycle Stage) plus
# op_list$consumption$pbac_adoption (complexity_score), so it has to run after
# op_list_consumption().

# TRUE where a Lifecycle Stage is stage 1 — "S1: Installation and Config." in
# Salesforce, i.e. onboarding. Same match as .pbac_install_stage() in
# R/report_pbac_scatter.R (stage number or the words), so a relabel of the
# picklist doesn't silently zero the count.
.csm_is_stage1 <- function(stage) {
  stage <- as.character(stage)
  !is.na(stage) &
    grepl("^\\s*S1\\b|install|onboard", stage, ignore.case = TRUE, perl = TRUE)
}

# Leading colour word of a "<colour>: <label>" picklist value, lowercased, NA
# when there isn't one. Vectorised counterpart of .tile_stoplight_key() in
# R/generate_slide_tile.R; "yellow" is read as amber, as the renderers do.
.csm_stoplight <- function(x) {
  x   <- trimws(as.character(x))
  hit <- !is.na(x) & grepl("^(green|amber|yellow|red|black)\\b", x,
                           ignore.case = TRUE, perl = TRUE)
  m <- rep(NA_character_, length(x))
  m[hit] <- tolower(sub("^(\\w+).*$", "\\1", x[hit], perl = TRUE))
  m[!is.na(m) & m == "yellow"] <- "amber"
  m
}

#' CSM coverage rollup
#'
#' Builds \code{op_list$csm_coverage}: one row per Customer Success Manager
#' summarizing the accounts assigned to them.
#'
#' \describe{
#'   \item{\code{csm}}{\code{Customer Success Manager} from
#'     \code{op_list$accounts}. Blank / \code{NA} assignments roll up under
#'     \code{"Unassigned"} rather than being dropped.}
#'   \item{\code{accounts}}{Number of assigned accounts.}
#'   \item{\code{arr}}{Sum of \code{Contract ARR Rollup}.}
#'   \item{\code{pbac_complexity}}{Sum of \code{complexity_score} from
#'     \code{op_list$consumption$pbac_adoption} (see
#'     \code{\link{op_list_pbac_adoption}}). \code{NA} when that table is
#'     absent; on an SFDC-only run (\code{looker = FALSE}) only the
#'     multi-platform component can score, so treat it as incomplete.}
#'   \item{\code{s1_onboarding}}{Accounts whose \code{Lifecycle Stage} is
#'     stage 1 (\code{"S1: Installation and Config."}).}
#'   \item{\code{request_retained}}{Accounts at the retained Request stage.}
#'   \item{\code{active_request}}{Accounts at the adopted \emph{or} retained
#'     Request stage. Both Request columns use the Looker-derived
#'     \code{request_adoption_looker_stage} where present, otherwise the
#'     Salesforce \code{Request Adoption} picklist (green = retained,
#'     amber = adopted) — same precedence as the account cards.}
#'   \item{\code{ongoing_agentic}}{Ongoing Agentic Adoption: accounts whose
#'     \code{Agentic Governance Adoption} picklist is amber or green.}
#' }
#'
#' Accounts are de-duplicated on \code{Account Name} first so a repeated row in
#' the Salesforce report can't double-count ARR.
#'
#' @param accounts Data frame. \code{op_list$accounts}; needs
#'   \code{Account Name}, \code{Customer Success Manager}, and uses
#'   \code{Contract ARR Rollup} and \code{Lifecycle Stage} when present.
#' @param pbac_adoption Data frame or \code{NULL}.
#'   \code{op_list$consumption$pbac_adoption}.
#'
#' @return A data frame with columns \code{csm}, \code{accounts}, \code{arr},
#'   \code{pbac_complexity}, \code{s1_onboarding}, \code{request_retained},
#'   \code{active_request}, \code{ongoing_agentic}, sorted by \code{arr}
#'   descending.
#' @examples
#' \dontrun{
#'   op_list$csm_coverage
#'
#'   op_list_csm_coverage(op_list$accounts, op_list$consumption$pbac_adoption)
#' }
#' @export
op_list_csm_coverage <- function(accounts, pbac_adoption = NULL) {

  need <- c("Account Name", "Customer Success Manager")
  if (is.null(accounts) || !all(need %in% names(accounts))) {
    stop("`accounts` must be op_list$accounts, with `Account Name` and ",
         "`Customer Success Manager` columns.")
  }

  accts <- accounts[!duplicated(trimws(as.character(accounts$`Account Name`))), ,
                    drop = FALSE]
  name_vec <- trimws(as.character(accts$`Account Name`))

  csm <- trimws(as.character(accts$`Customer Success Manager`))
  csm[is.na(csm) | csm == "" | csm == "-"] <- "Unassigned"

  arr <- if ("Contract ARR Rollup" %in% names(accts)) {
    suppressWarnings(as.numeric(accts$`Contract ARR Rollup`))
  } else {
    rep(NA_real_, length(name_vec))
  }

  have_pbac <- !is.null(pbac_adoption) && nrow(pbac_adoption) > 0 &&
    all(c("Account Name", "complexity_score") %in% names(pbac_adoption))
  complexity <- if (have_pbac) {
    pbac_adoption$complexity_score[
      match(name_vec, trimws(as.character(pbac_adoption$`Account Name`)))]
  } else {
    rep(NA_real_, length(name_vec))
  }

  stage1 <- if ("Lifecycle Stage" %in% names(accts)) {
    .csm_is_stage1(accts$`Lifecycle Stage`)
  } else {
    rep(FALSE, length(name_vec))
  }

  req_stage <- .request_stage(accts)   # R/op_list_product_adoption.R

  agentic <- if ("Agentic Governance Adoption" %in% names(accts)) {
    .csm_stoplight(accts$`Agentic Governance Adoption`)
  } else {
    rep(NA_character_, length(name_vec))
  }

  df <- data.frame(csm = csm, arr = arr, complexity = complexity,
                   stage1 = stage1,
                   req_retained = !is.na(req_stage) & req_stage == "retained",
                   req_active   = !is.na(req_stage) & req_stage %in% c("adopted", "retained"),
                   agentic_ag   = !is.na(agentic) & agentic %in% c("amber", "green"),
                   stringsAsFactors = FALSE)

  out <- df |>
    dplyr::group_by(csm) |>
    dplyr::summarise(
      accounts        = dplyr::n(),
      arr             = sum(arr, na.rm = TRUE),
      # NA (not 0) when there's no PBAC table at all — 0 would read as
      # "every account is trivial", which isn't what an SFDC-only run knows.
      pbac_complexity = if (have_pbac) sum(complexity, na.rm = TRUE) else NA_real_,
      s1_onboarding   = sum(stage1),
      request_retained    = sum(req_retained),
      active_request      = sum(req_active),
      ongoing_agentic     = sum(agentic_ag),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(arr))

  as.data.frame(out)
}
