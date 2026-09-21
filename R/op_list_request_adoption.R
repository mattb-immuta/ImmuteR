# Derive each account's Request adoption tier from the Looker Request metrics
# dashboard (1009) instead of reading the hand-maintained Salesforce picklist.
#
# The funnel is not a status column: dashboard 1009 expresses it as one tile
# per stage, each listing the accounts that qualify plus their feature usage
# counts. The tier is therefore membership — an account is assigned the
# strongest stage whose tile it appears on:
#
#   tried     -> Red     POC complete, active use case planning/testing/deployment
#   adopted   -> Amber   use case in production, driving toward retained status
#   retained  -> Green   'retained' for 3 of the last 4 weeks
#   (none)    -> Black   no active customer data request workflow development
#
# The "3 of the last 4 weeks" test behind Green is Looker's to make, not ours —
# it is baked into the retained tile's own definition (and its `Show row` gate),
# so this code deliberately does no windowing of its own. The stars definitions
# live in the dashboard; changing them changes these tiers.

# Stage -> tile title pattern, weakest to strongest. Matched case-insensitively
# against each tile's title, and assigned in this order so the strongest stage
# an account qualifies for wins.
.RA_TILE_PATTERNS <- c(
  tried    = "tried accounts",
  adopted  = "adopted",
  retained = "retained accounts"
)

# Exact Salesforce picklist strings, so a derived value is string-identical to
# the SFDC one and the existing renderers (fmtStoplight / adoptionHtml in
# report_all_accounts_html.R, which parse the leading colour word and show the
# rest as the definition) need no changes. The curly quotes the real picklist
# uses are written as \u escapes, matching utils_internal.R's convention.
.REQUEST_ADOPTION_TIERS <- c(
  retained = "Green: Customer ‘retained’ for 3 of the last 4 weeks.",
  adopted  = paste0("Amber: Use case in production, enabling data ",
                    "determinations, driving adoption towards ",
                    "“retained status”."),
  tried    = "Red: POC complete and active use case planning, testing, and deployment.",
  none     = "Black: No active customer data request workflow development"
)

# Leading colour word of a picklist value ("Green: ..." -> "green"), used to
# compare a derived tier against the SFDC one without comparing full sentences.
# Yellow folds into amber: the picklists use both spellings for the same state
# (see fmtStoplight in report_all_accounts_html.R), and the derived tiers only
# ever emit Amber, so keeping them distinct would report a mismatch on accounts
# where the two sources actually agree.
.ra_colour <- function(x) {
  v   <- tolower(trimws(as.character(x)))
  hit <- !is.na(v) & grepl("^(green|amber|yellow|red|black)", v)
  out <- rep(NA_character_, length(v))
  out[hit] <- sub("^(green|amber|yellow|red|black).*$", "\\1", v[hit])
  ifelse(!is.na(out) & out == "yellow", "amber", out)
}

# Account names on a stage tile — looker_tile_accounts() does the work; this
# only fixes the argument order used by the loop below.
#
# `show_row` gates the rows on the tile's `Show row` table calculation, and is
# applied only to the stages that ask for it (retained, per the tile as built)
# rather than wherever the column happens to exist: on the tried and adopted
# tiles the column is not the membership test, and filtering on it there drops
# accounts that do qualify.
.ra_stage_accounts <- function(dash, pattern, token, base_url,
                               show_row = FALSE) {
  looker_tile_accounts(dashboard_id = NULL, pattern = pattern,
                       show_row = show_row, token = token,
                       dash = dash, base_url = base_url)
}

#' Derive Request adoption tiers from the Looker Request dashboard
#'
#' Reads the stage tiles of the Request metrics dashboard (Looker dashboard
#' 1009) and assigns each account in \code{accounts} the strongest stage whose
#' tile lists it. Attached by \code{\link{enrich_opportunities}} as
#' \code{op_list$accounts$request_adoption_looker}:
#'
#' \describe{
#'   \item{\strong{Green}}{on the retained tile.}
#'   \item{\strong{Amber}}{on the adopted tile.}
#'   \item{\strong{Red}}{on the tried tile.}
#'   \item{\strong{Black}}{on none of them.}
#' }
#'
#' The stage definitions — including the "3 of the last 4 weeks" test behind
#' retained — belong to the dashboard, so this function does no thresholding of
#' its own. The retained tile's \code{Show row} table calculation is what
#' decides which of its rows actually count, so that tile is filtered to
#' \code{"Yes"}; the other stages are read as listed (see
#' \code{show_row_stages}).
#'
#' Accounts are matched to Looker by exact name, the same way
#' \code{\link{op_list_consumption}} joins its looks.
#'
#' @param accounts Data frame. \code{op_list$accounts}; needs
#'   \code{Account Name}, and uses \code{Request Adoption} when present to
#'   report agreement with the Salesforce value.
#' @param dashboard_id Integer. Default \code{1009}.
#' @param token Character. Bearer token from \code{\link{looker_auth}}.
#'   Defaults to calling \code{looker_auth()}.
#' @param dash Named list from \code{\link{looker_dashboard}}, or \code{NULL}
#'   to fetch it. Pass one you already have to save a call.
#' @param stages Named list of character vectors (\code{tried}, \code{adopted},
#'   \code{retained}) to use in place of pulling the tiles — for testing, or
#'   for reusing account lists already in hand. Any missing name is pulled.
#' @param tile_patterns Named character vector mapping stage to a lowercase
#'   substring of the tile's title, weakest stage first. Defaults to
#'   \code{.RA_TILE_PATTERNS}; override if the tiles get renamed.
#' @param show_row_stages Character vector of stages whose tile should be
#'   filtered to \code{`Show row` == "Yes"}. Default \code{"retained"} — the
#'   only tile where that calculation is the membership test.
#' @param base_url Character. Looker instance base URL.
#'
#' @return A tibble, one row per account in \code{accounts}, with the column
#'   names it lands on \code{op_list$accounts} under:
#' \describe{
#'   \item{\code{Account Name}}{Account name, as supplied.}
#'   \item{\code{request_adoption_looker_stage}}{\code{"tried"},
#'     \code{"adopted"}, \code{"retained"} or \code{"none"} — the raw stage,
#'     for filtering and grouping.}
#'   \item{\code{request_adoption_looker}}{The tier as a full picklist-format
#'     string, directly comparable to the Salesforce \code{Request Adoption}
#'     value.}
#'   \item{\code{request_adoption_match}}{\code{TRUE}/\code{FALSE} against the
#'     SFDC colour, \code{NA} when SFDC is blank or the column is absent.}
#' }
#' @export
op_list_request_adoption <- function(accounts,
                                     dashboard_id  = 1009,
                                     token         = NULL,
                                     dash          = NULL,
                                     stages        = NULL,
                                     tile_patterns = .RA_TILE_PATTERNS,
                                     show_row_stages = "retained",
                                     base_url      = Sys.getenv("LOOKER_BASE_URL",
                                                                "https://immuta.cloud.looker.com")) {
  if (is.null(accounts) || !("Account Name" %in% names(accounts))) {
    stop("`accounts` must be op_list$accounts, with an `Account Name` column.")
  }

  # Match on trimmed names (Looker's CSV export carries trailing spaces often
  # enough to cost a few accounts), but return the name exactly as supplied so
  # the result still joins back onto op_list$accounts on `Account Name`.
  name_raw <- as.character(accounts$`Account Name`)
  name_vec <- trimws(name_raw)
  need     <- setdiff(names(tile_patterns), names(stages))

  if (length(need) > 0) {
    if (is.null(token)) token <- looker_auth()
    if (is.null(dash))  dash  <- looker_dashboard(dashboard_id, token, base_url)

    for (stage in need) {
      stages[[stage]] <- .ra_stage_accounts(dash, tile_patterns[[stage]],
                                            token, base_url,
                                            show_row = stage %in% show_row_stages)
      message(".. request adoption: ", stage, " = ",
              length(stages[[stage]]), " accounts on the tile")
    }
  }

  # Weakest stage first, so a stronger one overwrites it. tile_patterns is
  # ordered, which is what makes this work.
  request_status <- rep("none", length(name_vec))
  for (stage in names(tile_patterns)) {
    request_status[name_vec %in% stages[[stage]]] <- stage
  }

  out <- tibble::tibble(
    `Account Name`                = name_raw,
    request_adoption_looker_stage = request_status,
    request_adoption_looker       = unname(.REQUEST_ADOPTION_TIERS[request_status])
  )

  # Agreement with the hand-maintained Salesforce value, on colour only.
  if ("Request Adoption" %in% names(accounts)) {
    sfdc <- .ra_colour(as.character(accounts$`Request Adoption`))
    drv  <- .ra_colour(out$request_adoption_looker)
    out$request_adoption_match <- ifelse(is.na(sfdc), NA, sfdc == drv)
  } else {
    out$request_adoption_match <- NA
  }

  out
}
