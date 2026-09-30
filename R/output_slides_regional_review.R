#' Generate regional success-meeting slides in a Google Slides deck.
#'
#' Writes one slide per account in a region into an existing presentation: an
#' account header (ARR, AE / CSM / FCTO, next renewal) and three columns —
#' Adoption, Performance, Engagement — carrying the same metadata as
#' \code{\link{generate_slide_tile}}, each with an empty text box underneath for
#' the CSM's commentary.
#'
#' @section Reruns never clobber CSM comments:
#' Every shape gets a deterministic object ID derived from the account name (see
#' \code{.slide_uid}). On each run the generator reads the presentation, and
#' then:
#' \itemize{
#'   \item accounts with no slide yet get one created, comment boxes empty;
#'   \item accounts that already have a slide get \emph{only} their header and
#'     metric shapes rewritten;
#'   \item the three comment shapes per slide are never written to after
#'     creation.
#' }
#' So a CSM can type into a comment box, you can re-run the pipeline the
#' following week against fresh \code{op_list} data, and their text survives
#' while every number around it updates. Nothing is ever deleted — an account
#' that leaves the region keeps its slide until you remove it by hand.
#'
#' Because comments live in the deck itself, the deck is the system of record
#' for commentary. Don't delete and recreate the presentation between meetings;
#' point at the same \code{presentation_id} each time.
#'
#' @param op_list Named list returned by \code{\link{enrich_opportunities}}.
#' @param presentation_id Character. The Google Slides presentation ID — the
#'   long string in the deck URL between \code{/d/} and \code{/edit}.
#' @param region Character or \code{NULL}. Region to generate slides for,
#'   matched case-insensitively against \code{op_list$accounts$Region}.
#'   \code{NULL} does every account.
#' @param token Character or \code{NULL}. Bearer token from
#'   \code{\link{slides_auth}}. Fetched automatically when \code{NULL}.
#' @param dry_run Logical. When \code{TRUE}, builds the request payload and
#'   reports what would change without calling the API. Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly, a data frame with one row per account and columns
#'   \code{account}, \code{slide_id}, and \code{action} (\code{"created"} or
#'   \code{"updated"}).
#'
#' @examples
#' \dontrun{
#'   # See what would happen first
#'   output_slides_regional_review(
#'     op_list,
#'     presentation_id = "1JG3_LbBqQp-3DwtlWG79dz-LhYaXUTf0Tvn51phKcB8",
#'     region  = "West",
#'     dry_run = TRUE
#'   )
#'
#'   output_slides_regional_review(
#'     op_list,
#'     presentation_id = "1JG3_LbBqQp-3DwtlWG79dz-LhYaXUTf0Tvn51phKcB8",
#'     region = "West"
#'   )
#' }
#' @export
output_slides_regional_review <- function(op_list,
                                          presentation_id,
                                          region  = NULL,
                                          token   = NULL,
                                          dry_run = FALSE) {

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required.", call. = FALSE)
  }

  payloads <- region_slide_payloads(op_list, region)
  if (length(payloads) == 0) {
    message(".. no accounts to render.")
    return(invisible(NULL))
  }

  if (is.null(token)) token <- slides_auth()

  pres  <- .slides_get(presentation_id, token)
  index <- .slides_index(pres)

  # Slide geometry, points. A 16:9 Google Slide is 720 x 405 pt.
  PW <- 720; M <- 30
  COLW <- 213; GUT <- 10
  COLX <- M + (0:2) * (COLW + GUT)

  requests <- list()
  log <- data.frame(account = character(0), slide_id = character(0),
                    action = character(0), stringsAsFactors = FALSE)

  for (pl in payloads) {
    uid <- .slide_uid(pl$account$name)
    ids <- .slide_ids(uid)
    is_new <- !(ids$slide %in% index$ids)

    if (is_new) {
      requests <- c(requests, list(list(
        createSlide = list(
          objectId = ids$slide,
          slideLayoutReference = list(predefinedLayout = "BLANK")
        )
      )))

      # Header
      requests <- c(requests,
        .slides_new_box(ids$title, ids$slide, M, 20, 430, 28,
                        size = 18, bold = TRUE),
        .slides_new_box(ids$arr, ids$slide, PW - M - 200, 22, 200, 24,
                        size = 14, bold = TRUE, align = "END"),
        .slides_new_box(ids$meta, ids$slide, M, 52, PW - 2 * M, 16,
                        size = 8, colour = c(.4, .4, .4)))

      # Three columns: header, metrics, comment box
      for (i in 1:3) {
        requests <- c(requests,
          .slides_new_box(ids$header[i], ids$slide, COLX[i], 78, COLW, 20,
                          size = 12, bold = TRUE),
          .slides_new_box(ids$metrics[i], ids$slide, COLX[i], 100, COLW, 108,
                          size = 8.5),
          .slides_new_box(ids$comment[i], ids$slide, COLX[i], 214, COLW, 168,
                          size = 9))
      }
    }

    # Header + metric text is rewritten every run. Comment shapes are absent
    # from this list by design — that is the whole mechanism.
    txt <- list(
      c(ids$title,      pl$account$name),
      c(ids$arr,        pl$account$arr),
      c(ids$meta,       .slide_meta_line(pl)),
      c(ids$header[1],  "Adoption"),
      c(ids$header[2],  "Performance"),
      c(ids$header[3],  "Engagement"),
      c(ids$metrics[1], .slide_block_adoption(pl)),
      c(ids$metrics[2], .slide_block_performance(pl)),
      c(ids$metrics[3], .slide_block_engagement(pl))
    )

    for (t in txt) {
      requests <- c(requests, .slides_set_text(t[1], t[2], index))
    }

    log <- rbind(log, data.frame(
      account  = pl$account$name,
      slide_id = ids$slide,
      action   = if (is_new) "created" else "updated",
      stringsAsFactors = FALSE
    ))
  }

  n_new <- sum(log$action == "created")
  n_upd <- sum(log$action == "updated")

  if (dry_run) {
    message(".. dry run: ", nrow(log), " account(s) — ",
            n_new, " slide(s) would be created, ", n_upd, " updated; ",
            length(requests), " API request(s). No changes sent.")
    return(invisible(log))
  }

  .slides_batch_update(presentation_id, requests, token)

  message(".. regional review slides written to ", presentation_id,
          if (!is.null(region)) paste0(" [", region, "]") else "",
          ": ", n_new, " created, ", n_upd, " updated ",
          "(comment boxes untouched)")

  invisible(log)
}


# ── Internal text builders ────────────────────────────────────────────────────

.slide_meta_line <- function(pl) {
  a <- pl$account
  paste(paste0("AE: ",   a$ae),
        paste0("CSM: ",  a$csm),
        paste0("FCTO: ", a$fcto),
        paste0("Next Renewal: ", a$next_renewal),
        sep = "   |   ")
}

.slide_block_adoption <- function(pl) {
  ad <- pl$adoption
  mods <- vapply(ad$modules,
                 function(m) paste0(m$label, ": ", m$status),
                 character(1))

  # Platforms and features are the evidence behind the PBAC rating, so they
  # sit directly under the module line — the same order the HTML card and the
  # PNG health summary use.
  paste(
    paste(mods, collapse = "   "),
    paste0("Platforms: ", ad$platforms, "    Features: ", ad$features_text),
    "",
    paste0("Registered: ", ad$registered,
           "    Licensed: ", ad$licensed,
           "    Ratio: ", ad$ratio),
    paste0("Lifecycle: ", ad$lifecycle_stage),
    sep = "\n"
  )
}

.slide_block_performance <- function(pl) {
  pf <- pl$performance

  paste(
    paste0("Incidents 90d: ", pf$cases_90d, "    360d: ", pf$cases_360d),
    paste0("CustX 90d: ", pf$escalations_90d,
           "    Open: ", pf$escalations_open),
    "",
    paste0(pf$deployment, "    Version: ", pf$version),
    sep = "\n"
  )
}

.slide_block_engagement <- function(pl) {
  en <- pl$engagement
  contacts <- if (is.null(en$contacts)) list() else en$contacts
  contact_lines <- vapply(contacts, function(r) {
    paste0(r$label, ": ", r$name,
           "  (Rel: ", r$relationship, ", Inf: ", r$influence, ")")
  }, character(1))
  paste(c(
    contact_lines,
    if (length(contact_lines)) "",
    paste0("Last QBR: ", en$last_qbr),
    paste0("Next QBR: ", en$next_qbr),
    "",
    paste0("Note: ", en$qbr_note)
  ), collapse = "\n")
}
