#' Build the \code{op_list$product_board} child object
#'
#' Pulls the Productboard note corpus with \code{\link{pb_pull_insights}}
#' (notes with their inline relationships, plus companies, users and
#' features), matches each note's company to an Immuta account with
#' \code{\link{pb_match_accounts}} (Salesforce Account ID where Productboard
#' has it, otherwise a case/punctuation/suffix-insensitive name match — see
#' \code{.pb_normalize_name()}), and summarises by account and by feature.
#'
#' @param accounts Data frame of Immuta accounts (typically
#'   \code{op_list$accounts}). Must contain \code{Account Name} and
#'   \code{Account ID}.
#' @param pb Optional list from \code{pb_pull_insights()} /
#'   \code{pb_load_insights()}, to reuse a pull instead of calling the API.
#' @param customers_only Logical. Default \code{TRUE}: keep only notes tied
#'   to an account in \code{accounts} (existing customers), dropping
#'   prospects, partners, unmatched companies and notes with no customer.
#'   \code{FALSE} keeps the whole corpus.
#' @return A named list:
#'   \describe{
#'     \item{notes}{Tibble of notes (see \code{\link{pb_pull_insights}}) with
#'       \code{account_name} / \code{account_id}; customers only by default.}
#'     \item{features}{Tibble of features and subfeatures.}
#'     \item{note_links}{Tibble: one row per note, with \code{note_id},
#'       \code{company_name} (as resolved from Productboard), \code{account_name}
#'       (matched Immuta account, or \code{NA} if unmatched), and
#'       \code{feature_ids} (list-column of linked feature/subfeature ids).}
#'     \item{by_account}{Tibble: one row per Immuta account that has any
#'       linked Productboard activity, with \code{account_name},
#'       \code{note_count}, \code{feature_request_count}, and list-columns
#'       \code{feature_names} / \code{note_names} for display.}
#'     \item{feature_requests}{Tibble: one row per feature that has at least
#'       one linked note, with \code{feature_id}, \code{feature_name},
#'       \code{status}, \code{request_count}, and an \code{accounts}
#'       list-column of requesting accounts (unmatched companies are listed
#'       under their raw Productboard name).}
#'     \item{unmatched_companies}{Tibble (\code{company_name}, \code{notes}) of
#'       Productboard companies on notes that didn't match any account —
#'       reported even when \code{customers_only = TRUE}, since a name
#'       mismatch here can hide a real customer's notes.}
#'   }
#' @export
op_list_product_board <- function(accounts, pb = NULL, customers_only = TRUE) {

  if (is.null(pb)) pb <- pb_pull_insights()
  pb <- pb_match_accounts(pb, accounts, customers_only = customers_only)

  notes    <- pb$notes
  features <- pb$features

  if (is.null(notes) || nrow(notes) == 0) {
    message(".. no Productboard notes", if (customers_only) " tied to existing customers", "")
    return(list(notes = tibble::tibble(), features = features,
                note_links = tibble::tibble(), by_account = tibble::tibble(),
                feature_requests = tibble::tibble(), unmatched_companies = pb$unmatched_companies))
  }

  feat_links <- pb$links[pb$links$target_type %in% c("feature", "subfeature"), , drop = FALSE]
  note_links <- tibble::tibble(
    note_id      = notes$note_id,
    company_name = notes$company_name,
    account_name = notes$account_name,
    feature_ids  = lapply(notes$note_id, function(id) feat_links$target_id[feat_links$note_id == id])
  )

  unmatched <- pb$unmatched_companies

  list(
    notes               = notes,
    features            = features,
    note_links          = note_links,
    by_account          = .pb_summarize_by_account(note_links, notes, features),
    feature_requests    = .pb_summarize_feature_requests(note_links, notes, features),
    unmatched_companies = unmatched
  )
}

#' @noRd
.pb_normalize_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[.,]", "", x)
  x <- gsub("\\b(inc|incorporated|llc|ltd|limited|co|corp|corporation|company|group|plc)\\b", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' @noRd
.pb_note_display_name <- function(notes, id) {
  nm <- notes$title[match(id, notes$note_id)]
  if (is.null(nm) || length(nm) == 0 || is.na(nm)) "(untitled note)" else nm
}

#' @noRd
.pb_feature_display_name <- function(features, id) {
  if (is.null(features) || !nrow(features)) return("(unnamed feature)")
  nm <- features$name[match(id, features$id)]
  if (is.null(nm) || length(nm) == 0 || is.na(nm)) "(unnamed feature)" else nm
}

#' @noRd
.pb_summarize_by_account <- function(note_links, notes, features) {
  matched <- note_links[!is.na(note_links$account_name), , drop = FALSE]
  if (nrow(matched) == 0) return(tibble::tibble())

  accounts_u <- sort(unique(matched$account_name))

  rows <- lapply(accounts_u, function(acct) {
    sub <- matched[matched$account_name == acct, , drop = FALSE]
    note_ids <- sub$note_id
    feat_ids <- unique(unlist(sub$feature_ids))

    tibble::tibble(
      account_name           = acct,
      note_count             = length(note_ids),
      feature_request_count  = length(feat_ids),
      note_names             = list(vapply(note_ids, function(id) .pb_note_display_name(notes, id), character(1))),
      feature_names          = list(vapply(feat_ids, function(id) .pb_feature_display_name(features, id), character(1)))
    )
  })

  dplyr::bind_rows(rows)
}

#' @noRd
.pb_summarize_feature_requests <- function(note_links, notes, features) {
  exploded <- note_links[lengths(note_links$feature_ids) > 0, , drop = FALSE]
  if (nrow(exploded) == 0) return(tibble::tibble())

  # One row per (note, feature) pair
  pairs <- do.call(rbind, lapply(seq_len(nrow(exploded)), function(i) {
    fids <- exploded$feature_ids[[i]]
    data.frame(
      feature_id   = fids,
      note_id      = exploded$note_id[i],
      company_name = exploded$company_name[i],
      account_name = exploded$account_name[i],
      stringsAsFactors = FALSE
    )
  }))

  feature_ids_u <- unique(pairs$feature_id)

  rows <- lapply(feature_ids_u, function(fid) {
    sub <- pairs[pairs$feature_id == fid, , drop = FALSE]
    # Prefer the matched Immuta account name; fall back to the raw
    # Productboard company name so unmatched requesters aren't silently lost.
    requesters <- ifelse(is.na(sub$account_name), sub$company_name, sub$account_name)
    requesters <- unique(requesters[!is.na(requesters)])

    status <- if (!is.null(features) && nrow(features)) features$status[match(fid, features$id)] else NA_character_

    tibble::tibble(
      feature_id    = fid,
      feature_name  = .pb_feature_display_name(features, fid),
      status        = status,
      request_count = length(unique(sub$note_id)),
      accounts      = list(requesters)
    )
  })

  result <- dplyr::bind_rows(rows)
  result[order(-result$request_count), , drop = FALSE]
}
