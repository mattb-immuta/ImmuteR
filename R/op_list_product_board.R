#' Build the \code{op_list$product_board} child object
#'
#' Pulls Productboard notes (customer feedback) and features (roadmap), then
#' resolves each note's customer relationship down to a company and
#' fuzzy-matches that company name against the Immuta account table (name
#' matching is case/punctuation/suffix-insensitive — see
#' \code{.pb_normalize_name()} — since Productboard's company names and
#' Salesforce's Account Names aren't guaranteed to be byte-identical).
#'
#' This is the slow step of the pipeline: Productboard's API only exposes
#' note relationships one note at a time (see \code{pb_get_note_relationships()}),
#' so this makes roughly \code{nrow(notes)} additional API calls. At the
#' default 50 req/s rate limit that's a few thousand notes per minute.
#'
#' @param accounts Data frame of Immuta accounts (typically
#'   \code{op_list$accounts}). Must contain an \code{Account Name} column.
#' @return A named list:
#'   \describe{
#'     \item{notes}{Tibble of all notes pulled from Productboard.}
#'     \item{features}{Tibble of all features pulled from Productboard.}
#'     \item{note_links}{Tibble: one row per note, with \code{note_id},
#'       \code{company_name} (as resolved from Productboard), \code{account_name}
#'       (matched Immuta account, or \code{NA} if unmatched), and
#'       \code{feature_ids} (list-column of linked feature UUIDs).}
#'     \item{by_account}{Tibble: one row per Immuta account that has any
#'       linked Productboard activity, with \code{account_name},
#'       \code{note_count}, \code{feature_request_count}, and list-columns
#'       \code{feature_names} / \code{note_names} for display.}
#'     \item{feature_requests}{Tibble: one row per feature that has at least
#'       one linked note, with \code{feature_id}, \code{feature_name},
#'       \code{status} (if available), \code{request_count}, and a
#'       \code{accounts} list-column of requesting Immuta account names
#'       (unmatched companies are listed under their raw Productboard name).}
#'     \item{unmatched_companies}{Character vector of resolved company names
#'       that didn't match any Immuta account — useful for spotting naming
#'       drift between Productboard and Salesforce.}
#'   }
#' @export
op_list_product_board <- function(accounts) {

  empty_result <- list(
    notes = tibble::tibble(), features = tibble::tibble(),
    note_links = tibble::tibble(), by_account = tibble::tibble(),
    feature_requests = tibble::tibble(), unmatched_companies = character(0)
  )

  message(".. pulling Productboard notes")
  notes <- pb_get_notes()

  message(".. pulling Productboard features")
  features <- pb_get_features()

  if (nrow(notes) == 0) {
    message(".. no Productboard notes found; skipping relationship resolution")
    return(utils::modifyList(empty_result, list(notes = notes, features = features)))
  }

  message(".. resolving relationships for ", nrow(notes), " notes (one API call each)")
  note_links <- dplyr::bind_rows(lapply(notes$id, .pb_resolve_note_links))

  acct_names <- accounts$`Account Name`
  norm_acct  <- .pb_normalize_name(acct_names)

  note_links$account_name <- vapply(note_links$company_name, function(nm) {
    if (is.na(nm) || !nzchar(nm)) return(NA_character_)
    idx <- match(.pb_normalize_name(nm), norm_acct)
    if (is.na(idx)) NA_character_ else acct_names[idx]
  }, character(1))

  unmatched <- unique(note_links$company_name[
    !is.na(note_links$company_name) & is.na(note_links$account_name)
  ])

  by_account       <- .pb_summarize_by_account(note_links, notes, features)
  feature_requests <- .pb_summarize_feature_requests(note_links, notes, features)

  list(
    notes               = notes,
    features            = features,
    note_links          = note_links,
    by_account          = by_account,
    feature_requests    = feature_requests,
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
.pb_resolve_note_links <- function(note_id) {
  rels <- tryCatch(pb_get_note_relationships(note_id), error = function(e) tibble::tibble())

  company_name <- NA_character_
  feature_ids  <- character(0)

  if (nrow(rels) > 0 && "type" %in% names(rels)) {
    cust <- rels[rels$type == "customer", , drop = FALSE]
    if (nrow(cust) > 0) {
      target_type <- cust[["target.type"]][1]
      target_id   <- cust[["target.id"]][1]
      company_name <- if (identical(target_type, "company")) {
        .pb_company_name(target_id)
      } else if (identical(target_type, "user")) {
        .pb_user_company_name(target_id)
      } else {
        NA_character_
      }
    }

    links <- rels[rels$type == "link" & rels[["target.type"]] %in% c("feature", "subfeature"), , drop = FALSE]
    if (nrow(links) > 0) feature_ids <- links[["target.id"]]
  }

  tibble::tibble(
    note_id      = note_id,
    company_name = company_name,
    feature_ids  = list(feature_ids)
  )
}

#' @noRd
.pb_note_display_name <- function(notes, id) {
  nm <- notes[["fields.name"]][match(id, notes$id)]
  if (is.null(nm) || length(nm) == 0 || is.na(nm)) "(untitled note)" else nm
}

#' @noRd
.pb_feature_display_name <- function(features, id) {
  nm <- features[["fields.name"]][match(id, features$id)]
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

    status <- features[["fields.status.name"]][match(fid, features$id)]
    if (is.null(status) || length(status) == 0) status <- NA_character_

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
