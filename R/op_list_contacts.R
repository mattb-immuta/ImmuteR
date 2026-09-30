#' Pull the Salesforce contacts report into a one-row-per-contact table.
#'
#' The contacts report lists each customer's executive buyer, champion, and
#' incident response contacts. This keeps every report column as-is (so a
#' mail merge can use the email / name fields directly) and appends a stable
#' set of normalised columns, so downstream code doesn't depend on the exact
#' Salesforce labels.
#'
#' Roles are read from a role/persona text column (e.g. \code{Contact Role},
#' possibly a multi-select picklist like "Champion; Incident Response"), and
#' from checkbox columns named for the role (\code{Champion},
#' \code{Executive Buyer}, \code{Incident Response}, ...). A contact can hold
#' several roles.
#'
#' If the report is instead one row per \emph{account} with a column per role
#' (\code{Executive Buyer}, \code{Champion}, \code{Incident Response Contact}),
#' it is reshaped to one row per contact per role, picking up a matching
#' \code{<role> ... Email} column where there is one.
#'
#' @param sf_contacts_report Character. Salesforce report ID. Defaults to
#'   \code{"00OVV000006TwOX2A0"}.
#' @param contacts_table Optional data frame. Supply an already-pulled report
#'   to skip the Salesforce call (useful for testing).
#'
#' @return A tibble, one row per contact: the report's own columns plus
#'   \code{contact_account_id} (15-character Account ID), \code{contact_account_name},
#'   \code{contact_name}, \code{contact_email}, \code{contact_title},
#'   \code{contact_role} (display string, e.g. \code{"Champion; Incident Response"}),
#'   and the logical flags \code{is_executive_buyer}, \code{is_champion},
#'   \code{is_incident_response}.
#'
#' @examples
#' \dontrun{
#'   # Incident response recipients for a mail merge
#'   ir <- dplyr::filter(op_list$contacts, is_incident_response)
#' }
#' @export
op_list_contacts <- function(sf_contacts_report = "00OVV000006TwOX2A0",
                             contacts_table     = NULL) {

  ct <- if (is.null(contacts_table)) {
    salesforcer::sf_run_report(sf_contacts_report)
  } else {
    contacts_table
  }
  ct <- as.data.frame(ct, check.names = FALSE, stringsAsFactors = FALSE)

  # Wide (a name column per role) -> long first; everything below assumes one
  # row per contact.
  wide_cols <- lapply(.CONTACT_ROLES, function(rx) .contact_wide_col(ct, rx))
  if (any(!vapply(wide_cols, is.null, logical(1)))) {
    ct <- .contacts_wide_to_long(ct, wide_cols)
  }

  n    <- nrow(ct)
  cols <- names(ct)

  pick <- function(candidates) {
    hit <- intersect(candidates, cols)
    if (length(hit) == 0) rep(NA_character_, n) else .contact_clean(ct[[hit[1]]])
  }

  # ── Account keys ────────────────────────────────────────────────────────────
  acct_id <- pick(c("Account ID", "Account ID 18", "Account: Account ID",
                    "Account: Account ID 18", "AccountId"))
  # 18-char IDs share their first 15 characters with the 15-char form, which
  # is what op_list$accounts$`Account ID` carries.
  acct_id <- ifelse(is.na(acct_id), NA_character_, substr(acct_id, 1, 15))

  # ── Contact identity ────────────────────────────────────────────────────────
  full  <- pick(c(".wide_name", "Full Name", "Contact Name",
                  "Contact: Full Name", "Name"))
  first <- pick(c("First Name", "Contact: First Name"))
  last  <- pick(c("Last Name",  "Contact: Last Name"))
  built <- trimws(paste(ifelse(is.na(first), "", first),
                        ifelse(is.na(last),  "", last)))
  built[!nzchar(built)] <- NA_character_

  # ── Roles ───────────────────────────────────────────────────────────────────
  role_cols <- intersect(c(".wide_role", "Contact Role", "Contact Roles",
                           "Role", "Roles", "Buying Role", "Buyer Role",
                           "Persona", "Contact Type", "Stakeholder Role"), cols)
  role_text <- if (length(role_cols) == 0) rep(NA_character_, n) else
    .contact_clean(ct[[role_cols[1]]])

  flags <- lapply(names(.CONTACT_ROLES), function(r) {
    rx <- .CONTACT_ROLES[[r]]
    from_text <- grepl(rx, role_text, ignore.case = TRUE)
    # Checkbox column named for the role ("Champion", "Is Champion", ...).
    box <- cols[grepl(rx, cols, ignore.case = TRUE) &
                vapply(cols, function(cn) .contact_is_flag(ct[[cn]]), logical(1))]
    from_box <- if (length(box) == 0) rep(FALSE, n) else {
      v <- ct[[box[1]]]
      if (is.logical(v)) !is.na(v) & v else
        tolower(.contact_clean(v)) %in% c("true", "yes", "1")
    }
    from_text | from_box
  })
  names(flags) <- names(.CONTACT_ROLES)

  if (!any(unlist(flags))) {
    warning("Contacts report ", sf_contacts_report, ": no contact could be ",
            "identified as executive buyer / champion / incident response. ",
            "Columns: ", paste(cols, collapse = ", "), call. = FALSE)
  }

  labels <- c(executive_buyer = "Executive Buyer", champion = "Champion",
              incident_response = "Incident Response")
  contact_role <- vapply(seq_len(n), function(i) {
    r <- labels[vapply(names(labels), function(k) flags[[k]][i], logical(1))]
    if (length(r) == 0) NA_character_ else paste(r, collapse = "; ")
  }, character(1))

  out <- ct
  out$contact_account_id   <- acct_id
  out$contact_account_name <- pick(c("Account Name", "Account: Account Name", "Account"))
  out$contact_name         <- ifelse(is.na(full), built, full)
  out$contact_email        <- pick(c(".wide_email", "Email", "Contact: Email",
                                     "Email Address"))
  out$contact_title        <- pick(c("Title", "Contact: Title", "Job Title"))
  out$contact_role         <- contact_role
  out$is_executive_buyer   <- flags$executive_buyer
  out$is_champion          <- flags$champion
  out$is_incident_response <- flags$incident_response

  # Scratch columns from the wide reshape.
  out <- out[, setdiff(names(out), c(".wide_name", ".wide_email", ".wide_role")),
             drop = FALSE]

  message("  contacts: ", n, " contacts; executive buyer ",
          sum(flags$executive_buyer), ", champion ", sum(flags$champion),
          ", incident response ", sum(flags$incident_response))

  tibble::as_tibble(out)
}


# Role key -> pattern matched against role text and column labels.
.CONTACT_ROLES <- list(
  executive_buyer   = "(executive|economic)[ _.-]*buyer",
  champion          = "champion",
  incident_response = "incident[ _.-]*response"
)

.contact_clean <- function(v) {
  v <- trimws(as.character(v))
  v[v %in% c("", "-")] <- NA_character_
  v
}

# A checkbox column (TRUE/FALSE, Yes/No, 0/1) rather than a name.
.contact_is_flag <- function(v) {
  if (is.logical(v)) return(TRUE)
  u <- unique(tolower(stats::na.omit(.contact_clean(v))))
  length(u) > 0 && all(u %in% c("true", "false", "yes", "no", "0", "1"))
}

# For a report with one column per role: that role's name column (and email
# column, if any), or NULL. Skips the rating/detail fields that share the
# prefix, and checkbox columns.
.contact_wide_col <- function(ct, role_rx) {
  cols <- names(ct)
  hit  <- cols[grepl(role_rx, cols, ignore.case = TRUE)]
  if (length(hit) == 0) return(NULL)

  detail <- grepl("relationship|influence|\\bid\\b|email|title|phone|date",
                  hit, ignore.case = TRUE, perl = TRUE)
  name_cols <- hit[!detail]
  name_cols <- name_cols[!vapply(name_cols, function(h) .contact_is_flag(ct[[h]]),
                                 logical(1))]
  if (length(name_cols) == 0) return(NULL)

  pref  <- c(name_cols[grepl("full name", name_cols, ignore.case = TRUE)],
             name_cols[grepl("name", name_cols, ignore.case = TRUE)],
             name_cols)
  email <- hit[grepl("email", hit, ignore.case = TRUE)]
  list(name = pref[1], email = if (length(email)) email[1] else NA_character_)
}

# One row per account with a column per role -> one row per (account, role,
# contact). Account columns are carried onto every row; the per-role columns
# become .wide_name / .wide_email / .wide_role.
.contacts_wide_to_long <- function(ct, wide_cols) {
  wide_cols <- Filter(Negate(is.null), wide_cols)
  role_specific <- unlist(lapply(names(.CONTACT_ROLES), function(r)
    names(ct)[grepl(.CONTACT_ROLES[[r]], names(ct), ignore.case = TRUE)]))
  base <- ct[, setdiff(names(ct), role_specific), drop = FALSE]

  labels <- c(executive_buyer = "Executive Buyer", champion = "Champion",
              incident_response = "Incident Response")

  parts <- lapply(names(wide_cols), function(r) {
    wc <- wide_cols[[r]]
    d  <- base
    d$.wide_name  <- .contact_clean(ct[[wc$name]])
    d$.wide_email <- if (is.na(wc$email)) NA_character_ else
      .contact_clean(ct[[wc$email]])
    d$.wide_role  <- labels[[r]]
    d[!is.na(d$.wide_name), , drop = FALSE]
  })
  long <- do.call(rbind, parts)

  # Several names in one cell ("A; B") -> one row each, so each contact gets
  # their own mail-merge row. Emails are split in step when counts agree.
  rows <- lapply(seq_len(nrow(long)), function(i) {
    nm <- trimws(strsplit(long$.wide_name[i], "\\s*;\\s*")[[1]])
    em <- if (is.na(long$.wide_email[i])) NA_character_ else
      trimws(strsplit(long$.wide_email[i], "\\s*[;,]\\s*")[[1]])
    if (length(nm) <= 1) return(long[i, , drop = FALSE])
    r <- long[rep(i, length(nm)), , drop = FALSE]
    r$.wide_name  <- nm
    r$.wide_email <- if (length(em) == length(nm)) em else long$.wide_email[i]
    r
  })
  long <- if (length(rows)) do.call(rbind, rows) else long

  # The same person in two roles -> one row with both roles.
  key <- paste(long[["Account Name"]], tolower(long$.wide_name), sep = "\r")
  keep <- !duplicated(key)
  long$.wide_role <- vapply(key, function(k)
    paste(unique(long$.wide_role[key == k]), collapse = "; "),
    character(1), USE.NAMES = FALSE)
  long$.wide_email <- vapply(key, function(k) {
    e <- stats::na.omit(long$.wide_email[key == k])
    if (length(e)) e[1] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  rownames(long) <- NULL
  long[keep, , drop = FALSE]
}


#' Append champion / executive buyer names to op_list$accounts.
#'
#' Rolls \code{contacts} (from \code{\link{op_list_contacts}}) up to one value
#' per account and joins by 15-character Account ID, falling back to Account
#' Name. Several contacts in one role are joined with \code{"; "}.
#'
#' @return \code{accounts} with \code{executive_buyer} and \code{champion}
#'   columns (\code{NA} where unknown). Any existing columns of those names are
#'   replaced.
#' @noRd
.accounts_add_contact_roles <- function(accounts, contacts) {

  role_cols <- c(executive_buyer = "is_executive_buyer", champion = "is_champion")
  for (r in names(role_cols)) accounts[[r]] <- NULL

  if (is.null(contacts) || nrow(contacts) == 0) {
    for (r in names(role_cols)) accounts[[r]] <- NA_character_
    return(accounts)
  }

  collapse <- function(x) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
  }

  roll <- function(key, flag) {
    keep <- !is.na(key) & flag
    ks <- unique(key[keep])
    stats::setNames(
      vapply(ks, function(k) collapse(contacts$contact_name[keep & key == k]),
             character(1)),
      ks)
  }

  acct_key <- substr(as.character(accounts[["Account ID"]]), 1, 15)

  for (r in names(role_cols)) {
    flag    <- contacts[[role_cols[[r]]]]
    by_id   <- roll(contacts$contact_account_id,   flag)
    by_name <- roll(contacts$contact_account_name, flag)
    accounts[[r]] <- dplyr::coalesce(unname(by_id[acct_key]),
                                     unname(by_name[accounts[["Account Name"]]]))
  }

  message("  contacts: executive buyer on ", sum(!is.na(accounts$executive_buyer)),
          ", champion on ", sum(!is.na(accounts$champion)),
          " of ", nrow(accounts), " accounts")

  accounts
}


#' The two contact roles for one account, ready to render.
#'
#' Shared by \code{\link{generate_slide_tile}} and
#' \code{\link{account_slide_payload}} so the PNG and the slide feed agree.
#' Relationship / influence come straight off the account report
#' (\code{Champion Relationship}, \code{Champion Influence},
#' \code{Executive Buyer Relationship}, \code{Executive Buyer Influence}).
#'
#' @param a_data One-row slice of \code{op_list$accounts}.
#' @return A list of two lists, each with \code{label}, \code{name},
#'   \code{relationship}, \code{influence} (character, \code{"—"} if missing).
#' @noRd
contact_role_rows <- function(a_data) {
  get <- function(candidates) {
    hit <- intersect(candidates, names(a_data))
    if (length(hit) == 0) return("—")
    v <- a_data[[hit[1]]][1]
    if (!is.na(v) && trimws(as.character(v)) == "-") v <- NA
    .coalesce_chr(v, "—")
  }
  list(
    list(label        = "Executive Buyer",
         name         = get(c("executive_buyer", "Executive Buyer")),
         relationship = get(c("Executive Buyer Relationship")),
         influence    = get(c("Executive Buyer Influence"))),
    list(label        = "Champion",
         name         = get(c("champion", "Champion")),
         relationship = get(c("Champion Relationship")),
         influence    = get(c("Champion Influence")))
  )
}

# Strong / Neutral / Weak -> dot colour, matching STRENGTH_COLOUR in
# report_all_accounts_html() (green / amber / red). Grey when blank.
.strength_colour <- function(x) {
  if (length(x) == 0 || is.na(x[1])) return("#3A3A3A")
  k <- tolower(trimws(as.character(x[1])))
  if (!nzchar(k)) return("#3A3A3A")
  switch(k,
    strong  = "#22C55E",
    neutral = "#F59E0B",
    weak    = "#EF4444",
    "#3A3A3A"
  )
}
