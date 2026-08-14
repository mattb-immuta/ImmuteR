#' Refresh customer name, CSM, and region in a roster Google Sheet.
#'
#' Updates the identity columns of a tracking sheet from the current
#' \code{op_list$accounts} — customer name, Salesforce account ID, Customer
#' Success Manager, and Region — while leaving every other column in the sheet
#' untouched.
#'
#' @section Why it doesn't just overwrite the sheet:
#' Roster sheets like this one carry hand-maintained columns alongside the
#' Salesforce-derived ones (Customer Agentic Vision, Owner/Champion, Agent Tool
#' Inventory, POC Status, and so on). Those are the whole point of the sheet and
#' cannot be regenerated from \code{op_list}. So rather than rewriting the tab,
#' this function:
#' \enumerate{
#'   \item reads the sheet as it currently stands;
#'   \item matches each existing row to an account by \strong{Salesforce account
#'     ID}, not by name — names change, IDs don't;
#'   \item rewrites only the four identity columns on matched rows, carrying
#'     every other cell through unchanged and in its existing row position;
#'   \item appends accounts that are in \code{op_list} but missing from the
#'     sheet, with the hand-maintained columns left blank;
#'   \item \strong{never deletes a row}. An account that has left
#'     \code{op_list} is reported and left in place, because its manual columns
#'     may still be wanted.
#' }
#' Row order is preserved by default so a CSM's notes stay where they expect
#' them, even when a reassignment would otherwise re-sort them elsewhere.
#'
#' Salesforce IDs come in 15- and 18-character forms; both sides are compared on
#' their common 15-character prefix.
#'
#' @param op_list Named list returned by \code{\link{enrich_opportunities}}.
#' @param ss Character. Sheet URL or ID. Defaults to the Agentic tracking sheet.
#' @param sheet Character or integer. Worksheet name or index. Defaults to
#'   \code{1}.
#' @param name_col,id_col,csm_col,region_col Character. Header text of the four
#'   managed columns, as they appear in row 1 of the sheet.
#' @param sort Logical. Re-sort all rows by CSM, then Region, then customer
#'   name. Defaults to \code{FALSE} — sorting moves rows out from under the
#'   people who wrote the manual columns.
#' @param dry_run Logical. Report the changes without writing. Defaults to
#'   \code{TRUE}, because this touches a shared sheet — pass \code{FALSE}
#'   explicitly once the diff looks right.
#'
#' @return Invisibly, a data frame of the proposed or applied changes with
#'   columns \code{account}, \code{field}, \code{old}, \code{new}.
#'
#' @examples
#' \dontrun{
#'   output_gsheet_account_roster(op_list)                 # dry run
#'   output_gsheet_account_roster(op_list, dry_run = FALSE)
#' }
#' @export
output_gsheet_account_roster <- function(
    op_list,
    ss         = "https://docs.google.com/spreadsheets/d/1WtuQ7Im0eme1SCkPgBaFOnCwRKABSfLZe9oUd4adWq0/edit",
    sheet      = 1,
    name_col   = "Customer Name",
    id_col     = "Salesforce Account ID",
    csm_col    = "Customer Success Manager",
    region_col = "Region",
    sort       = FALSE,
    dry_run    = TRUE) {

  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Package 'googlesheets4' is required.", call. = FALSE)
  }

  accounts <- op_list$accounts
  if (is.null(accounts)) {
    stop("`op_list$accounts` is NULL. Run enrich_opportunities() first.",
         call. = FALSE)
  }

  # ── Read the sheet as text so nothing is coerced or reformatted ────────────
  cur <- googlesheets4::read_sheet(ss, sheet = sheet, col_types = "c")
  cur <- as.data.frame(cur, check.names = FALSE, stringsAsFactors = FALSE)

  missing_cols <- setdiff(c(name_col, id_col, csm_col, region_col), names(cur))
  if (length(missing_cols) > 0) {
    stop("Sheet is missing expected column(s): ",
         paste(missing_cols, collapse = ", "),
         ". Columns found: ", paste(names(cur), collapse = ", "), call. = FALSE)
  }

  # ── Build the lookup from op_list ──────────────────────────────────────────
  src_id18 <- if ("Account ID 18" %in% names(accounts)) {
    as.character(accounts[["Account ID 18"]])
  } else {
    as.character(accounts[["Account ID"]])
  }

  src <- data.frame(
    id18   = src_id18,
    id15   = .sf_id15(src_id18),
    name   = as.character(accounts[["Account Name"]]),
    csm    = .roster_chr(accounts[["Customer Success Manager"]]),
    region = .roster_chr(accounts[["Region"]]),
    stringsAsFactors = FALSE
  )
  src <- src[!is.na(src$id15) & nzchar(src$id15), , drop = FALSE]
  src <- src[!duplicated(src$id15), , drop = FALSE]

  # ── Match existing rows on ID ──────────────────────────────────────────────
  cur_id15 <- .sf_id15(cur[[id_col]])
  idx      <- match(cur_id15, src$id15)

  changes <- list()
  note <- function(acct, field, old, new) {
    old <- .roster_chr(old); new <- .roster_chr(new)
    if (!identical(old, new)) {
      changes[[length(changes) + 1]] <<- data.frame(
        account = acct, field = field, old = old, new = new,
        stringsAsFactors = FALSE)
      TRUE
    } else FALSE
  }

  for (i in seq_len(nrow(cur))) {
    j <- idx[i]
    if (is.na(j)) next
    acct <- src$name[j]

    note(acct, "Name",   cur[[name_col]][i],   src$name[j])
    note(acct, "ID",     cur[[id_col]][i],     src$id18[j])
    note(acct, "CSM",    cur[[csm_col]][i],    src$csm[j])
    note(acct, "Region", cur[[region_col]][i], src$region[j])

    cur[[name_col]][i]   <- src$name[j]
    cur[[id_col]][i]     <- src$id18[j]
    cur[[csm_col]][i]    <- src$csm[j]
    cur[[region_col]][i] <- src$region[j]
  }

  # Rows the sheet has but op_list no longer does — reported, never deleted.
  stale <- cur[[name_col]][is.na(idx) & !is.na(cur_id15) & nzchar(cur_id15)]

  # ── Append accounts absent from the sheet ──────────────────────────────────
  new_src <- src[!(src$id15 %in% cur_id15), , drop = FALSE]

  if (nrow(new_src) > 0) {
    blank <- cur[rep(NA_integer_, nrow(new_src)), , drop = FALSE]
    blank[] <- lapply(blank, function(x) rep(NA_character_, nrow(new_src)))

    blank[[name_col]]   <- new_src$name
    blank[[id_col]]     <- new_src$id18
    blank[[csm_col]]    <- new_src$csm
    blank[[region_col]] <- new_src$region

    for (n in new_src$name) {
      changes[[length(changes) + 1]] <- data.frame(
        account = n, field = "Row", old = "(absent)", new = "(added)",
        stringsAsFactors = FALSE)
    }

    cur <- rbind(cur, blank)
    rownames(cur) <- NULL
  }

  if (sort) {
    cur <- cur[order(cur[[csm_col]], cur[[region_col]], cur[[name_col]],
                     na.last = TRUE), , drop = FALSE]
    rownames(cur) <- NULL
  }

  chg <- if (length(changes) > 0) do.call(rbind, changes) else
    data.frame(account = character(0), field = character(0),
               old = character(0), new = character(0),
               stringsAsFactors = FALSE)

  # ── Report / write ─────────────────────────────────────────────────────────
  n_add <- sum(chg$field == "Row")
  n_fld <- sum(chg$field != "Row")

  message(".. roster diff: ", nrow(cur), " row(s) total; ",
          n_fld, " field change(s) across ",
          length(unique(chg$account[chg$field != "Row"])), " account(s); ",
          n_add, " row(s) to append",
          if (length(stale) > 0) {
            paste0("; ", length(stale), " sheet row(s) no longer in op_list (kept): ",
                   paste(stale, collapse = ", "))
          } else "")

  if (dry_run) {
    message(".. dry run — nothing written. Re-run with dry_run = FALSE to apply.")
    return(invisible(chg))
  }

  # reformat = FALSE leaves the sheet's own cell formatting alone; only values
  # are replaced. Writing from A1 rewrites the header row identically too.
  googlesheets4::range_write(ss, data = cur, sheet = sheet, range = "A1",
                             col_names = TRUE, reformat = FALSE)

  message(".. roster written to ", ss)
  invisible(chg)
}


# ── Internal helpers ──────────────────────────────────────────────────────────

# Trim to a plain character scalar, mapping blanks/dashes to "".
.roster_chr <- function(x) {
  v <- trimws(as.character(x))
  v[is.na(v) | v %in% c("-", "—", "NA")] <- ""
  v
}
