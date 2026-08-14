#' Normalise QBR/EBR columns on the account table.
#'
#' The Salesforce account report carries QBR (quarterly business review) dates
#' and a note, but the field names have changed more than once —
#' \code{Date of last QBR/EBR}, later \code{Date of last EBR}, and whatever the
#' current report calls them. This function finds them by pattern and adds three
#' stably-named columns that the rest of the package reads:
#' \code{Date Last QBR}, \code{Date Next QBR}, and \code{QBR Note}.
#'
#' Source columns are left in place — this only adds the normalised copies, so
#' nothing that reads the original names breaks.
#'
#' Called by \code{\link{enrich_opportunities}} immediately after the account
#' report is fetched, so the fields land on \code{op_list$accounts} alongside
#' \code{Lifecycle Stage} and the adoption picklists.
#'
#' @section Detection:
#' \itemize{
#'   \item \strong{last QBR} — a column matching
#'     \code{/(last|prev|most recent).*(qbr|ebr)/i} or the reverse order.
#'   \item \strong{next QBR} — the same, with \code{/next|upcoming|scheduled/}.
#'   \item \strong{note} — \code{/(qbr|ebr).*(note|comment|summary)/i}, falling
#'     back to any \code{/note|comment/i} column.
#' }
#' Anything not found becomes \code{NA} with a warning naming the columns that
#' were available — the pull is never aborted, since a tile or report renders
#' an em-dash perfectly well.
#'
#' @param account.table Data frame. The Salesforce account report.
#' @param last_col,next_col,note_col Character or \code{NULL}. Explicit source
#'   column names, overriding detection.
#'
#' @return \code{account.table} with \code{Date Last QBR} (Date),
#'   \code{Date Next QBR} (Date), and \code{QBR Note} (character) appended.
#'
#' @examples
#' \dontrun{
#'   accounts <- normalize_qbr_fields(accounts)
#'
#'   # Override detection if the report's columns are named unexpectedly
#'   normalize_qbr_fields(accounts,
#'                        last_col = "Date of last EBR",
#'                        next_col = "Date of next EBR",
#'                        note_col = "EBR Summary")
#' }
#' @export
normalize_qbr_fields <- function(account.table,
                                 last_col = NULL,
                                 next_col = NULL,
                                 note_col = NULL) {

  nms <- names(account.table)

  last_col <- last_col %||% .qbr_pick(nms, "(last|prev|most.?recent)")
  next_col <- next_col %||% .qbr_pick(nms, "(next|upcoming|scheduled)")

  if (is.null(note_col)) {
    hits <- nms[grepl("(qbr|ebr).*(note|comment|summary)|(note|comment|summary).*(qbr|ebr)",
                      nms, ignore.case = TRUE)]
    if (length(hits) == 0) hits <- nms[grepl("note|comment", nms, ignore.case = TRUE)]
    note_col <- if (length(hits) == 0) NA_character_ else hits[1]
  }

  missing <- c(
    if (is.na(last_col)) "last QBR date",
    if (is.na(next_col)) "next QBR date",
    if (is.na(note_col)) "QBR note"
  )
  if (length(missing) > 0) {
    warning("Account report is missing: ", paste(missing, collapse = ", "),
            " — those columns will be NA. Columns available: ",
            paste(nms, collapse = ", "), call. = FALSE)
  }

  account.table[["Date Last QBR"]] <- if (is.na(last_col)) {
    as.Date(NA)
  } else {
    .qbr_parse_date(account.table[[last_col]])
  }

  account.table[["Date Next QBR"]] <- if (is.na(next_col)) {
    as.Date(NA)
  } else {
    .qbr_parse_date(account.table[[next_col]])
  }

  account.table[["QBR Note"]] <- if (is.na(note_col)) {
    NA_character_
  } else {
    n <- trimws(as.character(account.table[[note_col]]))
    n[!nzchar(n) | n %in% c("-", "—")] <- NA_character_
    n
  }

  message(".. QBR fields normalised [last='", last_col,
          "', next='", next_col, "', note='", note_col, "']: ",
          sum(!is.na(account.table[["Date Last QBR"]])), " last / ",
          sum(!is.na(account.table[["Date Next QBR"]])), " next / ",
          sum(!is.na(account.table[["QBR Note"]])), " note, of ",
          nrow(account.table), " account(s)")

  account.table
}


# ── Internal helpers ──────────────────────────────────────────────────────────

# Find a QBR/EBR column qualified by `when` (e.g. "last", "next").
# Returns NA_character_ rather than erroring — a missing field is survivable.
.qbr_pick <- function(nms, when) {
  hit <- nms[grepl(paste0(when, ".*(qbr|ebr)"), nms, ignore.case = TRUE) |
             grepl(paste0("(qbr|ebr).*", when), nms, ignore.case = TRUE)]
  if (length(hit) > 0) hit[1] else NA_character_
}

# Salesforce hands dates back as Date, POSIXct, or character in a few shapes.
.qbr_parse_date <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x[!is.na(x) & trimws(x) %in% c("-", "—")] <- NA_character_

  n_real <- sum(!is.na(x))
  if (n_real == 0) return(as.Date(rep(NA, length(x))))

  fmts <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d",
            "%m/%d/%y", "%d %b %Y", "%b %d, %Y")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(x, format = f))
    if (sum(!is.na(d)) >= 0.8 * n_real) return(d)
  }
  suppressWarnings(as.Date(x))
}
