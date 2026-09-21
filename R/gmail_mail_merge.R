# ============================================================================
# gmail_mail_merge.R
#
# Mail merge for Gmail where the email BODY lives in one shared Google Doc
# template, and a spreadsheet supplies the per-send recipients and the
# tenant list to drop into a specific spot in that template.
#
# SPREADSHEET ROW SHAPE (one row = one email):
#
#   to               - recipient addresses. Multiple addresses in one cell,
#                       separated by "," or ";" (e.g. "a@x.com; b@x.com")
#   cc               - CC addresses, same delimited format (optional)
#   subject           - subject line. May contain {{column_name}} tokens
#                       that get filled from any other column in that row
#                       (e.g. "Incident update: {{affected_tenants}}")
#   affected_tenants  - affected SaaS tenants for that send, delimited the
#                       same way as `to` (e.g. "Acme Corp; Globex; Initech")
#
# GOOGLE DOC TEMPLATE:
#
#   One Google Doc holds the shared body copy. Wherever the tenant list
#   should appear, type the literal token:
#
#       {{affected_tenants}}
#
#   in the Doc itself. At send time that token is replaced with a proper
#   HTML bulleted list of that row's tenants. Any other {{column_name}}
#   token you type in the Doc is filled from the matching spreadsheet
#   column too, so you can also drop things like {{to}} inline if useful.
#
# Requires: gmailr, googledrive, glue, dplyr
#   install.packages(c("gmailr", "googledrive", "glue", "dplyr"))
# ============================================================================

library(gmailr)
library(googledrive)
library(glue)
library(dplyr)

# ---------------------------------------------------------------------------
# ONE-TIME AUTH SETUP
# ---------------------------------------------------------------------------
# You need both Gmail send access and Drive read access (to export the Doc).
#
#   gmailr::gm_auth_configure(path = "path/to/client_secret.json")
#   gmailr::gm_auth(email = "you@yourdomain.com", scopes = "send")
#
#   googledrive::drive_auth(email = "you@yourdomain.com",
#                            scopes = "https://www.googleapis.com/auth/drive.readonly")
#
# Both use the same OAuth client under the hood (gargle) and will cache
# tokens locally, so this is a one-time step per machine/user.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

#' Split a delimited spreadsheet cell into a trimmed character vector
#'
#' Handles cells like "a@x.com, b@x.com" or "a@x.com; b@x.com" as well as
#' cells that are already a genuine vector (a list-column).
split_field <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(character(0))
  if (is.list(x)) x <- unlist(x)
  if (length(x) > 1) return(trimws(as.character(x)))
  parts <- strsplit(as.character(x), "[,;]\\s*")[[1]]
  trimws(parts[nzchar(trimws(parts))])
}

#' Render a character vector as an HTML bulleted list (HTML-escaped)
tenants_to_html_bullets <- function(tenants) {
  if (length(tenants) == 0) return("")
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    s
  }
  items <- paste0("<li>", esc(tenants), "</li>", collapse = "")
  paste0("<ul>", items, "</ul>")
}

#' Download the shared Google Doc template as raw HTML (once per run)
#'
#' @param doc Anything googledrive::as_id() accepts: a Doc URL, a Doc ID,
#'   or a dribble from drive_get().
fetch_doc_template_html <- function(doc) {
  tmp <- tempfile(fileext = ".html")
  googledrive::drive_download(googledrive::as_id(doc), path = tmp,
                               type = "html", overwrite = TRUE)
  html <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(tmp)
  html
}

#' Run a Gmail mail merge whose body comes from a shared Google Doc template
#'
#' @param data data.frame/tibble with columns `to`, `cc` (optional),
#'   `subject`, `affected_tenants` (see header comment for the exact shape).
#'   Read it in beforehand with read.csv(), readxl::read_excel(), or
#'   googlesheets4::read_sheet().
#' @param template_doc Google Doc URL or ID for the shared body template.
#' @param col_map Named list mapping logical field names (to, cc, subject,
#'   affected_tenants) to the actual column names in `data`, in case your
#'   sheet uses different headers. Defaults to identical names.
#' @param dry_run If TRUE, builds every message (including the merged body)
#'   but does not send — prints a preview per row. Always run this first.
#' @param delay_seconds Pause between sends to stay under Gmail API rate
#'   limits on large batches. Default 1 second.
#' @param stop_on_error If FALSE (default), a failed row is logged and the
#'   merge continues. If TRUE, the function stops at the first failure.
#'
#' @return A data.frame with one row per input row: `to`, `status`
#'   ("sent" / "dry_run" / "failed"), `message`.
#'
#' @examples
#' \dontrun{
#' rows <- read.csv("incident_notify.csv", stringsAsFactors = FALSE)
#' # rows$to               "security@acme.com; ciso@acme.com"
#' # rows$cc               "legal@acme.com"
#' # rows$subject          "Action required: incident affecting {{affected_tenants}}"
#' # rows$affected_tenants "Acme Corp; Globex"
#'
#' gmail_mail_merge(rows, template_doc = "https://docs.google.com/document/d/XXXX/edit",
#'                   dry_run = TRUE)
#'
#' results <- gmail_mail_merge(rows, template_doc = "https://docs.google.com/document/d/XXXX/edit")
#' }
gmail_mail_merge <- function(data,
                              template_doc,
                              col_map = list(
                                to = "to",
                                cc = "cc",
                                subject = "subject",
                                affected_tenants = "affected_tenants"
                              ),
                              dry_run = FALSE,
                              delay_seconds = 1,
                              stop_on_error = FALSE) {

  stopifnot(is.data.frame(data), nrow(data) > 0)

  to_col <- col_map$to
  if (is.null(to_col) || !to_col %in% names(data)) {
    stop("`data` must have a '", to_col %||% "to", "' column with recipient address(es).")
  }

  message("Fetching template Doc...")
  template_html <- fetch_doc_template_html(template_doc)

  get_col <- function(row, logical_name) {
    real_name <- col_map[[logical_name]]
    if (is.null(real_name) || !real_name %in% names(row)) return(NA_character_)
    val <- row[[real_name]]
    if (length(val) == 0 || (length(val) == 1 && is.na(val))) return(NA_character_)
    as.character(val)
  }

  # Fill {{placeholder}} tokens in a template string using every column
  # available in that row (e.g. {{subject}}, {{affected_tenants}}, ...).
  render_scalar_template <- function(template, row) {
    if (is.na(template) || !nzchar(template)) return(template %||% "")
    as.character(tryCatch(
      glue::glue_data(as.list(row), template, .open = "{{", .close = "}}"),
      error = function(e) template
    ))
  }

  results <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row <- data[i, , drop = FALSE]
    to_vec <- split_field(get_col(row, "to"))

    row_result <- list(to = paste(to_vec, collapse = "; "), status = "failed", message = "")

    if (length(to_vec) == 0) {
      row_result$message <- "missing 'to' address; row skipped"
      results[[i]] <- row_result
      message(sprintf("[%d/%d] SKIPPED: %s", i, nrow(data), row_result$message))
      next
    }

    tryCatch({
      cc_vec      <- split_field(get_col(row, "cc"))
      tenants_vec <- split_field(get_col(row, "affected_tenants"))
      subject_raw <- get_col(row, "subject") %||% ""

      # Subject supports the same {{column}} tokens as the doc body.
      subject <- render_scalar_template(subject_raw, row)

      # Build this row's body: replace {{affected_tenants}} with a real
      # bulleted list, and any other {{column_name}} token with that
      # column's plain value.
      body_html <- template_html
      body_html <- gsub("{{affected_tenants}}", tenants_to_html_bullets(tenants_vec),
                         body_html, fixed = TRUE)
      for (nm in names(row)) {
        token <- paste0("{{", nm, "}}")
        if (grepl(token, body_html, fixed = TRUE)) {
          body_html <- gsub(token, get_col(row, nm) %||% "", body_html, fixed = TRUE)
        }
      }

      if (dry_run) {
        message(sprintf(
          "[%d/%d] DRY RUN -> to: %s | subject: %s | tenants: %s",
          i, nrow(data), paste(to_vec, collapse = "; "), subject,
          paste(tenants_vec, collapse = ", ")
        ))
        row_result$status <- "dry_run"
        row_result$message <- "not sent (dry_run = TRUE)"
        results[[i]] <- row_result
        next
      }

      mime_msg <- gmailr::gm_mime() |>
        gmailr::gm_to(paste(to_vec, collapse = ", ")) |>
        gmailr::gm_subject(subject) |>
        gmailr::gm_html_body(body_html)

      if (length(cc_vec) > 0) {
        mime_msg <- gmailr::gm_cc(mime_msg, paste(cc_vec, collapse = ", "))
      }

      gmailr::gm_send_message(mime_msg)

      row_result$status <- "sent"
      row_result$message <- "ok"
      message(sprintf("[%d/%d] SENT -> %s", i, nrow(data), paste(to_vec, collapse = "; ")))

      if (delay_seconds > 0) Sys.sleep(delay_seconds)

    }, error = function(e) {
      row_result$status <<- "failed"
      row_result$message <<- conditionMessage(e)
      message(sprintf("[%d/%d] FAILED -> %s (%s)", i, nrow(data),
                       paste(to_vec, collapse = "; "), conditionMessage(e)))
      if (stop_on_error) stop(e)
    })

    results[[i]] <- row_result
  }

  dplyr::bind_rows(results)
}

# ---------------------------------------------------------------------------
# Example end-to-end usage
# ---------------------------------------------------------------------------
# gmailr::gm_auth_configure(path = "client_secret.json")
# gmailr::gm_auth(email = "you@yourdomain.com", scopes = "send")
# googledrive::drive_auth(email = "you@yourdomain.com",
#                          scopes = "https://www.googleapis.com/auth/drive.readonly")
#
# rows <- read.csv("incident_notify.csv", stringsAsFactors = FALSE)
#
# preview <- gmail_mail_merge(
#   rows,
#   template_doc = "https://docs.google.com/document/d/XXXXXXXXXXXX/edit",
#   dry_run = TRUE
# )
#
# results <- gmail_mail_merge(
#   rows,
#   template_doc = "https://docs.google.com/document/d/XXXXXXXXXXXX/edit",
#   delay_seconds = 1.5
# )
# table(results$status)
