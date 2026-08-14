#' Authenticate against the Google Slides API.
#'
#' Returns a bearer access token for \code{slides.googleapis.com}. The Slides
#' scope is not covered by the token \code{googlesheets4::gs4_auth()} normally
#' mints, so this re-runs \code{gs4_auth()} asking for the Sheets \emph{and}
#' Slides scopes together. That reuses the OAuth client and token cache already
#' set up on the machine — no second client secret to configure — but it does
#' mean a one-time browser consent the first time you call it, after which the
#' widened token is cached like any other.
#'
#' Because the widened token supersedes the Sheets-only one, existing
#' \code{output_gsheet_*()} calls keep working unchanged.
#'
#' @param email Character. Google account to authenticate as. Defaults to the
#'   \code{IMMUTER_GOOGLE_EMAIL} environment variable, falling back to
#'   \code{"matt.benigni@immuta.com"}.
#'
#' @return Invisibly, the OAuth2 access token as a character string.
#'
#' @examples
#' \dontrun{
#'   slides_auth()
#' }
#' @export
slides_auth <- function(email = Sys.getenv("IMMUTER_GOOGLE_EMAIL",
                                           unset = "matt.benigni@immuta.com")) {

  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Package 'googlesheets4' is required.", call. = FALSE)
  }

  googlesheets4::gs4_auth(
    email  = email,
    scopes = c("https://www.googleapis.com/auth/spreadsheets",
               "https://www.googleapis.com/auth/presentations",
               "https://www.googleapis.com/auth/drive.file")
  )

  tok <- googlesheets4::gs4_token()
  access <- tryCatch(tok$auth_token$credentials$access_token,
                     error = function(e) NULL)

  if (is.null(access) || !nzchar(access)) {
    stop("Could not extract an access token from googlesheets4. ",
         "Try googlesheets4::gs4_deauth() then slides_auth() again.",
         call. = FALSE)
  }

  invisible(access)
}


# ── Internal REST helpers ─────────────────────────────────────────────────────

.SLIDES_BASE <- "https://slides.googleapis.com/v1/presentations"

# Shared request skeleton with a readable error body.
.slides_req <- function(token) {
  httr2::request(.SLIDES_BASE) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_error(body = function(resp) {
      e <- tryCatch(httr2::resp_body_json(resp)$error$message,
                    error = function(e) NULL)
      if (is.null(e)) NULL else e
    })
}

# GET a presentation. Returns the parsed JSON as a nested list.
.slides_get <- function(presentation_id, token) {
  resp <- .slides_req(token) |>
    httr2::req_url_path_append(presentation_id) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)
}

# POST a batchUpdate. `requests` is an unnamed list of request objects.
# Google caps a batch at 500 requests, so this chunks automatically.
.slides_batch_update <- function(presentation_id, requests, token,
                                 chunk_size = 400L) {
  requests <- Filter(Negate(is.null), requests)
  if (length(requests) == 0) return(invisible(0L))

  chunks <- split(requests, ceiling(seq_along(requests) / chunk_size))

  for (i in seq_along(chunks)) {
    .slides_req(token) |>
      httr2::req_url_path_append(paste0(presentation_id, ":batchUpdate")) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(list(requests = unname(chunks[[i]])),
                           auto_unbox = TRUE) |>
      httr2::req_perform()
  }

  invisible(length(requests))
}

# Deterministic, API-legal object ID for an account.
#
# Slides object IDs must match [a-zA-Z0-9_-]{5,50} and be unique within the
# presentation. Deriving them from the account name (rather than letting Google
# assign them) is what makes reruns idempotent: the generator can look up
# whether a slide already exists and update only the metric shapes, leaving the
# CSM's comment shapes alone. A short checksum guards against two account names
# slugifying to the same string.
.slide_uid <- function(account_name) {
  slug <- gsub("[^a-z0-9]", "", tolower(as.character(account_name)))
  slug <- substr(slug, 1, 24)
  if (!nzchar(slug)) slug <- "acct"

  chars <- utf8ToInt(as.character(account_name))
  csum  <- sum(chars * seq_along(chars)) %% 100000L

  paste0(slug, "_", formatC(csum, width = 5, flag = "0"))
}

# Object IDs for every shape on an account's slide. Anything prefixed "c_" is
# CSM-owned and must never be rewritten by the generator.
.slide_ids <- function(uid) {
  list(
    slide   = paste0("s_", uid),
    title   = paste0("t_", uid),
    arr     = paste0("r_", uid),
    meta    = paste0("d_", uid),
    header  = paste0("h", 1:3, "_", uid),
    metrics = paste0("m", 1:3, "_", uid),
    comment = paste0("c", 1:3, "_", uid)
  )
}

# Collect every objectId already present in the presentation, plus which of
# them currently hold text (deleteText errors on an empty shape).
.slides_index <- function(pres) {
  ids       <- character(0)
  has_text  <- character(0)

  for (sl in pres$slides %||% list()) {
    ids <- c(ids, sl$objectId)
    for (el in sl$pageElements %||% list()) {
      ids <- c(ids, el$objectId)
      txt <- el$shape$text$textElements
      if (!is.null(txt)) {
        body <- paste(vapply(txt, function(t) t$textRun$content %||% "",
                             character(1)), collapse = "")
        if (nzchar(trimws(body))) has_text <- c(has_text, el$objectId)
      }
    }
  }

  list(ids = unique(ids), has_text = unique(has_text))
}

# Replace a shape's text: delete existing content (only if any), then insert.
.slides_set_text <- function(object_id, text, index) {
  reqs <- list()

  if (object_id %in% index$has_text) {
    reqs <- c(reqs, list(list(
      deleteText = list(objectId  = object_id,
                        textRange = list(type = "ALL"))
    )))
  }

  if (nzchar(text)) {
    reqs <- c(reqs, list(list(
      insertText = list(objectId = object_id,
                        text     = text,
                        insertionIndex = 0L)
    )))
  }

  reqs
}

# createShape + styling for a TEXT_BOX at a given rect (points).
.slides_new_box <- function(object_id, slide_id, x, y, w, h,
                            size = 9, bold = FALSE,
                            colour = c(0, 0, 0), align = "START") {
  list(
    list(createShape = list(
      objectId    = object_id,
      shapeType   = "TEXT_BOX",
      elementProperties = list(
        pageObjectId = slide_id,
        size = list(width  = list(magnitude = w, unit = "PT"),
                    height = list(magnitude = h, unit = "PT")),
        transform = list(scaleX = 1, scaleY = 1,
                         translateX = x, translateY = y, unit = "PT")
      )
    )),
    list(updateTextStyle = list(
      objectId  = object_id,
      textRange = list(type = "ALL"),
      style = list(
        fontFamily = "Inter",
        fontSize   = list(magnitude = size, unit = "PT"),
        bold       = bold,
        foregroundColor = list(opaqueColor = list(rgbColor = list(
          red = colour[1], green = colour[2], blue = colour[3]
        )))
      ),
      fields = "fontFamily,fontSize,bold,foregroundColor"
    )),
    list(updateParagraphStyle = list(
      objectId  = object_id,
      textRange = list(type = "ALL"),
      style     = list(alignment = align),
      fields    = "alignment"
    ))
  )
}
