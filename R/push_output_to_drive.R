#' Upload the output directory to a Google Drive folder.
#'
#' Walks \code{output_dir} recursively and uploads every file to a Drive folder,
#' recreating the local subdirectory structure (\code{slide_content/headers/},
#' \code{slide_content/health_summaries/}, \code{slide_payloads/}, ...) as Drive
#' subfolders.
#'
#' Unlike \code{\link{sync_output_to_drive}}, which is a plain filesystem copy
#' into a locally-mounted Drive folder, this talks to the Drive API directly.
#' Use it when the shared folder isn't synced to the machine, or when the run is
#' unattended.
#'
#' @section Why this works when the Slides API doesn't:
#' The \code{googledrive} package ships with the tidyverse OAuth client, and the
#' Drive API \emph{is} enabled on that client's Google Cloud project. The Slides
#' API is not, which is why \code{\link{output_slides_regional_review}} needs an
#' Immuta-owned client and this function does not. First call opens a browser
#' for consent; the token is cached afterwards like googlesheets4's.
#'
#' @section Files are updated in place, not duplicated:
#' Drive happily stores several files with the same name in one folder, so a
#' naive re-upload would leave a pile of duplicates and break any link people
#' have shared. This function looks for an existing file of the same name in the
#' target folder and calls \code{drive_update()} on it, which keeps the file ID,
#' the URL, and any comments intact. Only genuinely new files are created.
#'
#' Nothing is ever deleted from Drive — a file removed locally stays there until
#' someone removes it by hand.
#'
#' @param output_dir Character. Local directory to upload. Defaults to
#'   \code{"output"}.
#' @param folder_id Character. Destination Drive folder — either a bare folder
#'   ID or the full \code{drive.google.com/drive/folders/...} URL. Defaults to
#'   the shared Customer Success folder.
#' @param email Character. Google account to authenticate as. Defaults to the
#'   \code{IMMUTER_GOOGLE_EMAIL} environment variable, falling back to
#'   \code{"matt.benigni@immuta.com"}.
#' @param pattern Character or \code{NULL}. Optional regex; only matching
#'   filenames are uploaded. \code{NULL} (default) uploads everything.
#' @param dry_run Logical. List what would be uploaded without touching Drive.
#'   Defaults to \code{FALSE}.
#'
#' @return Invisibly, a data frame with one row per file: \code{local_path},
#'   \code{drive_path}, \code{action} (\code{"created"} / \code{"updated"} /
#'   \code{"failed"}), and \code{id}.
#'
#' @examples
#' \dontrun{
#'   push_output_to_drive(dry_run = TRUE)   # see the plan first
#'   push_output_to_drive()
#'
#'   # Just the slide PNGs
#'   push_output_to_drive(pattern = "\\.png$")
#' }
#' @export
push_output_to_drive <- function(output_dir = "output",
                                 folder_id  = "18UzTfGqtIOW138wTC0LqpQkts7XwAwIx",
                                 email      = Sys.getenv("IMMUTER_GOOGLE_EMAIL",
                                                         unset = "matt.benigni@immuta.com"),
                                 pattern    = NULL,
                                 dry_run    = FALSE) {

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is required. install.packages('googledrive')",
         call. = FALSE)
  }
  if (!dir.exists(output_dir)) {
    stop("Source directory '", output_dir, "' does not exist. ",
         "Is the working directory the project root?", call. = FALSE)
  }

  root_id <- .drive_folder_id(folder_id)

  # ── Enumerate local files ──────────────────────────────────────────────────
  rel <- list.files(output_dir, recursive = TRUE, all.files = FALSE,
                    full.names = FALSE, include.dirs = FALSE)
  rel <- rel[!grepl("(^|/)(\\.DS_Store|Thumbs\\.db|~\\$[^/]*)$", rel)]
  if (!is.null(pattern)) rel <- rel[grepl(pattern, rel)]

  if (length(rel) == 0) {
    message(".. nothing to upload from '", output_dir, "'.")
    return(invisible(NULL))
  }

  if (dry_run) {
    message(".. dry run: ", length(rel), " file(s) would upload to folder ",
            root_id, ":")
    for (r in sort(rel)) message("     ", r)
    return(invisible(data.frame(local_path = file.path(output_dir, rel),
                                drive_path = rel,
                                action     = "dry_run",
                                id         = NA_character_,
                                stringsAsFactors = FALSE)))
  }

  googledrive::drive_auth(email = email)

  # ── Upload ─────────────────────────────────────────────────────────────────
  # Folder IDs are memoised so a directory of 70 PNGs costs one lookup, not 70.
  folder_cache <- new.env(parent = emptyenv())
  assign(".", root_id, envir = folder_cache)

  log <- vector("list", length(rel))

  for (i in seq_along(rel)) {
    r         <- rel[i]
    local     <- file.path(output_dir, r)
    sub       <- dirname(r)
    parent_id <- .drive_ensure_path(sub, root_id, folder_cache)

    res <- tryCatch({
      existing <- googledrive::drive_ls(
        path = googledrive::as_id(parent_id),
        pattern = paste0("^", .drive_regex_escape(basename(r)), "$")
      )
      existing <- existing[existing$name == basename(r), ]

      if (nrow(existing) > 0) {
        d <- googledrive::drive_update(googledrive::as_id(existing$id[1]),
                                       media = local)
        list(action = "updated", id = d$id[1])
      } else {
        d <- googledrive::drive_upload(media = local,
                                       path  = googledrive::as_id(parent_id),
                                       name  = basename(r),
                                       overwrite = FALSE)
        list(action = "created", id = d$id[1])
      }
    }, error = function(e) {
      warning("Failed to upload '", r, "': ", conditionMessage(e), call. = FALSE)
      list(action = "failed", id = NA_character_)
    })

    log[[i]] <- data.frame(local_path = local, drive_path = r,
                           action = res$action, id = res$id %||% NA_character_,
                           stringsAsFactors = FALSE)
  }

  out <- do.call(rbind, log)

  message(".. pushed to Drive folder ", root_id, ": ",
          sum(out$action == "created"), " created, ",
          sum(out$action == "updated"), " updated",
          if (any(out$action == "failed")) {
            paste0(", ", sum(out$action == "failed"), " FAILED")
          } else "")

  invisible(out)
}


# ── Internal helpers ──────────────────────────────────────────────────────────

# Accept either a bare Drive folder ID or a full folder URL.
.drive_folder_id <- function(x) {
  x <- trimws(as.character(x))
  m <- regmatches(x, regexpr("(?<=/folders/)[A-Za-z0-9_-]+", x, perl = TRUE))
  if (length(m) > 0) return(m[1])
  sub("[?#].*$", "", x)
}

# Escape a filename for use inside drive_ls()'s regex `pattern`.
.drive_regex_escape <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}

# Resolve (creating as needed) a nested subpath under `root_id`, memoising each
# level in `cache` so repeated files in the same directory cost one lookup.
.drive_ensure_path <- function(sub, root_id, cache) {
  if (identical(sub, ".") || !nzchar(sub)) return(root_id)
  if (exists(sub, envir = cache, inherits = FALSE)) {
    return(get(sub, envir = cache, inherits = FALSE))
  }

  parts     <- strsplit(sub, "/", fixed = TRUE)[[1]]
  parent_id <- root_id
  walked    <- character(0)

  for (p in parts) {
    walked <- c(walked, p)
    key    <- paste(walked, collapse = "/")

    if (exists(key, envir = cache, inherits = FALSE)) {
      parent_id <- get(key, envir = cache, inherits = FALSE)
      next
    }

    kids <- googledrive::drive_ls(path = googledrive::as_id(parent_id),
                                  type = "folder")
    hit  <- kids[kids$name == p, ]

    parent_id <- if (nrow(hit) > 0) {
      hit$id[1]
    } else {
      message(".. creating Drive folder: ", key)
      googledrive::drive_mkdir(name = p,
                               path = googledrive::as_id(parent_id))$id[1]
    }

    assign(key, parent_id, envir = cache)
  }

  parent_id
}
