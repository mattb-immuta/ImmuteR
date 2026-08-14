#' Copy the contents of \code{output/} to a synced Google Drive folder.
#'
#' Recursively copies every file under \code{output_dir} into
#' \code{drive_dir}, recreating the subdirectory structure
#' (\code{slide_headers/}, \code{account_briefs/}, ...) as it goes. This is a
#' plain filesystem copy — it assumes the shared Drive folder is mounted
#' locally by Google Drive for Desktop (typically somewhere under
#' \code{~/Library/CloudStorage/GoogleDrive-<you>/Shared drives/...} on macOS),
#' so the Drive client handles the actual upload. No \code{googledrive}
#' authentication is involved.
#'
#' Existing files with matching names are overwritten; files already in
#' \code{drive_dir} that no longer exist locally are left untouched, so this
#' will not delete anyone else's work. Set \code{overwrite = FALSE} to skip
#' rather than replace files that already exist.
#'
#' \code{drive_dir} must already exist. It is deliberately not created: a typo
#' in the path would otherwise produce a stray local folder that silently never
#' syncs to Drive.
#'
#' @param output_dir Character. Local directory to copy from. Defaults to
#'   \code{"output"} (relative to the project root, as \code{scripts/main.R}
#'   sets it).
#' @param drive_dir Character. Destination directory — the locally synced path
#'   to the shared Drive folder. Defaults to the \code{IMMUTER_DRIVE_DIR}
#'   environment variable, which is the intended way to set this (add it to
#'   \code{.Renviron} via \code{usethis::edit_r_environ()}) so the path stays
#'   out of version control.
#' @param overwrite Logical. Replace files that already exist in
#'   \code{drive_dir}. Defaults to \code{TRUE}.
#' @param quiet Logical. Suppress the per-run summary message. Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly, a data frame with one row per file and columns
#'   \code{source}, \code{destination}, and \code{copied} (logical).
#'
#' @examples
#' \dontrun{
#'   # Reads the destination from IMMUTER_DRIVE_DIR
#'   sync_output_to_drive()
#'
#'   # Or point it somewhere explicitly
#'   sync_output_to_drive(
#'     drive_dir = "~/Library/CloudStorage/GoogleDrive-matt.benigni@immuta.com/Shared drives/Customer Success/ImmuteR"
#'   )
#' }
#' @export
sync_output_to_drive <- function(output_dir = "output",
                                 drive_dir  = Sys.getenv("IMMUTER_DRIVE_DIR", unset = ""),
                                 overwrite  = TRUE,
                                 quiet      = FALSE) {

  # ── 1. Validate paths ──────────────────────────────────────────────────────
  if (!nzchar(drive_dir)) {
    stop("No destination set. Either pass `drive_dir=` or add IMMUTER_DRIVE_DIR ",
         "to your .Renviron (usethis::edit_r_environ()), pointing at the locally ",
         "synced shared Drive folder.", call. = FALSE)
  }

  drive_dir <- path.expand(drive_dir)

  if (!dir.exists(output_dir)) {
    stop("Source directory '", output_dir, "' does not exist. ",
         "Is the working directory the project root?", call. = FALSE)
  }
  if (!dir.exists(drive_dir)) {
    stop("Drive directory '", drive_dir, "' does not exist. Check the path and ",
         "that Google Drive for Desktop is running — this function will not ",
         "create it, since a typo would produce a local folder that never syncs.",
         call. = FALSE)
  }

  src_root <- normalizePath(output_dir, mustWork = TRUE)
  dst_root <- normalizePath(drive_dir,  mustWork = TRUE)

  # Guard against copying a directory into itself or into its own child.
  if (identical(src_root, dst_root) ||
      startsWith(paste0(dst_root, .Platform$file.sep),
                 paste0(src_root, .Platform$file.sep))) {
    stop("`drive_dir` is the same as, or nested inside, `output_dir`. ",
         "Refusing to copy a directory into itself.", call. = FALSE)
  }

  # ── 2. Enumerate files ─────────────────────────────────────────────────────
  rel_files <- list.files(src_root, recursive = TRUE, all.files = FALSE,
                          full.names = FALSE, include.dirs = FALSE)

  # Drop OS/editor cruft that has no business on a shared drive.
  rel_files <- rel_files[!grepl("(^|/)(\\.DS_Store|Thumbs\\.db|~\\$[^/]*)$", rel_files)]

  if (length(rel_files) == 0) {
    if (!quiet) message(".. nothing to sync — '", output_dir, "' is empty.")
    return(invisible(data.frame(source      = character(0),
                                destination = character(0),
                                copied      = logical(0),
                                stringsAsFactors = FALSE)))
  }

  src_paths <- file.path(src_root, rel_files)
  dst_paths <- file.path(dst_root, rel_files)

  # ── 3. Recreate the subdirectory structure ─────────────────────────────────
  needed_dirs <- unique(dirname(dst_paths))
  needed_dirs <- needed_dirs[!dir.exists(needed_dirs)]
  for (d in needed_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  # ── 4. Copy ────────────────────────────────────────────────────────────────
  copied <- vapply(seq_along(src_paths), function(i) {
    tryCatch(
      file.copy(src_paths[i], dst_paths[i],
                overwrite = overwrite, copy.date = TRUE),
      error = function(e) {
        warning("Failed to copy '", rel_files[i], "': ", conditionMessage(e),
                call. = FALSE)
        FALSE
      }
    )
  }, logical(1))

  n_ok   <- sum(copied)
  n_fail <- sum(!copied)

  if (!quiet) {
    message(".. synced ", n_ok, " of ", length(copied), " file(s) to: ", dst_root,
            if (n_fail > 0) {
              paste0("  (", n_fail,
                     if (overwrite) " failed" else " skipped or failed", ")")
            } else "")
  }

  invisible(data.frame(source      = src_paths,
                       destination = dst_paths,
                       copied      = copied,
                       stringsAsFactors = FALSE))
}
