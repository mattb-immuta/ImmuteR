#' Generate the paired header + health-summary PNGs for every account.
#'
#' The Google Slides builder pairs an account's two PNGs by the
#' \code{<index>_<account_name>} portion of their filenames, so it needs an
#' exact 1:1 set: every account with a header must also have a health summary,
#' the two must carry the same number and cleaned name, and the numbers must run
#' 1..N with no gaps. Calling \code{\link{generate_all_slide_headers}} and
#' \code{\link{generate_all_slide_tiles}} separately cannot guarantee that — a
#' tile that fails for one account leaves an orphaned header behind, which is
#' how a run ends up with more headers than health summaries.
#'
#' This driver enforces the invariant:
#' \enumerate{
#'   \item Both assets are rendered from one ordered account list, so an
#'     account's header and health summary always agree on name and number.
#'   \item Stale PNGs from earlier runs are removed first (numbering shifts
#'     whenever an account is added or dropped, so leftovers from a previous run
#'     would collide with the new sequence).
#'   \item Any account that fails to produce \emph{both} assets has its orphan
#'     deleted and is reported by name, with the underlying error.
#'   \item The surviving pairs are renumbered 1..N so the sequence is
#'     contiguous.
#' }
#'
#' A CSV manifest of \code{index, account_name, header_png, health_png} is
#' written alongside the assets for the slide builder to read, so it never has
#' to re-derive the cleaned filenames itself.
#'
#' @param op_list Named list returned by \code{enrich_opportunities()}.
#' @param headers_dir Character. Directory for the header PNGs. Defaults to
#'   \code{"output/slide_content/headers"}.
#' @param health_dir Character. Directory for the health-summary PNGs. Defaults
#'   to \code{"output/slide_content/health_summaries"}.
#' @param dpi Integer. Output resolution for both assets. Defaults to \code{600}.
#' @param manifest_path Character or \code{NULL}. Where to write the CSV
#'   manifest. \code{NULL} skips it. Defaults to
#'   \code{"output/slide_content/slide_assets_manifest.csv"}.
#' @param clean Logical. Delete existing PNGs in the two output directories
#'   before rendering. Defaults to \code{TRUE}; set \code{FALSE} only when
#'   deliberately topping up an existing set.
#'
#' @return Invisibly returns the manifest as a data frame, one row per complete
#'   pair, ordered by \code{index}.
#'
#' @examples
#' \dontrun{
#'   generate_all_slide_assets(op_list)
#' }
#' @export
generate_all_slide_assets <- function(op_list,
                                      headers_dir   = "output/slide_content/headers",
                                      health_dir    = "output/slide_content/health_summaries",
                                      dpi           = 600,
                                      manifest_path = "output/slide_content/slide_assets_manifest.csv",
                                      clean         = TRUE) {

  accounts <- .account_slide_order(op_list)
  if (length(accounts) == 0) stop("op_list$accounts has no account names.")

  .render_slide_asset_pairs(accounts, op_list,
                            headers_dir   = headers_dir,
                            health_dir    = health_dir,
                            dpi           = dpi,
                            manifest_path = manifest_path,
                            clean         = clean)
}


#' Generate the paired header + health-summary PNGs for one or more regions.
#'
#' Same guarantees as \code{\link{generate_all_slide_assets}} — 1:1 header and
#' health-summary pairs, orphans dropped, indices contiguous — applied to a
#' subset of accounts, so a region's deck can be rebuilt without re-rendering
#' the whole book.
#'
#' Each region gets its own subdirectory under \code{base_dir} and is numbered
#' \strong{1..N within that region}, not with its slice of the global numbering.
#' That is what makes a region set self-contained, and it is also why regions
#' are kept apart on disk: two regions numbered from 1 in one directory would
#' collide, and writing into the all-accounts directories would have a region
#' run silently clobber files belonging to accounts elsewhere.
#'
#' Accounts keep the canonical region/CSM/name ordering from
#' \code{\link{generate_all_slide_assets}}, so an account's position within its
#' region is the same in both.
#'
#' @param op_list Named list returned by \code{\link{enrich_opportunities}}.
#' @param region Character vector or \code{NULL}. Regions to render, matched
#'   case-insensitively against \code{op_list$accounts$Region}. \code{NULL}
#'   (default) renders every distinct non-empty region, one directory each. A
#'   region with no accounts is warned about and skipped; if none of the
#'   requested regions match, the function errors and lists what is available.
#' @param base_dir Character. Parent directory for the per-region asset
#'   directories. Defaults to \code{"output/slide_content"}.
#' @param dpi Integer. Output resolution for both assets. Defaults to
#'   \code{600}.
#' @param clean Logical. Delete existing PNGs in a region's directories before
#'   rendering it. Defaults to \code{TRUE}.
#'
#' @return Invisibly, the combined manifest as a data frame with a
#'   \code{region} column alongside \code{index}, \code{account_name},
#'   \code{header_png} and \code{health_png}. A per-region manifest CSV is also
#'   written into each region's directory.
#'
#' @examples
#' \dontrun{
#'   generate_region_slide_assets(op_list, "West")
#'   generate_region_slide_assets(op_list, c("West", "East"))
#'   generate_region_slide_assets(op_list)   # every region, one dir each
#' }
#' @export
generate_region_slide_assets <- function(op_list,
                                         region   = NULL,
                                         base_dir = "output/slide_content",
                                         dpi      = 600,
                                         clean    = TRUE) {

  accounts <- op_list$accounts
  if (is.null(accounts) || !("Region" %in% names(accounts))) {
    stop("op_list$accounts has no `Region` column.", call. = FALSE)
  }

  acct_region <- trimws(as.character(accounts$Region))
  available   <- sort(unique(acct_region[!is.na(acct_region) & nzchar(acct_region)]))

  if (is.null(region)) region <- available
  region <- trimws(as.character(region))
  region <- region[!is.na(region) & nzchar(region)]

  if (length(region) == 0) {
    stop("No regions to render. Available: ", paste(available, collapse = ", "),
         call. = FALSE)
  }

  # Canonical ordering once, then sliced per region — so an account's position
  # within its region matches generate_all_slide_assets().
  ordered <- .account_slide_order(op_list)

  manifests <- list()

  for (r in region) {
    in_region <- !is.na(acct_region) &
      tolower(acct_region) == tolower(r)

    region_accounts <- ordered[ordered %in%
                                 trimws(as.character(accounts[["Account Name"]][in_region]))]

    if (length(region_accounts) == 0) {
      warning("Region '", r, "' has no accounts; skipped. Available: ",
              paste(available, collapse = ", "), call. = FALSE)
      next
    }

    slug        <- .region_slug(r)
    headers_dir <- file.path(base_dir, slug, "headers")
    health_dir  <- file.path(base_dir, slug, "health_summaries")

    message("== ", r, " (", length(region_accounts), " accounts) -> ",
            file.path(base_dir, slug))

    m <- .render_slide_asset_pairs(
      region_accounts, op_list,
      headers_dir   = headers_dir,
      health_dir    = health_dir,
      dpi           = dpi,
      manifest_path = file.path(base_dir, slug, "slide_assets_manifest.csv"),
      clean         = clean
    )

    m$region    <- r
    manifests[[r]] <- m[, c("region", "index", "account_name",
                            "header_png", "health_png")]
  }

  if (length(manifests) == 0) {
    stop("None of the requested regions matched an account. Available: ",
         paste(available, collapse = ", "), call. = FALSE)
  }

  invisible(do.call(rbind, c(manifests, list(make.row.names = FALSE))))
}


# Directory-safe form of a region label ("EMEA / UKI" -> "emea_uki"). Lowercased
# so the directory name can't depend on how the region was typed in Salesforce.
.region_slug <- function(region) {
  s <- tolower(trimws(as.character(region)))
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) "region" else s
}


# The shared renderer behind generate_all_slide_assets() and
# generate_region_slide_assets(): renders both assets for an ordered account
# vector, drops half-pairs, renumbers the survivors 1..N, writes the manifest.
# Takes the account list rather than deriving it, which is the only difference
# between rendering the whole book and rendering one region.
.render_slide_asset_pairs <- function(accounts,
                                      op_list,
                                      headers_dir,
                                      health_dir,
                                      dpi           = 600,
                                      manifest_path = NULL,
                                      clean         = TRUE) {

  n <- length(accounts)
  if (n == 0) stop("No accounts to render.", call. = FALSE)

  for (d in c(headers_dir, health_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # Numbering is positional, so a leftover PNG from a run with a different
  # account set would sit in the output directory under a number that now
  # belongs to a different account.
  if (isTRUE(clean)) {
    stale <- c(list.files(headers_dir, pattern = "\\.png$", full.names = TRUE),
               list.files(health_dir,  pattern = "\\.png$", full.names = TRUE))
    if (length(stale) > 0) {
      unlink(stale)
      message("Cleared ", length(stale), " PNG(s) from the previous run.")
    }
  }

  message("Generating slide assets for ", n, " accounts...")

  # ── Pass 1: render both assets under a provisional index ────────────────────
  rendered <- lapply(seq_len(n), function(i) {
    acct <- accounts[i]

    hdr <- tryCatch(
      generate_slide_header(acct, op_list, output_dir = headers_dir,
                            dpi = dpi, index = i),
      error = function(e) conditionMessage(e)
    )
    hlt <- tryCatch(
      generate_slide_tile(acct, op_list, output_dir = health_dir,
                          dpi = dpi, index = i),
      error = function(e) conditionMessage(e)
    )

    hdr_ok <- .asset_written(hdr)
    hlt_ok <- .asset_written(hlt)

    list(
      account   = acct,
      header    = if (hdr_ok) hdr[1] else NA_character_,
      health    = if (hlt_ok) hlt[1] else NA_character_,
      hdr_error = if (hdr_ok) NA_character_ else as.character(hdr)[1],
      hlt_error = if (hlt_ok) NA_character_ else as.character(hlt)[1]
    )
  })

  # ── Drop half-pairs: an orphan is worse than an absent account ──────────────
  complete <- vapply(rendered, function(r) !is.na(r$header) && !is.na(r$health),
                     logical(1))

  for (r in rendered[!complete]) {
    for (f in c(r$header, r$health)) {
      if (!is.na(f) && file.exists(f)) unlink(f)
    }
    warning("No slide assets for '", r$account, "' — ",
            "header: ", if (is.na(r$hdr_error)) "ok" else r$hdr_error,
            "; health summary: ", if (is.na(r$hlt_error)) "ok" else r$hlt_error,
            call. = FALSE)
  }

  pairs <- rendered[complete]
  if (length(pairs) == 0) stop("No account produced both assets — nothing written.")

  # ── Pass 2: renumber the survivors 1..N so the sequence has no gaps ─────────
  manifest <- data.frame(
    index        = seq_along(pairs),
    account_name = vapply(pairs, function(r) r$account, character(1)),
    header_png   = NA_character_,
    health_png   = NA_character_,
    stringsAsFactors = FALSE
  )

  for (k in seq_along(pairs)) {
    manifest$header_png[k] <- .rename_slide_asset(pairs[[k]]$header, k,
                                                  pairs[[k]]$account, "header")
    manifest$health_png[k] <- .rename_slide_asset(pairs[[k]]$health, k,
                                                  pairs[[k]]$account, "health")
  }

  n_dropped <- sum(!complete)
  message("Done. ", nrow(manifest), " paired asset set(s) written",
          if (n_dropped > 0) paste0(", ", n_dropped, " account(s) dropped.") else ".")

  if (!is.null(manifest_path)) {
    mdir <- dirname(manifest_path)
    if (!dir.exists(mdir)) dir.create(mdir, recursive = TRUE)
    utils::write.csv(manifest, manifest_path, row.names = FALSE)
    message(".. manifest written: ", manifest_path)
  }

  invisible(manifest)
}


# TRUE when a generator returned a path that actually exists on disk. The
# generators return the path invisibly on success; on failure tryCatch() hands
# back the error message instead, which is a character string too — so the file
# check, not the type, is what distinguishes them.
.asset_written <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && file.exists(x)
}


# Move a rendered asset onto its final, contiguous index. Renaming after the
# fact (rather than rendering twice) is what lets the numbering depend on which
# accounts actually produced both files.
#
# Renames run in increasing index order and a final index is never greater than
# the provisional one it replaces, so the target name is always already free.
.rename_slide_asset <- function(path, index, account_name, suffix) {
  target <- file.path(dirname(path),
                      paste0(.account_asset_stem(index, account_name, suffix),
                             ".png"))
  if (identical(normalizePath(path, mustWork = FALSE),
                normalizePath(target, mustWork = FALSE))) {
    return(target)
  }
  if (!file.rename(path, target)) {
    stop("Could not rename '", basename(path), "' to '", basename(target), "'.",
         call. = FALSE)
  }
  target
}
