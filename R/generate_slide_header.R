#' Generate a branded PNG slide header for a single account.
#'
#' Creates a 10 × 1 inch PNG banner styled with Immuta brand tokens
#' (Inter font, \code{#0D0D0D} background, \code{#C8F135} lime ARR accent)
#' sized to sit at the top of a landscape Google Slide.
#'
#' Internally the graphic is first rendered as an SVG (pure vector), then
#' converted to a high-resolution PNG via \code{rsvg}. This two-step pipeline
#' keeps text as vector paths until the final rasterization, eliminating the
#' font-blurring that affects direct raster renderers. The default
#' \code{dpi = 600} produces a 6000 × 600 px PNG that stays crisp at any zoom
#' level in Google Slides.
#'
#' Three rows are rendered:
#' \enumerate{
#'   \item Account name (left, large) and Contract ARR rollup (right, lime).
#'   \item Account team: AE, CSM, and FCTO separated by dividers.
#'   \item "Next Renewal: \$Amount, Q#-YYYY" inline text.
#' }
#'
#' @param account_name Character. Exact Salesforce account name as it appears
#'   in \code{op_list$accounts}.
#' @param op_list Named list returned by \code{enrich_opportunities()}, expected
#'   to contain \code{accounts} and \code{renewals} elements.
#' @param output_dir Character. Directory to write the PNG into. Created
#'   recursively if it does not exist. Defaults to
#'   \code{"output/slide_content/headers"}.
#' @param dpi Integer. Output resolution in dots per inch. Defaults to
#'   \code{600} (6000 × 600 px for a 10 × 1 inch image).
#' @param index Integer or \code{NULL}. Sequence number used as the filename
#'   prefix. When \code{NULL} (the default) it is derived from the account's
#'   position in the canonical slide ordering of \code{op_list$accounts} —
#'   Region, then Customer Success Manager, then account name — so the header
#'   and the health summary for an account always share the same number.
#'
#' @return Invisibly returns the full path to the written PNG file. The file is
#'   named \code{"<index>_<account_name>_header.png"} — the name lowercased with
#'   spaces collapsed to underscores and all other punctuation dropped, e.g.
#'   \code{"1_acorns_header.png"}.
#'
#' @examples
#' \dontrun{
#'   generate_slide_header("Cigna", op_list)
#'   generate_slide_header("Cigna", op_list, dpi = 900)
#' }
#' @export
generate_slide_header <- function(account_name,
                                   op_list,
                                   output_dir = "output/slide_content/headers",
                                   dpi        = 600,
                                   index      = NULL) {

  # ── 0. Check dependencies ──────────────────────────────────────────────────
  if (!requireNamespace("ggplot2",  quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("svglite",  quietly = TRUE)) stop("Package 'svglite' is required.")
  if (!requireNamespace("rsvg",     quietly = TRUE)) stop("Package 'rsvg' is required.")

  # ── 1. Extract account data ────────────────────────────────────────────────
  a_data <- dplyr::filter(op_list$accounts, `Account Name` == account_name)
  if (nrow(a_data) == 0) stop("Account '", account_name, "' not found in op_list$accounts.")
  a_data <- a_data[1, ]

  arr_raw <- suppressWarnings(as.numeric(a_data[["Contract ARR Rollup"]]))
  arr_fmt <- if (!is.na(arr_raw) && arr_raw >= 1e6) {
    scales::dollar(arr_raw, scale = 1e-6, suffix = "M", accuracy = 0.01)
  } else if (!is.na(arr_raw)) {
    scales::dollar(arr_raw, accuracy = 1000)
  } else {
    "—"
  }

  ae   <- .coalesce_chr(a_data[["Account Owner"]],            "—")
  csm  <- .coalesce_chr(a_data[["Customer Success Manager"]], "—")
  fcto <- .coalesce_chr(a_data[["Field CTO"]],                "—")

  # ── 2. Identify next renewal ───────────────────────────────────────────────
  next_ren <- op_list$renewals |>
    dplyr::filter(`Account Name` == account_name) |>
    dplyr::arrange(`Close Date`) |>
    dplyr::slice(1)

  if (nrow(next_ren) > 0) {
    renewal_amt_raw <- suppressWarnings(as.numeric(next_ren[["Amount"]][1]))
    renewal_amt_fmt <- if (!is.na(renewal_amt_raw)) {
      scales::dollar(renewal_amt_raw, accuracy = 1000)
    } else {
      "—"
    }

    contract_end_date <- suppressWarnings(as.Date(next_ren[["Current Contract End Date"]][1]))
    contract_end_fmt  <- if (!is.na(contract_end_date)) format(contract_end_date, "%Y-%m-%d") else "—"

    renewal_label <- paste0("Next Renewal:  ", renewal_amt_fmt, ",  ", contract_end_fmt)
  } else {
    renewal_label <- "No open renewal"
  }

  # ── 3. Canvas and layout constants (inches as coordinate units) ────────────
  W          <- 10.0    # width  (inches)
  H          <- 1.0     # height (inches)
  pad        <- 0.28    # left / right margin
  div_y      <- 0.47    # horizontal divider y
  col_w      <- 1.65    # team column width
  team_start <- pad
  renewal_x  <- team_start + 3 * col_w + 0.18

  # svglite embeds the font name as a reference; rsvg resolves it via the
  # system font cache at rasterisation time.  Inter is pre-installed on macOS
  # 13+ and Windows 11; on older systems it falls back to the default sans.
  fam <- "Inter"

  # Text sizes (ggplot2 mm units; 1 pt ≈ 0.353 mm)
  sz_name  <- 6.5   # ~18 pt
  sz_arr   <- 5.2   # ~15 pt
  sz_body  <- 3.5   # ~10 pt
  sz_label <- 2.3   # ~6.5 pt

  # ── 4. Build plot ──────────────────────────────────────────────────────────
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, W), ylim = c(0, H), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#0D0D0D", colour = NA),
      plot.margin     = ggplot2::margin(0, 0, 0, 0)
    )

  # Row 1 — account name (left); append (M) badge for multiplatform accounts
  is_multiplatform <- isTRUE(a_data[["multiplatform"]])
  display_name     <- if (is_multiplatform) paste0(account_name, " (M)") else account_name

  p <- p +
    ggplot2::annotate("text",
      x = pad, y = div_y + (H - div_y) * 0.50,
      label = display_name,
      hjust = 0, vjust = 0.5,
      family = fam, fontface = "bold",
      size = sz_name, colour = "#FFFFFF"
    )

  # Row 1 — ARR label + value (right-aligned)
  p <- p +
    ggplot2::annotate("text",
      x = W - pad, y = div_y + (H - div_y) * 0.80,
      label = "CONTRACT ARR",
      hjust = 1, vjust = 0.5,
      family = fam, fontface = "bold",
      size = sz_label, colour = "#999999"
    ) +
    ggplot2::annotate("text",
      x = W - pad, y = div_y + (H - div_y) * 0.36,
      label = arr_fmt,
      hjust = 1, vjust = 0.5,
      family = fam, fontface = "bold",
      size = sz_arr, colour = "#C8F135"
    )

  # Horizontal divider
  p <- p +
    ggplot2::annotate("segment",
      x = pad, xend = W - pad,
      y = div_y, yend = div_y,
      colour = "white", alpha = 0.12, linewidth = 0.4
    )

  # Row 2 — account team columns
  team <- list(
    list(label = "AE",   value = ae),
    list(label = "CSM",  value = csm),
    list(label = "FCTO", value = fcto)
  )

  for (i in seq_along(team)) {
    col   <- team[[i]]
    col_x <- team_start + (i - 1) * col_w

    p <- p +
      ggplot2::annotate("text",
        x = col_x, y = div_y * 0.73,
        label = col$label,
        hjust = 0, vjust = 0.5,
        family = fam, fontface = "bold",
        size = sz_label, colour = "#999999"
      ) +
      ggplot2::annotate("text",
        x = col_x, y = div_y * 0.37,
        label = col$value,
        hjust = 0, vjust = 0.5,
        family = fam, fontface = "bold",
        size = sz_body, colour = "#FFFFFF"
      )

    if (i < length(team)) {
      div_x <- col_x + col_w - 0.18
      p <- p +
        ggplot2::annotate("segment",
          x = div_x, xend = div_x,
          y = div_y * 0.16, yend = div_y * 0.84,
          colour = "white", alpha = 0.12, linewidth = 0.4
        )
    }
  }

  # Row 2 — renewal info
  p <- p +
    ggplot2::annotate("text",
      x = renewal_x, y = div_y * 0.50,
      label = renewal_label,
      hjust = 0, vjust = 0.5,
      family = fam, fontface = "bold",
      size = sz_body, colour = "#FFFFFF"
    )

  # Row 2 — when op_list was built, bottom-right. The top-right corner is the
  # ARR block, so the date takes the corner below it: same edge, opposite row.
  # Deliberately the quietest text on the banner — a deck slide read months
  # later should be able to answer "how current was this?" without the date
  # competing with the account name or the ARR figure for attention.
  p <- p +
    ggplot2::annotate("text",
      x = W - pad, y = div_y * 0.50,
      label = .op_list_updated(op_list),
      hjust = 1, vjust = 0.5,
      family = fam, fontface = "bold",
      size = sz_label, colour = "#999999"
    )

  # ── 5. Write file ──────────────────────────────────────────────────────────
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Filename pattern: "<index>_<account_name>_header.png" (e.g. "1_acorns_header.png")
  if (is.null(index)) index <- .account_slide_index(op_list, account_name)
  stem     <- .account_asset_stem(index, account_name, "header")
  svg_path <- file.path(tempdir(),  paste0(stem, ".svg"))
  png_path <- file.path(output_dir, paste0(stem, ".png"))

  # Step 1: render to SVG (vector — fonts stay as paths)
  ggplot2::ggsave(
    filename = svg_path,
    plot     = p,
    width    = W,
    height   = H,
    units    = "in",
    device   = svglite::svglite,
    bg       = "#0D0D0D"
  )

  # Step 2: rasterise SVG → PNG at target DPI via librsvg
  # width in pixels = W (inches) × dpi
  rsvg::rsvg_png(svg_path, png_path, width = round(W * dpi))

  unlink(svg_path)   # clean up temp SVG

  message(".. slide header written: ", png_path,
          "  (", round(W * dpi), " x ", round(H * dpi), " px)")
  invisible(png_path)
}


#' Generate slide headers for every account in an op_list.
#'
#' Calls \code{\link{generate_slide_header}} for every unique account in
#' \code{op_list$accounts}. Accounts that fail are skipped with a warning.
#'
#' Headers only. Because a skipped account still leaves the other accounts
#' numbered around it, and because the health summaries are generated by a
#' separate function that may skip a different set, this can produce more headers
#' than health summaries. Use \code{\link{generate_all_slide_assets}} for the
#' paired, gap-free 1..N set the Google Slides builder expects.
#'
#' @param op_list Named list returned by \code{enrich_opportunities()}.
#' @param output_dir Character. Output directory. Defaults to
#'   \code{"output/slide_content/headers"}.
#' @param dpi Integer. Output resolution. Defaults to \code{600}.
#'
#' @return Invisibly returns a named character vector of written file paths
#'   (\code{NA} for any skipped accounts).
#'
#' @examples
#' \dontrun{
#'   generate_all_slide_headers(op_list)
#'   generate_all_slide_headers(op_list, dpi = 900)
#' }
#' @export
generate_all_slide_headers <- function(op_list,
                                        output_dir = "output/slide_content/headers",
                                        dpi        = 600) {

  accounts <- .account_slide_order(op_list)
  message("Generating slide headers for ", length(accounts), " accounts...")

  paths <- purrr::imap_chr(accounts, function(acct, i) {
    tryCatch(
      generate_slide_header(acct, op_list,
                            output_dir = output_dir,
                            dpi        = dpi,
                            index      = i),
      error = function(e) {
        warning("Skipped '", acct, "': ", conditionMessage(e), call. = FALSE)
        NA_character_
      }
    )
  })

  names(paths) <- accounts
  n_ok  <- sum(!is.na(paths))
  n_err <- sum(is.na(paths))
  message("Done. ", n_ok, " header(s) written",
          if (n_err > 0) paste0(", ", n_err, " skipped.") else ".")

  invisible(paths)
}


# ── Internal helpers ──────────────────────────────────────────────────────────

.coalesce_chr <- function(x, fallback = "—") {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) fallback else as.character(x)
}

# Canonical account ordering for generated slide assets.
#
# Grouped by Region, then Customer Success Manager, then account name — so the
# generated PNGs fall into the order a regional review is actually presented in
# (one region at a time, one CSM's book at a time) rather than a flat A–Z run.
#
# Ordering is derived from op_list$accounts rather than its row order, so an
# account keeps the same sequence number whichever generator runs and whether or
# not the Salesforce report happens to come back in a different order between
# runs. Accounts with a missing Region or CSM sort last within their group so an
# incomplete Salesforce record can't push the populated ones out of order.
.account_slide_order <- function(op_list) {
  acc <- op_list$accounts
  nm  <- as.character(acc[["Account Name"]])

  keep <- !is.na(nm) & nzchar(trimws(nm))
  acc  <- acc[keep, , drop = FALSE]
  nm   <- trimws(nm[keep])

  # One row per account: the first occurrence wins, so a duplicated account name
  # can't produce two entries (and therefore two index numbers) for one account.
  first <- !duplicated(nm)
  acc   <- acc[first, , drop = FALSE]
  nm    <- nm[first]

  grp <- function(col) {
    if (!col %in% names(acc)) return(rep("", length(nm)))
    v <- trimws(as.character(acc[[col]]))
    v[is.na(v) | !nzchar(v)] <- NA_character_
    v
  }

  region <- grp("Region")
  csm    <- grp("Customer Success Manager")

  # NA last within each level; tolower() keeps the sort case-insensitive so
  # "acorns" and "Acorns" can't straddle another account.
  ord <- order(is.na(region), tolower(region),
               is.na(csm),    tolower(csm),
               tolower(nm),
               method = "radix")

  nm[ord]
}

# Position of one account in that ordering (NA if the account isn't present —
# the calling generator has already errored on that case by this point).
.account_slide_index <- function(op_list, account_name) {
  match(as.character(account_name), .account_slide_order(op_list))
}

# Filename stem for an account's generated assets:
#   "<index>_<cleaned account name>_<suffix>"   e.g. "1_acorns_header"
#
# The name is lowercased, whitespace collapses to single underscores, and every
# other special character is dropped ("Becton, Dickinson and Co." ->
# "becton_dickinson_and_co"). Because that mangling can in principle collide
# between two accounts, the index prefix is what keeps each filename unique, and
# it also sorts the assets into the same order as the account list.
.account_asset_stem <- function(index, account_name, suffix) {
  nm <- tolower(trimws(as.character(account_name)))
  nm <- gsub("[[:space:]]+", "_", nm)
  nm <- gsub("[^a-z0-9_]", "", nm)
  nm <- gsub("_+", "_", nm)
  nm <- gsub("^_+|_+$", "", nm)
  if (!nzchar(nm)) nm <- "account"

  idx <- suppressWarnings(as.integer(index))
  if (length(idx) != 1 || is.na(idx)) {
    stop("Invalid asset index for '", account_name, "'.", call. = FALSE)
  }

  paste0(idx, "_", nm, "_", suffix)
}
