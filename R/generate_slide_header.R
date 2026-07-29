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
#'   \code{"output/slide_headers"}.
#' @param dpi Integer. Output resolution in dots per inch. Defaults to
#'   \code{600} (6000 × 600 px for a 10 × 1 inch image).
#'
#' @return Invisibly returns the full path to the written PNG file.
#'
#' @examples
#' \dontrun{
#'   generate_slide_header("Cigna", op_list)
#'   generate_slide_header("Cigna", op_list, dpi = 900)
#' }
#' @export
generate_slide_header <- function(account_name,
                                   op_list,
                                   output_dir = "output/slide_headers",
                                   dpi        = 600) {

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

    churn_quarter <- .coalesce_chr(next_ren[["churn_forecast_period"]][1], NA_character_)
    if (is.na(churn_quarter)) {
      cd <- suppressWarnings(as.Date(next_ren[["Close Date"]][1]))
      churn_quarter <- if (!is.na(cd)) date_to_fiscal_quarter(cd + 1) else "—"
    }

    renewal_label <- paste0("Next Renewal:  ", renewal_amt_fmt, ",  ", churn_quarter)
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

  # Row 1 — account name (left)
  p <- p +
    ggplot2::annotate("text",
      x = pad, y = div_y + (H - div_y) * 0.50,
      label = account_name,
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

  # ── 5. Write file ──────────────────────────────────────────────────────────
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  safe_name <- gsub("[^A-Za-z0-9_-]", "_", account_name)
  svg_path  <- file.path(tempdir(), paste0(safe_name, "_slide_header.svg"))
  png_path  <- file.path(output_dir, paste0(safe_name, "_slide_header.png"))

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
#' @param op_list Named list returned by \code{enrich_opportunities()}.
#' @param output_dir Character. Output directory. Defaults to
#'   \code{"output/slide_headers"}.
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
                                        output_dir = "output/slide_headers",
                                        dpi        = 600) {

  accounts <- unique(op_list$accounts[["Account Name"]])
  message("Generating slide headers for ", length(accounts), " accounts...")

  paths <- purrr::map_chr(accounts, function(acct) {
    tryCatch(
      generate_slide_header(acct, op_list,
                            output_dir = output_dir,
                            dpi        = dpi),
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
