#' Generate a branded PNG account tile for a single account.
#'
#' Creates a 5 × 8 inch PNG card styled with the same Immuta brand tokens as
#' \code{\link{generate_slide_header}} (Inter font, \code{#0D0D0D} background,
#' \code{#C8F135} lime accent), sized to drop into a Google Slide beside other
#' content.
#'
#' Like \code{generate_slide_header()}, the graphic is rendered first as an SVG
#' (pure vector) then rasterised to PNG via \code{rsvg}, so text stays crisp at
#' any zoom level. The default \code{dpi = 600} produces a 3000 × 4800 px PNG.
#'
#' Three lime-headed sections are rendered:
#' \describe{
#'   \item{\strong{Adoption}}{Stoplight dots for PBAC, Comply, Request, and
#'     Agentic Governance. PBAC is rated from
#'     \code{op_list$consumption$pbac_adoption} and Request from the stage
#'     tile the account appears on in Looker dashboard 1009, each labelled
#'     with what the value says ("Moderate adoption", "Retained") rather than
#'     the colour word; Comply and Agentic Governance are the Salesforce
#'     picklists, labelled Green / Yellow / Red / Black. Both derived rows
#'     fall back to their picklist when the pipeline ran without Looker. Then
#'     the platforms and features behind the PBAC rating, then Registered
#'     Users, Licensed Users, and the Consumption Ratio (colour-coded: lime at
#'     \eqn{\ge} 50\%, amber at \eqn{\ge} 25\%, red below); then Lifecycle
#'     Stage.}
#'   \item{\strong{Performance}}{Support cases over the trailing 90 and 360
#'     days, customer escalations over 90 days and currently open (from
#'     \code{op_list$support}, via BigQuery), plus deployment type, Immuta
#'     version, and version support status.}
#'   \item{\strong{Engagement}}{Executive Buyer and Champion side by side —
#'     the contact's name, then Relationship and Influence as stoplight dots
#'     (Strong green, Neutral amber, Weak red); then the date of the last and
#'     next QBR, and the QBR note (wrapped, truncated to two lines).}
#' }
#'
#' Data sources: the four adoption fields, \code{Lifecycle Stage},
#' \code{Is SaaS Customer}, \code{Customer on Unsupported Version}, and the
#' contact-role fields (\code{executive_buyer} and \code{champion}, rolled up
#' from \code{op_list$contacts} by \code{\link{enrich_opportunities}}, plus
#' \code{Executive Buyer Relationship} / \code{Influence} and
#' \code{Champion Relationship} / \code{Influence}), and the
#' three QBR fields (\code{Date Last QBR}, \code{Date Next QBR},
#' \code{QBR Note} — normalised by \code{\link{normalize_qbr_fields}}) come from
#' \code{op_list$accounts}; consumption figures and \code{Immuta Version} from
#' \code{op_list$consumption$low_consumption}; case counts from
#' \code{op_list$support} (built by \code{\link{op_list_support}}, which needs
#' \code{bigrquery::bq_auth()}). Any value that is missing
#' renders as an em-dash or a grey "no data" dot rather than erroring — so a
#' tile still builds from an SFDC-only \code{op_list}.
#'
#' @param account_name Character. Exact Salesforce account name as it appears
#'   in \code{op_list$accounts}.
#' @param op_list Named list returned by \code{enrich_opportunities()}.
#' @param output_dir Character. Directory to write the PNG into. Created
#'   recursively if it does not exist. Defaults to
#'   \code{"output/slide_content/health_summaries"}.
#' @param dpi Integer. Output resolution in dots per inch. Defaults to
#'   \code{600} (3000 × 4800 px for a 5 × 8 inch image).
#' @param index Integer or \code{NULL}. Sequence number used as the filename
#'   prefix. When \code{NULL} (the default) it is derived from the account's
#'   position in the canonical slide ordering of \code{op_list$accounts} —
#'   Region, then Customer Success Manager, then account name — so the health
#'   summary and the header for an account always share the same number.
#'
#' @return Invisibly returns the full path to the written PNG file. The file is
#'   named \code{"<index>_<account_name>_health.png"} — the name lowercased with
#'   spaces collapsed to underscores and all other punctuation dropped, e.g.
#'   \code{output/slide_content/health_summaries/1_acorns_health.png}.
#'
#' @examples
#' \dontrun{
#'   generate_slide_tile("Cigna", op_list)
#'   generate_slide_tile("Cigna", op_list, dpi = 900)
#' }
#' @export
generate_slide_tile <- function(account_name,
                                op_list,
                                output_dir = "output/slide_content/health_summaries",
                                dpi        = 600,
                                index      = NULL) {

  # ── 0. Check dependencies ──────────────────────────────────────────────────
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("svglite", quietly = TRUE)) stop("Package 'svglite' is required.")
  if (!requireNamespace("rsvg",    quietly = TRUE)) stop("Package 'rsvg' is required.")

  # ── 1. Account row ─────────────────────────────────────────────────────────
  a_data <- dplyr::filter(op_list$accounts, `Account Name` == account_name)
  if (nrow(a_data) == 0) stop("Account '", account_name, "' not found in op_list$accounts.")
  a_data <- a_data[1, ]

  # ── 2. Consumption row (absent if op_list_consumption() never ran) ─────────
  c_data <- .tile_row(op_list$consumption$low_consumption, account_name)

  reg_raw <- .tile_num(c_data, "Registered Users")
  lic_raw <- .tile_num(c_data, "Licensed Users")
  rat_raw <- .tile_num(c_data, "Consumption Ratio")

  # Recompute the ratio if the column is absent. consumption_ratio() carries the
  # ELA rule (Licensed Users of 0 is unlimited seats, so the ratio is 1, not NA).
  if (is.na(rat_raw)) rat_raw <- consumption_ratio(reg_raw, lic_raw)

  reg_fmt <- .tile_int(reg_raw)
  lic_fmt <- .tile_int(lic_raw)
  rat_fmt <- if (is.na(rat_raw)) "—" else paste0(round(rat_raw * 1000) / 10, "%")

  # Ratio colour matches pctClass() in report_all_accounts_html().
  rat_col <- if (is.na(rat_raw)) {
    "#999999"
  } else if (rat_raw >= 0.50) {
    "#C8F135"
  } else if (rat_raw >= 0.25) {
    "#F59E0B"
  } else {
    "#EF4444"
  }

  # ── 3. Support counts (absent unless enrich_opportunities(looker = TRUE)) ──
  s_data <- .tile_row(op_list$support, account_name)

  cases_90   <- .tile_num(s_data, "cases_90d")
  cases_360  <- .tile_num(s_data, "cases_360d")
  esc_90     <- .tile_num(s_data, "escalations_90d")
  esc_open   <- .tile_num(s_data, "escalations_open")

  # ── 4. Deployment / version ────────────────────────────────────────────────
  is_saas <- isTRUE(a_data[["Is SaaS Customer"]] %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
  unsup   <- isTRUE(a_data[["Customer on Unsupported Version"]] %in%
                      c(TRUE, "Yes", "TRUE", "true", "yes"))

  version_fmt <- .coalesce_chr(if (!is.null(c_data)) c_data[["Immuta Version"]] else NA, "—")

  # Deployment tag — same wording and palette as the .tag-saas / .tag-sm-sup /
  # .tag-sm-unsup pills on the account cards in report_all_accounts_html(), so
  # a tile and the dashboard never disagree about how an account is described.
  dep_tag <- .tile_deploy_tag(is_saas, unsup)

  # ── 5. Lifecycle stage ─────────────────────────────────────────────────────
  # One line: the platforms/features rows above pushed this block down, and a
  # second line would run into the Performance header. The longest picklist
  # value today is 40 characters, so nothing wraps — but a longer one would
  # truncate with an ellipsis rather than collide.
  lifecycle <- .tile_wrap(.coalesce_chr(a_data[["Lifecycle Stage"]], "—"),
                          width = 46, max_lines = 1)

  # ── 5b. QBR fields (normalised on accounts by normalize_qbr_fields()) ──────
  qbr_last <- fmt_date(a_data[["Date Last QBR"]])
  qbr_next <- fmt_date(a_data[["Date Next QBR"]])
  # Two lines, not three: the contact-role rows above took the third.
  qbr_note <- .tile_wrap(.coalesce_chr(a_data[["QBR Note"]], "—"),
                         width = 46, max_lines = 2)

  # ── 5c. Contact roles (Executive Buyer, Champion) ──────────────────────────
  # Shared with account_slide_payload() via contact_role_rows(). Several
  # contacts in one role arrive "; "-joined; the tile shows as much as fits
  # on one line of a half-width column.
  contact_roles <- contact_role_rows(a_data)

  # ── 6. Adoption stoplights + platform strings ─────────────────────────────
  # PBAC and Request are rated from Looker, Comply and Agentic from the
  # Salesforce picklists — see adoption_rows(), which account_slide_payload()
  # shares so the PNG and the slide feed can't disagree.
  p_data   <- pbac_row(op_list, account_name)
  adoption <- adoption_rows(a_data, p_data)

  # Features behind the PBAC rating, on one line under the stoplights.
  # "None" is a measurement (Looker ran, nothing is in use); an em-dash is the
  # absence of one, so an SFDC-only tile doesn't assert that a customer uses
  # no features when the truth is that nothing was measured.
  pbac_features <- if (is.null(p_data)) {
    "—"
  } else {
    f <- pbac_feature_labels(p_data)
    if (length(f) == 0) "None" else .tile_wrap(paste(f, collapse = " · "),
                                               width = 52, max_lines = 1)
  }

  plat_prod <- if (!is.null(c_data) && "platform_prod" %in% names(c_data) &&
                   !is.na(c_data[["platform_prod"]])) c_data[["platform_prod"]] else "—"
  plat_dev  <- if (!is.null(c_data) && "platform_dev"  %in% names(c_data) &&
                   !is.na(c_data[["platform_dev"]]))  c_data[["platform_dev"]]  else NA_character_

  # ── 7. Canvas and layout constants (inches as coordinate units) ────────────
  W   <- 5.0    # width  (inches)
  H   <- 8.0    # height (inches)
  pad <- 0.30   # left / right margin

  fam <- "Inter"   # resolved from the system font cache at rasterisation time

  # Text sizes (ggplot2 mm units; 1 pt ~ 0.353 mm)
  sz_header    <- 6.5   # ~18 pt   lime section headers
  sz_metric    <- 7.0   # ~20 pt   3-column big numbers
  sz_metric_sm <- 5.8   # ~16 pt   4-column support counts
  sz_body      <- 3.6   # ~10 pt
  sz_rating    <- 3.2   # ~9 pt    contact Relationship / Influence words
  sz_tag       <- 2.8   # ~8 pt    deployment pill (0.78x body, as in the HTML)
  sz_label     <- 2.5   # ~7 pt    micro-caps

  col_lime  <- "#C8F135"
  col_white <- "#FFFFFF"
  col_muted <- "#999999"
  col_none  <- "#3A3A3A"
  col_rule  <- "white"

  col3 <- (W - 2 * pad) / 3   # 3-column grid pitch
  col4 <- (W - 2 * pad) / 4   # 4-column grid pitch
  col2 <- (W - 2 * pad) / 2   # 2-column grid pitch

  # ── Vertical anchors, top-down ─────────────────────────────────────────────
  # ADOPTION — stoplights, consumption, lifecycle
  y_h1        <- H - 0.40
  y_rule_1    <- H - 0.66
  y_ad_row1   <- H - 0.94   # label; dot + status sit y_drop below
  y_ad_row2   <- H - 1.42
  y_drop      <- 0.24

  # Evidence for the PBAC rating, directly under the stoplights: which
  # platforms are in play (production in white, dev-only muted after it) and
  # which features are in use. This is where the old standalone platforms row
  # went — it says more next to the rating it explains than it did floating
  # below Lifecycle Stage, and moving it keeps the canvas the same height.
  y_plat_eye  <- H - 1.88
  y_plat_val  <- H - 2.08
  y_feat_eye  <- H - 2.30
  y_feat_val  <- H - 2.50

  y_con_value <- H - 2.86
  y_con_label <- H - 3.12
  y_lc_eye    <- H - 3.38
  y_lc_value  <- H - 3.58   # vjust = 1, so this is the top of the wrapped text

  # PERFORMANCE — deployment tag + version, then BigQuery incident/custx counts
  y_h2        <- H - 3.92
  y_rule_2    <- H - 4.18
  y_dep_label <- H - 4.42
  y_dep_value <- H - 4.70   # pill centre line, and the version value
  y_sup_eye   <- H - 5.00
  y_sup_value <- H - 5.34
  y_sup_label <- H - 5.58

  # ENGAGEMENT — contact roles, QBR dates, note. Pulled up to the same
  # header gap the other sections use, so the two contact rows fit.
  y_h3        <- H - 5.90
  y_rule_3    <- H - 6.14
  y_ct_label  <- H - 6.36   # EXECUTIVE BUYER | CHAMPION
  y_ct_name   <- H - 6.56
  y_ct_rate   <- H - 6.78   # REL ● word   INF ● word
  y_qbr_label <- H - 7.00
  y_qbr_value <- H - 7.20
  y_note_eye  <- H - 7.40
  y_note_val  <- H - 7.50   # vjust = 1, top of the wrapped note (2 lines)

  # ── 8. Build plot ──────────────────────────────────────────────────────────
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, W), ylim = c(0, H), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#0D0D0D", colour = NA),
      plot.margin     = ggplot2::margin(0, 0, 0, 0)
    )

  # Section header + underrule, used three times
  hdr <- function(plot, label, y_text, y_rule) {
    plot +
      ggplot2::annotate("text",
        x = pad, y = y_text, label = label,
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = sz_header, colour = col_lime
      ) +
      ggplot2::annotate("segment",
        x = pad, xend = W - pad, y = y_rule, yend = y_rule,
        colour = col_rule, alpha = 0.12, linewidth = 0.4
      )
  }

  # Micro-caps label above a value, on a shared x
  lab_val <- function(plot, x, y_label, y_value, label, value,
                      value_colour = col_white, value_size = sz_body) {
    plot +
      ggplot2::annotate("text",
        x = x, y = y_label, label = label,
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = sz_label, colour = col_muted
      ) +
      ggplot2::annotate("text",
        x = x, y = y_value, label = value,
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = value_size, colour = value_colour
      )
  }

  # ── Section 1: ADOPTION ────────────────────────────────────────────────────
  p <- hdr(p, "Adoption", y_h1, y_rule_1)

  ad_y <- c(y_ad_row1, y_ad_row2)
  for (i in seq_along(adoption)) {
    ad    <- adoption[[i]]
    x_0   <- pad + (((i - 1) %% 2)) * col2
    y_lab <- ad_y[((i - 1) %/% 2) + 1]
    y_val <- y_lab - y_drop

    p <- p +
      ggplot2::annotate("text",
        x = x_0, y = y_lab, label = toupper(ad$label),
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = sz_label, colour = col_muted
      ) +
      ggplot2::annotate("point",
        x = x_0 + 0.05, y = y_val,
        size = 2.8, shape = 21,
        fill = .tile_stoplight_colour(ad$value),
        colour = col_white, stroke = 0.25
      ) +
      ggplot2::annotate("text",
        x = x_0 + 0.22, y = y_val,
        label = ad$word,
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = sz_body, colour = col_white
      )
  }

  # Platforms + features, the evidence behind the PBAC rating above.
  p <- p +
    ggplot2::annotate("text",
      x = pad, y = y_plat_eye, label = "PLATFORMS",
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_label, colour = col_muted
    ) +
    ggplot2::annotate("text",
      x = pad, y = y_plat_val, label = plat_prod,
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_body, colour = if (identical(plat_prod, "—")) col_none else col_white
    ) +
    ggplot2::annotate("text",
      x = pad, y = y_feat_eye, label = "FEATURES",
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_label, colour = col_muted
    ) +
    ggplot2::annotate("text",
      x = pad, y = y_feat_val, label = pbac_features,
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_body,
      colour = if (pbac_features %in% c("None", "—")) col_none else col_white
    )

  # Dev/test-only platforms trail the production list in muted type, so the
  # difference between "running it" and "trying it" stays visible.
  if (!is.na(plat_dev) && nzchar(plat_dev)) {
    p <- p +
      ggplot2::annotate("text",
        x = pad + col2, y = y_plat_val, label = paste0("dev: ", plat_dev),
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = sz_body, colour = col_muted
      )
  }

  consumption <- list(
    list(label = "REGISTERED", value = reg_fmt, colour = col_white),
    list(label = "LICENSED",   value = lic_fmt, colour = col_white),
    list(label = "RATIO",      value = rat_fmt, colour = rat_col)
  )

  for (i in seq_along(consumption)) {
    cc <- consumption[[i]]
    p <- lab_val(p, pad + (i - 1) * col3,
                 y_con_label, y_con_value,
                 cc$label, cc$value,
                 value_colour = cc$colour, value_size = sz_metric)
  }

  p <- p +
    ggplot2::annotate("text",
      x = pad, y = y_lc_eye, label = "LIFECYCLE STAGE",
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_label, colour = col_muted
    ) +
    ggplot2::annotate("text",
      x = pad, y = y_lc_value, label = lifecycle,
      hjust = 0, vjust = 1, family = fam, fontface = "bold",
      size = sz_body, colour = col_white, lineheight = 1.15
    )

  # ── Section 2: PERFORMANCE ─────────────────────────────────────────────────
  p <- hdr(p, "Performance", y_h2, y_rule_2)

  # Deployment tag pill — the SaaS / SM: Supported / SM: Unsupported badge from
  # the dashboard, rendered here with the same fill, text and border colours.
  p <- p +
    ggplot2::annotate("text",
      x = pad, y = y_dep_label, label = "DEPLOYMENT",
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_label, colour = col_muted
    )

  p <- .tile_tag(p, x = pad, y = y_dep_value, tag = dep_tag,
                 fam = fam, size = sz_tag)

  p <- lab_val(p, pad + col3, y_dep_label, y_dep_value, "VERSION", version_fmt)

  # ── BigQuery incident / customer-escalation counts ─────────────────────────
  # Windows are 90/360 because that is what the rollup precomputes.
  # `escalations_open` is current state rather than a window, so it sits last.
  p <- p +
    ggplot2::annotate("text",
      x = pad, y = y_sup_eye, label = "INCIDENTS & CUSTOMER ESCALATIONS",
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_label, colour = col_muted
    )

  support <- list(
    list(label = "INC 90D",    value = cases_90,  escalation = FALSE),
    list(label = "INC 360D",   value = cases_360, escalation = FALSE),
    list(label = "CUSTX 90D",  value = esc_90,    escalation = TRUE),
    list(label = "CUSTX OPEN", value = esc_open,  escalation = TRUE)
  )

  for (i in seq_along(support)) {
    s <- support[[i]]
    # Escalations turn red once there's at least one; zero stays white.
    s_col <- if (is.na(s$value)) col_muted
             else if (s$escalation && s$value > 0) "#EF4444"
             else col_white

    p <- lab_val(p, pad + (i - 1) * col4,
                 y_sup_label, y_sup_value,
                 s$label, .tile_int(s$value),
                 value_colour = s_col, value_size = sz_metric_sm)
  }

  # ── Section 3: ENGAGEMENT ──────────────────────────────────────────────────
  p <- hdr(p, "Engagement", y_h3, y_rule_3)

  # Executive Buyer | Champion, each: micro-caps role, name, then the two
  # ratings as dots — same dot style as the adoption stoplights.
  for (i in seq_along(contact_roles)) {
    cr  <- contact_roles[[i]]
    x_0 <- pad + (i - 1) * col2
    nm  <- .tile_wrap(cr$name, width = 22, max_lines = 1)

    p <- p +
      ggplot2::annotate("text",
        x = x_0, y = y_ct_label, label = toupper(cr$label),
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = sz_label, colour = col_muted
      ) +
      ggplot2::annotate("text",
        x = x_0, y = y_ct_name, label = nm,
        hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
        size = sz_body, colour = if (identical(nm, "—")) col_none else col_white
      )

    ratings <- list(list(k = "REL", v = cr$relationship, dx = 0),
                    list(k = "INF", v = cr$influence,    dx = 1.10))
    for (r in ratings) {
      word <- if (identical(r$v, "—")) "—" else
        paste0(toupper(substring(r$v, 1, 1)), tolower(substring(r$v, 2)))
      p <- p +
        ggplot2::annotate("text",
          x = x_0 + r$dx, y = y_ct_rate, label = r$k,
          hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
          size = sz_label, colour = col_muted
        ) +
        ggplot2::annotate("point",
          x = x_0 + r$dx + 0.28, y = y_ct_rate,
          size = 2.8, shape = 21,
          fill = .strength_colour(r$v),
          colour = col_white, stroke = 0.25
        ) +
        ggplot2::annotate("text",
          x = x_0 + r$dx + 0.40, y = y_ct_rate,
          label = .tile_wrap(word, width = 9, max_lines = 1),
          hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
          size = sz_rating, colour = col_white
        )
    }
  }

  p <- lab_val(p, pad,        y_qbr_label, y_qbr_value, "LAST QBR", qbr_last)
  p <- lab_val(p, pad + col2, y_qbr_label, y_qbr_value, "NEXT QBR", qbr_next)

  p <- p +
    ggplot2::annotate("text",
      x = pad, y = y_note_eye, label = "QBR NOTE",
      hjust = 0, vjust = 0.5, family = fam, fontface = "bold",
      size = sz_label, colour = col_muted
    ) +
    ggplot2::annotate("text",
      x = pad, y = y_note_val, label = qbr_note,
      hjust = 0, vjust = 1, family = fam, fontface = "bold",
      size = sz_body, colour = col_white, lineheight = 1.15
    )

  # ── 9. Write file ──────────────────────────────────────────────────────────
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Filename pattern: "<index>_<account_name>_health.png" (e.g. "1_acorns_health.png")
  if (is.null(index)) index <- .account_slide_index(op_list, account_name)
  stem     <- .account_asset_stem(index, account_name, "health")
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
  rsvg::rsvg_png(svg_path, png_path, width = round(W * dpi))

  unlink(svg_path)

  message(".. account tile written: ", png_path,
          "  (", round(W * dpi), " x ", round(H * dpi), " px)")
  invisible(png_path)
}


#' Generate account tiles for every account in an op_list.
#'
#' Calls \code{\link{generate_slide_tile}} for every unique account in
#' \code{op_list$accounts}, writing one
#' \code{"<index>_<account_name>_health.png"} per account. Accounts that fail
#' are skipped with a warning.
#'
#' Health summaries only — see the note on
#' \code{\link{generate_all_slide_headers}}; use
#' \code{\link{generate_all_slide_assets}} when the headers and health summaries
#' have to line up.
#'
#' @param op_list Named list returned by \code{enrich_opportunities()}.
#' @param output_dir Character. Output directory. Defaults to
#'   \code{"output/slide_content/health_summaries"}.
#' @param dpi Integer. Output resolution. Defaults to \code{600}.
#'
#' @return Invisibly returns a named character vector of written file paths
#'   (\code{NA} for any skipped accounts).
#'
#' @examples
#' \dontrun{
#'   generate_all_slide_tiles(op_list)
#' }
#' @export
generate_all_slide_tiles <- function(op_list,
                                     output_dir = "output/slide_content/health_summaries",
                                     dpi        = 600) {

  accounts <- .account_slide_order(op_list)
  message("Generating account tiles for ", length(accounts), " accounts...")

  paths <- purrr::imap_chr(accounts, function(acct, i) {
    tryCatch(
      generate_slide_tile(acct, op_list, output_dir = output_dir, dpi = dpi,
                          index = i),
      error = function(e) {
        warning("Skipped '", acct, "': ", conditionMessage(e), call. = FALSE)
        NA_character_
      }
    )
  })

  names(paths) <- accounts
  n_ok  <- sum(!is.na(paths))
  n_err <- sum(is.na(paths))
  message("Done. ", n_ok, " tile(s) written",
          if (n_err > 0) paste0(", ", n_err, " skipped.") else ".")

  invisible(paths)
}


# ── Internal helpers ──────────────────────────────────────────────────────────

# One row for an account from a lookup table, or NULL if absent.
.tile_row <- function(df, account_name) {
  if (is.null(df) || !is.data.frame(df) || !"Account Name" %in% names(df)) return(NULL)
  hit <- df[!is.na(df[["Account Name"]]) & df[["Account Name"]] == account_name, , drop = FALSE]
  if (nrow(hit) == 0) NULL else hit[1, ]
}

# Pull a numeric column off a one-row data frame, tolerating NULL/missing.
.tile_num <- function(df, col) {
  if (is.null(df) || !col %in% names(df)) return(NA_real_)
  suppressWarnings(as.numeric(df[[col]][1]))
}

# Comma-grouped integer, em-dash when missing.
.tile_int <- function(x) {
  if (length(x) == 0 || is.na(x)) "—" else format(round(x), big.mark = ",", trim = TRUE)
}

# Deployment badge, mirroring the .tag-saas / .tag-sm-sup / .tag-sm-unsup rules
# in report_all_accounts_html(). Same three labels, same fill/text/border hexes
# (--im-lime-lt, --amber-lt, --red-lt resolved to literals), so the tile and the
# dashboard describe an account identically.
.tile_deploy_tag <- function(is_saas, unsupported) {
  if (isTRUE(is_saas)) {
    list(label = "SaaS",
         fill = "#F5FCD8", colour = "#5A7A00", border = "#C8E870")
  } else if (isTRUE(unsupported)) {
    list(label = "SM: Unsupported",
         fill = "#FEE2E2", colour = "#991B1B", border = "#FCA5A5")
  } else {
    list(label = "SM: Supported",
         fill = "#FEF3C7", colour = "#92400E", border = "#FCD34D")
  }
}

# Draw a rounded-rect badge with its label, left-aligned at x, centred on y.
#
# ggplot2 has no rounded-rectangle annotation, so the pill is a grid
# roundrectGrob placed via annotation_custom in data coordinates. Width is
# estimated from the label length: `size` is in ggplot mm units, so
# pt = size / 0.353, and bold Inter averages ~0.56 em per character.
.tile_tag <- function(p, x, y, tag, fam, size = 2.8,
                      pad_x = 0.10, height = 0.21) {
  char_in <- size / 0.353 / 72 * 0.56
  w <- 2 * pad_x + nchar(tag$label) * char_in

  grob <- grid::roundrectGrob(
    r  = grid::unit(2.5, "pt"),
    gp = grid::gpar(fill = tag$fill, col = tag$border, lwd = 0.8)
  )

  p +
    ggplot2::annotation_custom(grob,
      xmin = x, xmax = x + w,
      ymin = y - height / 2, ymax = y + height / 2
    ) +
    ggplot2::annotate("text",
      x = x + w / 2, y = y, label = tag$label,
      hjust = 0.5, vjust = 0.5, family = fam, fontface = "bold",
      size = size, colour = tag$colour
    )
}

# Leading colour word of a Salesforce adoption picklist value, e.g.
# "Green (any 2): 1. Multi-platform consumption ..." -> "green".
#
# The picklists say AMBER, not "yellow" — 22 of the 123 populated adoption
# values across the four fields lead with "Amber:", and omitting it made every
# one of them render as an em-dash. "yellow" is kept as an alias in case the
# picklist wording changes again.
.tile_stoplight_key <- function(x) {
  if (length(x) == 0 || is.na(x[1])) return(NA_character_)
  x <- trimws(as.character(x[1]))
  if (!nzchar(x) || x %in% c("-", "—")) return(NA_character_)
  m <- regmatches(x, regexpr("^(green|amber|yellow|red|black)\\b", x,
                             ignore.case = TRUE))
  if (length(m) == 0) NA_character_ else tolower(m)
}

# Dot fill colour — mirrors the .stoplight-* CSS in report_all_accounts_html().
.tile_stoplight_colour <- function(x) {
  k <- .tile_stoplight_key(x)
  if (is.na(k)) return("#3A3A3A")   # no data
  switch(k,
    green  = "#22C55E",
    amber  = "#F59E0B",
    yellow = "#F59E0B",
    red    = "#EF4444",
    black  = "#111111",
    "#3A3A3A"
  )
}

# Title-case status word beneath the label, em-dash when unknown.
.tile_stoplight_word <- function(x) {
  k <- .tile_stoplight_key(x)
  if (is.na(k)) return("—")
  paste0(toupper(substring(k, 1, 1)), substring(k, 2))
}

# Greedy word wrap into a single string with embedded newlines. Truncates to
# max_lines with an ellipsis so a long free-text note can't run off the canvas.
.tile_wrap <- function(x, width = 44, max_lines = NULL) {
  if (length(x) == 0 || is.na(x)) return("—")

  lines <- strwrap(as.character(x), width = width)
  if (length(lines) == 0) return("—")

  if (!is.null(max_lines) && length(lines) > max_lines) {
    lines <- lines[seq_len(max_lines)]
    lines[max_lines] <- paste0(sub("\\s+$", "", lines[max_lines]), "…")
  }
  paste(lines, collapse = "\n")
}
