#' One-quarter renewal risk brief for the ELT (PNG + HTML)
#'
#' Builds a single landscape image summarising renewal risk for one churn
#' quarter, laid out to be dropped straight into an exec deck:
#'
#' \itemize{
#'   \item a headline (churn forecast against plan, plus renewal ARR that is
#'     expected to renew but rated Black/Red on product adoption);
#'   \item four KPI tiles (up for renewal, churn forecast vs plan, Black/Red
#'     adoption ARR, Amber/Green adoption ARR);
#'   \item on the left, a table of every renewal in the quarter: customer,
#'     renewal amount, adoption rating, account risk, close date;
#'   \item on the right, two panels on one shared scale, each with two bars:
#'     \emph{Forecast churn} (Closed Lost, Verbally Confirmed, Will Churn,
#'     Expected to Churn renewals) shows their renewal ARR stacked by Account
#'     Risk next to their Churn Forecast stacked by adoption rating;
#'     \emph{Forecast retained} (At Risk, Expected to Renew, Closed Won) shows
#'     renewal amount minus Churn Forecast, stacked by Account Risk and then
#'     by adoption rating.
#' }
#'
#' @section Colour conventions:
#' Account Risk uses the historical forecast-sheet colours (Closed Lost
#' \code{#990000}, Verbally Confirmed \code{#EA9999}, Will Churn
#' \code{#E06666}, Expected to Churn \code{#F1C232}, At Risk \code{#FFE599},
#' Expected to Renew \code{#B6D7A8}, Closed Won \code{#38761D}). Adoption uses
#' the dashboard stoplights from \code{\link{report_all_accounts_html}} (Green
#' \code{#22C55E}, Amber \code{#F59E0B}, Red \code{#EF4444}, Black
#' \code{#111111}; unrated grey).
#'
#' @section Churn forecast:
#' Closed Lost, Verbally Confirmed, Will Churn and Expected to Churn count as
#' churn; the dollars are the opportunity's \code{Churn Forecast}, not its
#' Amount. Account Risk is matched on keywords, so the Salesforce spellings
#' ("Expected to Churn", "At Risk Renewal", "Expecting to Renew") all land in
#' the right bucket. The plan comes from \code{load_constants()$churn_plan}.
#'
#' @param op_list The list returned by \code{\link{enrich_opportunities}} /
#'   \code{\link{enrich_list_object}}.
#' @param churn_quarter Character, e.g. \code{"Q4-2027"} (the
#'   \code{\link{date_to_fiscal_quarter}} label).
#' @param output_dir Directory to write into. Created if missing.
#' @param max_rows Optional integer. When set and the quarter has more
#'   renewals, the table keeps the largest \code{max_rows} by amount (still in
#'   risk order) and adds a "+ N more" row. \code{NULL} (default) lists all.
#' @param png Logical. Also rasterise to PNG via \code{rsvg} (2x scale).
#' @param source \code{"auto"} (default) uses \code{op_list$forecast} when it
#'   has the quarter, otherwise \code{op_list$renewals}. Force one with
#'   \code{"forecast"} or \code{"renewals"}.
#' @param churn_basis \code{"contract_arr"} (default) counts every renewal at
#'   its account's \code{Contract ARR Rollup} — churn categories as churn, the
#'   rest as retained — and the table shows Contract ARR. An account with
#'   several renewals in the quarter is counted once. \code{"churn_forecast"}
#'   uses the opportunity \code{Churn Forecast} for churn and renewal Amount
#'   minus Churn Forecast for retained (use once the Salesforce field is fixed).
#'
#' @return Invisibly, a list with \code{data} (one row per renewal, as
#'   tabled), \code{svg}, \code{html} and \code{png} (file paths; \code{png}
#'   is \code{NULL} when not written).
#'
#' @examples
#' \dontrun{
#'   report_renewal_risk_brief(op_list, "Q4-2027")
#' }
#' @export
report_renewal_risk_brief <- function(op_list,
                                      churn_quarter,
                                      output_dir = file.path("output", "renewal_risk_brief"),
                                      max_rows   = NULL,
                                      png        = TRUE,
                                      source     = c("auto", "forecast", "renewals"),
                                      churn_basis = c("contract_arr", "churn_forecast")) {

  source      <- match.arg(source)
  churn_basis <- match.arg(churn_basis)
  stopifnot(is.character(churn_quarter), length(churn_quarter) == 1)
  churn_quarter <- trimws(churn_quarter)

  df <- .rrb_data(op_list, churn_quarter, source, churn_basis)
  if (nrow(df) == 0) {
    stop("No renewals with Churn Quarter ", churn_quarter, " in op_list.", call. = FALSE)
  }

  churn_zero <- sum(df$churn & df$churn_forecast <= 0)
  if (churn_basis == "churn_forecast" && churn_zero > 0) {
    message("  renewal risk brief: ", churn_zero, " churn-category renewal(s) have $0 ",
            "Churn Forecast and add nothing to the forecast.")
  }

  plan_tbl <- load_constants()$churn_plan
  plan     <- plan_tbl$company_plan[match(churn_quarter, plan_tbl$fiscal.period)]

  svg_txt <- .rrb_svg(df, churn_quarter, plan, max_rows, .rrb_as_of(op_list), churn_basis)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  stem     <- file.path(output_dir, paste0("renewal_risk_", churn_quarter))
  svg_path <- paste0(stem, ".svg")
  htm_path <- paste0(stem, ".html")
  writeLines(svg_txt, svg_path, useBytes = TRUE)
  writeLines(c(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    paste0("<title>Renewal risk ", churn_quarter, "</title>"),
    "<style>body{margin:0;padding:24px;background:#F3F4F6}svg{display:block;max-width:100%;height:auto;margin:auto}</style>",
    "</head><body>", svg_txt, "</body></html>"
  ), htm_path, useBytes = TRUE)

  png_path <- NULL
  if (isTRUE(png)) {
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      warning("Package 'rsvg' not installed; skipping PNG.", call. = FALSE)
    } else {
      png_path <- paste0(stem, ".png")
      w <- as.numeric(sub('.*width="([0-9.]+)".*', "\\1", substr(svg_txt, 1, 300)))
      rsvg::rsvg_png(charToRaw(svg_txt), png_path, width = 2 * w)
    }
  }

  message("  renewal risk brief: ", nrow(df), " renewal(s) in ", churn_quarter,
          " -> ", if (!is.null(png_path)) png_path else htm_path)
  invisible(list(data = df, svg = svg_path, html = htm_path, png = png_path))
}


# ── Internal: constants ───────────────────────────────────────────────────────

.RRB_RISK <- c("Closed Lost", "Verbally Confirmed", "Will Churn", "Expected to Churn",
               "At Risk", "Expected to Renew", "Closed Won", "Not set")
.RRB_CHURN  <- c("Closed Lost", "Verbally Confirmed", "Will Churn", "Expected to Churn")
.RRB_RETAIN <- c("At Risk", "Expected to Renew", "Closed Won")
# fill, ink (text on fill)
.RRB_RISK_COL <- list(
  "Closed Lost"        = c("#990000", "#FFFFFF"),
  "Verbally Confirmed" = c("#EA9999", "#5B0F0F"),
  "Will Churn"         = c("#E06666", "#4C0B0B"),
  "Expected to Churn"  = c("#F1C232", "#4A3800"),
  "At Risk"            = c("#FFE599", "#5C4A00"),
  "Expected to Renew"  = c("#B6D7A8", "#274E13"),
  "Closed Won"         = c("#38761D", "#FFFFFF"),
  "Not set"            = c("#D9D9D9", "#374151")
)
.RRB_ADOPT <- c(Black = "#111111", Red = "#EF4444", Amber = "#F59E0B",
                Green = "#22C55E", Unrated = "#D1D5DB")


# ── Internal: data ────────────────────────────────────────────────────────────

# Salesforce Account Risk picklist -> canonical label, by keyword.
.rrb_risk <- function(x) {
  v   <- tolower(trimws(as.character(x)))
  out <- rep("Not set", length(v))
  out[grepl("renew", v)]                         <- "Expected to Renew"
  out[grepl("at risk", v)]                       <- "At Risk"
  out[grepl("churn", v)]                         <- "Expected to Churn"
  out[grepl("will churn", v)]                    <- "Will Churn"
  out[grepl("verbal", v)]                        <- "Verbally Confirmed"
  out[grepl("closed won", v)]                    <- "Closed Won"
  out[grepl("closed lost", v)]                   <- "Closed Lost"
  out
}

# "Black: Critical Churn Risk" -> "Black"; blank -> "Unrated".
.rrb_adopt <- function(x) {
  v <- tolower(trimws(as.character(x)))
  m <- regmatches(v, regexpr("^(green|amber|yellow|red|black)", v))
  out <- rep("Unrated", length(v))
  hit <- grepl("^(green|amber|yellow|red|black)", v)
  m <- sub("^yellow$", "amber", m)
  out[hit] <- paste0(toupper(substr(m, 1, 1)), substr(m, 2, nchar(m)))
  out
}

.rrb_col <- function(df, choices) {
  hit <- intersect(choices, names(df))
  if (length(hit)) df[[hit[1]]] else rep(NA, nrow(df))
}

.rrb_id15 <- function(x) substr(trimws(as.character(x)), 1, 15)

# Date, POSIXct, ISO or US-style strings -> Date; anything else -> NA.
.rrb_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  us <- is.na(d) & !is.na(x)
  d[us] <- suppressWarnings(as.Date(x[us], format = "%m/%d/%Y"))
  d
}

.rrb_data <- function(op_list, q, source, basis = "contract_arr") {
  pick <- function(tbl) {
    tbl <- op_list[[tbl]]
    if (!is.data.frame(tbl) || !"Churn Quarter" %in% names(tbl)) return(NULL)
    tbl <- as.data.frame(tbl, check.names = FALSE, stringsAsFactors = FALSE)
    tbl <- tbl[!is.na(tbl$`Churn Quarter`) & trimws(tbl$`Churn Quarter`) == q, , drop = FALSE]
    if (nrow(tbl)) tbl else NULL
  }
  raw <- switch(source,
    forecast = pick("forecast"),
    renewals = pick("renewals"),
    auto     = { f <- pick("forecast"); if (is.null(f)) pick("renewals") else f }
  )
  if (is.null(raw)) return(data.frame())

  name <- as.character(.rrb_col(raw, c("Account Name", "Opportunity Name")))
  fallback <- as.character(.rrb_col(raw, "Opportunity Name"))
  name[is.na(name) | !nzchar(trimws(name))] <- fallback[is.na(name) | !nzchar(trimws(name))]

  df <- data.frame(
    account        = name,
    account_id     = as.character(.rrb_col(raw, c("Account ID", "Account ID 18"))),
    amount         = suppressWarnings(as.numeric(extract_usd_amount(.rrb_col(raw, c("Amount"))))),
    churn_forecast = suppressWarnings(as.numeric(extract_usd_amount(.rrb_col(raw, c("Churn Forecast"))))),
    risk_raw       = as.character(.rrb_col(raw, c("Account Risk"))),
    close_date     = .rrb_date(.rrb_col(raw, c("Close Date"))),
    stringsAsFactors = FALSE
  )
  df$amount[is.na(df$amount)]                 <- 0
  df$churn_forecast[is.na(df$churn_forecast)] <- 0
  df$risk  <- .rrb_risk(df$risk_raw)
  df$churn <- df$risk %in% .RRB_CHURN
  # Retained = renewal amount minus whatever churn is forecast on it (a
  # partial downsell on an At Risk renewal still comes off), floored at 0.
  df$retained <- ifelse(df$risk %in% .RRB_RETAIN, pmax(df$amount - df$churn_forecast, 0), 0)
  # Only the churn categories count toward the churn forecast total.
  df$churn_forecast[!df$churn] <- 0

  # Row in op_list$accounts for each renewal: Account ID (15-char) first,
  # then name. Feeds both the adoption rating and Contract ARR.
  acc <- op_list$accounts
  idx <- rep(NA_integer_, nrow(df))
  if (is.data.frame(acc) && nrow(acc) > 0) {
    if ("Account ID" %in% names(acc)) {
      idx <- match(.rrb_id15(df$account_id), .rrb_id15(acc$`Account ID`), incomparables = NA)
    }
    miss <- is.na(idx) & !is.na(df$account)
    if (any(miss) && "Account Name" %in% names(acc)) {
      idx[miss] <- match(tolower(trimws(df$account[miss])), tolower(trimws(acc$`Account Name`)))
    }
  }
  acc_col <- function(col) if (is.data.frame(acc) && col %in% names(acc)) acc[[col]][idx] else rep(NA, nrow(df))
  df$adoption     <- .rrb_adopt(acc_col("product_adoption_rating"))
  df$contract_arr <- suppressWarnings(as.numeric(extract_usd_amount(acc_col("Contract ARR Rollup"))))

  if (basis == "contract_arr") {
    # Contract ARR is per account, so an account with two renewals in the
    # quarter is counted once (on its largest renewal) rather than twice.
    # Falls back to the renewal Amount when the account has no Contract ARR.
    key   <- ifelse(is.na(idx), paste0("row", seq_len(nrow(df))), paste0("acc", idx))
    o     <- order(-df$amount)
    first <- logical(nrow(df))
    first[o] <- !duplicated(key[o])
    arr   <- ifelse(is.na(df$contract_arr), df$amount, df$contract_arr)
    df$amount_renewal <- df$amount
    df$amount         <- ifelse(first, arr, 0)
    df$churn_forecast <- ifelse(df$churn, df$amount, 0)
    df$retained       <- ifelse(df$risk %in% .RRB_RETAIN, df$amount, 0)
    n_dup <- sum(!first)
    if (n_dup > 0) {
      message("  renewal risk brief: ", n_dup, " extra renewal(s) on the same account counted at $0 ",
              "(Contract ARR is per account).")
    }
  }

  df[order(match(df$risk, .RRB_RISK), -df$amount, df$account), , drop = FALSE]
}

# Same "Updated <date>" stamp as the dashboard and slide headers.
.rrb_as_of <- function(op_list) .op_list_updated(op_list, prefix = "Updated ")


# ── Internal: rendering ───────────────────────────────────────────────────────

.rrb_usd <- function(v) {
  v <- ifelse(is.na(v), 0, v)
  ifelse(abs(v) >= 1e6, paste0("$", formatC(v / 1e6, format = "f", digits = 2), "M"),
  ifelse(abs(v) >= 1e3, paste0("$", round(v / 1e3), "K"),
                        paste0("$", round(v))))
}

.rrb_esc <- function(x) html_escape(x)

.rrb_trunc <- function(x, n) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x)

.rrb_text <- function(x, y, s, size = 13, fill = "#111111", weight = 400,
                      anchor = "start", extra = "") {
  sprintf('<text x="%.1f" y="%.1f" font-size="%s" font-weight="%s" fill="%s" text-anchor="%s"%s>%s</text>',
          x, y, size, weight, fill, anchor, extra, .rrb_esc(s))
}

.rrb_rect <- function(x, y, w, h, fill, rx = 0, stroke = NULL) {
  sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%s" fill="%s"%s/>',
          x, y, w, h, rx, fill,
          if (is.null(stroke)) "" else sprintf(' stroke="%s" stroke-width="1"', stroke))
}

.rrb_svg <- function(df, q, plan, max_rows, as_of, basis = "contract_arr") {
  W <- 1280; P <- 32
  carr <- basis == "contract_arr"
  # Wording that depends on what the dollars are.
  amt_lab   <- if (carr) "Contract ARR" else "Renewal"
  churn_def <- if (carr) "contract ARR" else "Churn Forecast"
  ret_def   <- if (carr) "contract ARR" else "renewal minus Churn Forecast"
  INK <- "#111111"; SEC <- "#4B5563"; MUT <- "#6B7280"; LINE <- "#E5E7EB"
  out <- character(0)
  add <- function(...) out <<- c(out, ...)

  # ── Numbers ────────────────────────────────────────────────────────────────
  total    <- sum(df$amount)
  churn    <- sum(df$churn_forecast)
  bad      <- df$adoption %in% c("Black", "Red")
  good     <- df$adoption %in% c("Amber", "Green")
  hidden   <- sum(df$amount[bad & df$risk %in% c("Expected to Renew", "Closed Won")])

  vs_plan <- if (is.na(plan)) "" else {
    d <- churn - plan
    if (abs(d) < 1) ", on plan" else paste0(", ", .rrb_usd(abs(d)), if (d > 0) " over plan" else " under plan")
  }
  headline <- paste0(.rrb_usd(churn), " of ", .rrb_usd(total), " up for renewal is forecast to churn", vs_plan)
  # Second headline line: the risk the forecast doesn't show yet.
  headline2 <- if (hidden > 0) {
    paste0(.rrb_usd(hidden), " expected to renew sits with Black / Red adoption accounts")
  } else if (any(bad)) {
    paste0(.rrb_usd(sum(df$amount[bad])), " of the quarter sits with Black / Red adoption accounts")
  } else ""

  # ── Table rows (optionally capped) ────────────────────────────────────────
  tbl  <- df
  more <- NULL
  if (!is.null(max_rows) && nrow(df) > max_rows) {
    keep <- order(-df$amount)[seq_len(max_rows)]
    tbl  <- df[sort(keep), , drop = FALSE]
    rest <- df[-keep, , drop = FALSE]
    more <- list(n = nrow(rest), amt = sum(rest$amount))
  }

  # ── Geometry ──────────────────────────────────────────────────────────────
  y_kpi  <- 140
  y_body <- y_kpi + 70 + 30
  RH     <- 26
  tbl_x  <- P; tbl_w <- 644
  n_tbl  <- nrow(tbl) + (!is.null(more)) + 1                 # + total row
  tbl_h  <- 34 + n_tbl * RH
  ch_x   <- tbl_x + tbl_w + 36; ch_w <- W - P - ch_x
  ch_h   <- 470                                             # panels + 2-row legend
  H      <- y_body + max(tbl_h, ch_h) + 44                  # + footnote

  add(sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d" font-family="Inter, -apple-system, Helvetica Neue, Helvetica, Arial, sans-serif">',
              W, H, W, H))
  add(.rrb_rect(0, 0, W, H, "#FFFFFF"))

  # ── Header ────────────────────────────────────────────────────────────────
  add(.rrb_text(P, 42, paste0("RENEWAL RISK \u00B7 ", q, "  (", .rrb_quarter_months(q), ")"),
                size = 12, fill = MUT, weight = 600, extra = ' letter-spacing="1"'))
  add(.rrb_text(P, 72, headline, size = 23, weight = 700))
  if (nzchar(headline2)) add(.rrb_text(P, 98, headline2, size = 17, fill = "#991B1B", weight = 600))
  add(.rrb_text(P, 122, paste0(as_of,
                              " \u00B7 churn = ", churn_def, " on Closed Lost, Verbally Confirmed, Will Churn and Expected to Churn",
                              " \u00B7 adoption = product adoption rating"),
                size = 12.5, fill = SEC))

  # ── KPI tiles ─────────────────────────────────────────────────────────────
  kp <- list(
    c("Up for renewal",         .rrb_usd(total),           paste(nrow(df), "renewals")),
    c("Churn forecast",         .rrb_usd(churn),           if (is.na(plan)) "no plan for quarter" else paste("plan", .rrb_usd(plan))),
    c("Black / Red adoption",   .rrb_usd(sum(df$amount[bad])),  paste(sum(bad), "renewals")),
    c("Amber / Green adoption", .rrb_usd(sum(df$amount[good])), paste(sum(good), "renewals"))
  )
  kw <- (W - 2 * P - 3 * 12) / 4
  for (i in seq_along(kp)) {
    x <- P + (i - 1) * (kw + 12)
    add(.rrb_rect(x, y_kpi, kw, 70, "#F7F7F5", rx = 8))
    add(.rrb_text(x + 14, y_kpi + 20, kp[[i]][1], size = 12, fill = SEC))
    add(.rrb_text(x + 14, y_kpi + 46, kp[[i]][2], size = 23, weight = 700))
    add(.rrb_text(x + 14, y_kpi + 62, kp[[i]][3], size = 11.5, fill = MUT))
  }

  # ── Table ─────────────────────────────────────────────────────────────────
  cx <- tbl_x + c(0, 290, 306, 400, 558)         # customer, renewal (right), adoption, risk, close
  add(.rrb_text(tbl_x, y_body + 2, "Renewals in the quarter", size = 14, weight = 600))
  hy <- y_body + 28
  add(.rrb_text(cx[1], hy, "Customer", 12, SEC, 600),
      .rrb_text(cx[2], hy, amt_lab, 12, SEC, 600, "end"),
      .rrb_text(cx[3], hy, "Adoption", 12, SEC, 600),
      .rrb_text(cx[4], hy, "Account risk", 12, SEC, 600),
      .rrb_text(cx[5], hy, "Close date", 12, SEC, 600))
  add(sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#D1D5DB"/>', tbl_x, tbl_x + tbl_w, hy + 8, hy + 8))

  y <- hy + 8
  for (i in seq_len(nrow(tbl))) {
    r  <- tbl[i, ]
    ty <- y + RH / 2 + 4.5
    add(.rrb_text(cx[1], ty, .rrb_trunc(r$account, 34), 12.5))
    add(.rrb_text(cx[2], ty, .rrb_usd(r$amount), 12.5, anchor = "end"))
    add(sprintf('<circle cx="%.1f" cy="%.1f" r="5" fill="%s" stroke="rgba(0,0,0,.15)"/>',
                cx[3] + 5, y + RH / 2, .RRB_ADOPT[[r$adoption]]))
    add(.rrb_text(cx[3] + 16, ty, r$adoption, 12.5))
    rc <- .RRB_RISK_COL[[r$risk]]
    pw <- nchar(r$risk) * 7.2 + 16
    add(.rrb_rect(cx[4], y + 5, pw, RH - 10, rc[1], rx = 4))
    add(.rrb_text(cx[4] + 8, ty - 0.5, r$risk, 11.5, rc[2], 600))
    add(.rrb_text(cx[5], ty, if (is.na(r$close_date)) "\u2014" else format(r$close_date, "%b %d, %Y"), 12.5))
    y <- y + RH
    add(sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="%s"/>', tbl_x, tbl_x + tbl_w, y, y, LINE))
  }
  if (!is.null(more)) {
    ty <- y + RH / 2 + 4.5
    add(.rrb_text(cx[1], ty, paste0("+ ", more$n, " more renewal", if (more$n == 1) "" else "s"), 12.5, MUT))
    add(.rrb_text(cx[2], ty, .rrb_usd(more$amt), 12.5, MUT, anchor = "end"))
    y <- y + RH
    add(sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="%s"/>', tbl_x, tbl_x + tbl_w, y, y, LINE))
  }
  add(sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#D1D5DB"/>', tbl_x, tbl_x + tbl_w, y, y))
  ty <- y + RH / 2 + 4.5
  add(.rrb_text(cx[1], ty, "Total", 12.5, weight = 700),
      .rrb_text(cx[2], ty, .rrb_usd(total), 12.5, weight = 700, anchor = "end"),
      .rrb_text(cx[4], ty, paste(.rrb_usd(churn), "churn forecast"), 12.5, weight = 700))

  # ── Churn / retained panels ───────────────────────────────────────────────
  # Each panel has two bars on one shared scale. Left bar: dollars stacked by
  # Account Risk. Right bar: the same population stacked by adoption rating.
  ch_df <- df[df$risk %in% .RRB_CHURN, , drop = FALSE]
  rt_df <- df[df$risk %in% .RRB_RETAIN, , drop = FALSE]
  adopt_order <- c("Black", "Red", "Amber", "Green", "Unrated")
  adopt_col   <- lapply(stats::setNames(adopt_order, adopt_order), function(a)
    c(.RRB_ADOPT[[a]], if (a %in% c("Black", "Red")) "#FFFFFF" else "#111111"))
  by <- function(d, val, key, levels) {
    vapply(levels, function(l) sum(d[[val]][d[[key]] == l]), numeric(1))
  }
  bars_churn <- list(
    list(lab = c(if (carr) "Contract ARR" else "Renewal ARR", "by account risk"),
         parts = by(ch_df, "amount", "risk", .RRB_CHURN), cols = .RRB_RISK_COL),
    list(lab = c(if (carr) "Churn (ARR)" else "Churn Forecast", "by adoption"),
         parts = by(ch_df, "churn_forecast", "adoption", adopt_order), cols = adopt_col)
  )
  bars_ret <- list(
    list(lab = c("Retained ARR", "by account risk"),
         parts = by(rt_df, "retained", "risk", .RRB_RETAIN), cols = .RRB_RISK_COL),
    list(lab = c("Retained ARR", "by adoption"),
         parts = by(rt_df, "retained", "adoption", adopt_order), cols = adopt_col)
  )

  ymax  <- max(vapply(c(bars_churn, bars_ret), function(b) sum(b$parts), numeric(1)), 1)
  ticks <- pretty(c(0, ymax), n = 4)
  ymax  <- max(ticks)
  TOP <- y_body + 64; B <- y_body + ch_h - 110
  Y <- function(v) B - v / ymax * (B - TOP)
  pw <- (ch_w - 24) / 2

  panel <- function(x0, title, subtitle, bars) {
    add(.rrb_text(x0, y_body + 2, title, 14, weight = 600))
    add(.rrb_text(x0, y_body + 22, subtitle, 12, SEC))
    for (t in ticks) {
      add(sprintf('<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="%s"/>', x0 + 44, x0 + pw, Y(t), Y(t),
                  if (t == 0) "#333333" else "#EEEEEE"))
      add(.rrb_text(x0 + 38, Y(t) + 4, .rrb_usd(t), 11, MUT, anchor = "end"))
    }
    cw <- (pw - 44) / length(bars)
    for (k in seq_along(bars)) {
      bar  <- bars[[k]]
      bx   <- x0 + 44 + (k - 1) * cw + cw * 0.2
      bw   <- cw * 0.6
      base <- 0
      for (nm in names(bar$parts)) {
        v <- bar$parts[[nm]]
        if (v <= 0) next
        rc <- bar$cols[[nm]]
        add(.rrb_rect(bx, Y(base + v), bw, Y(base) - Y(base + v), rc[1], stroke = "#FFFFFF"))
        if (Y(base) - Y(base + v) >= 17) {
          add(.rrb_text(bx + bw / 2, Y(base + v / 2) + 4.5, .rrb_usd(v), 12, rc[2], 600, "middle"))
        }
        base <- base + v
      }
      add(.rrb_text(bx + bw / 2, Y(base) - 8, .rrb_usd(base), 13.5, INK, 700, "middle"))
      add(.rrb_text(bx + bw / 2, B + 19, bar$lab[1], 12.5, INK, 600, "middle"))
      add(.rrb_text(bx + bw / 2, B + 34, bar$lab[2], 11.5, MUT, 400, "middle"))
    }
  }
  panel(ch_x, "Forecast churn",
        if (carr) paste0(.rrb_usd(sum(ch_df$amount)), " contract ARR forecast to churn") else
          paste0(.rrb_usd(sum(ch_df$churn_forecast)), " Churn Forecast on ",
                 .rrb_usd(sum(ch_df$amount)), " ARR"),
        bars_churn)
  add(sprintf('<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="#D1D5DB"/>',
              ch_x + pw + 12, ch_x + pw + 12, y_body - 8, B + 40))
  panel(ch_x + pw + 24, "Forecast retained",
        if (carr) paste0(.rrb_usd(sum(rt_df$retained)), " contract ARR not forecast to churn") else
          paste0(.rrb_usd(sum(rt_df$retained)), " = renewal minus Churn Forecast"),
        bars_ret)

  # ── Legends + footnote ────────────────────────────────────────────────────
  # Account risk squares (wrapping), then adoption dots on their own row.
  lx <- ch_x; ly <- B + 62
  for (rk in setdiff(.RRB_RISK, "Not set")) {
    iw <- 16 + nchar(rk) * 6.6 + 18
    if (lx + iw > W - P) { lx <- ch_x; ly <- ly + 22 }
    add(.rrb_rect(lx, ly - 10, 11, 11, .RRB_RISK_COL[[rk]][1], rx = 2))
    add(.rrb_text(lx + 16, ly, rk, 12, SEC))
    lx <- lx + iw
  }
  lx <- ch_x; ly <- ly + 22
  for (a in adopt_order) {
    lab <- if (a == "Unrated") "Unrated" else paste(a, "adoption")
    add(sprintf('<circle cx="%.1f" cy="%.1f" r="5" fill="%s" stroke="rgba(0,0,0,.15)"/>', lx + 5, ly - 4, .RRB_ADOPT[[a]]))
    add(.rrb_text(lx + 16, ly, lab, 12, SEC))
    lx <- lx + 16 + nchar(lab) * 6.6 + 18
  }

  note <- paste0("Churn = Closed Lost, Verbally Confirmed, Will Churn, Expected to Churn \u00B7 ",
                 "retained = At Risk, Expected to Renew, Closed Won (", ret_def, ") \u00B7 ",
                 "one shared scale",
                 if (carr) " \u00B7 an account with several renewals in the quarter is counted once" else "")
  unset <- df$risk == "Not set"
  if (any(unset)) {
    note <- paste0(note, " \u00B7 ", sum(unset), " renewal", if (sum(unset) == 1) "" else "s",
                   " with no account risk (", .rrb_usd(sum(df$amount[unset])), ") not charted")
  }
  add(.rrb_text(P, H - 18, note, 11.5, MUT))

  add("</svg>")
  paste(out, collapse = "\n")
}

# "Q4-2027" -> "Nov 26 – Jan 27". Fiscal year starts in February and is
# labelled by the calendar year it ends in (see date_to_fiscal_quarter()).
.rrb_quarter_months <- function(q) {
  m <- regmatches(q, regexec("^Q([1-4])-(\\d{4})$", q))[[1]]
  if (length(m) != 3) return("")
  qn <- as.integer(m[2]); fy <- as.integer(m[3])
  start <- as.Date(sprintf("%d-02-01", fy - 1))
  s <- seq(start, by = "month", length.out = 12)[(qn - 1) * 3 + 1]
  e <- seq(s, by = "month", length.out = 3)[3]
  paste(format(s, "%b %y"), "\u2013", format(e, "%b %y"))
}
