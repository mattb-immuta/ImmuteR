#' Generate an HTML dashboard of all account summaries.
#'
#' Produces a self-contained, single-file HTML page listing every account in
#' \code{op_list} in alphabetical order with full detail cards.  The page
#' includes:
#' \itemize{
#'   \item A search box for quick account lookup by name.
#'   \item Region filter pills to narrow the visible set.
#'   \item A-Z alphabet jump bar for instant scrolling.
#'   \item Expandable detail cards showing Account Overview, Architecture,
#'         Open Renewals, and Expansion Pipeline for each account.
#' }
#'
#' @param op_list Named list returned by \code{enrich_opportunities()}, expected
#'   to contain \code{accounts}, \code{renewals}, \code{expansions}, and
#'   \code{active_orders} elements.
#' @param output_path Character. Path where the HTML file will be written.
#'   Parent directories are created automatically.
#'   Defaults to \code{"output/immuta_accounts.html"}.
#' @param guide_path Character. The knitted reader's guide
#'   (\code{\link{report_user_guide}}), embedded in the page's Documentation
#'   tab. Defaults to
#'   \code{immuta_accounts_guide.html} next to \code{output_path}. If it is
#'   missing the page still renders, with a warning, and the tab links to
#'   that file name instead.
#' @param doctrine_path Character. The knitted CS Doctrine
#'   (\code{\link{report_cs_doctrine}}), embedded in the Documentation tab.
#'   Defaults to \code{cs_doctrine.html} next to \code{output_path}; missing is
#'   handled the same way as \code{guide_path}.
#' @param reporting_path Character. The knitted CSM Reporting Requirements
#'   (\code{\link{report_csm_reporting}}), the Documentation tab's CSM
#'   Reporting sub-tab. Defaults to \code{csm_reporting.html} next to
#'   \code{output_path}; missing is handled the same way as \code{guide_path}.
#' @param favicon Character. The browser-tab icon. Either a local image file
#'   (\code{.ico}, \code{.png}, \code{.svg}), which is embedded in the page so
#'   it shows offline and in downloaded copies, or a URL the browser fetches
#'   when the page opens. Defaults to \code{inst/assets/immuta_favicon.svg} (the
#'   Immuta mark, black, white in dark-mode tabs) if that file exists, otherwise Immuta's own
#'   \code{https://www.immuta.com/favicon.ico}. \code{NULL} omits it.
#'
#' @return Invisibly returns \code{output_path}.
#'
#' @examples
#' \dontrun{
#'   report_all_accounts_html(op_list)
#'   report_all_accounts_html(op_list, "reports/accounts.html")
#' }
#' @export
report_all_accounts_html <- function(op_list,
                                     output_path = "output/immuta_accounts.html",
                                     favicon     = .rah_default_favicon(),
                                     guide_path  = file.path(dirname(output_path),
                                                             "immuta_accounts_guide.html"),
                                     doctrine_path = file.path(dirname(output_path),
                                                               "cs_doctrine.html"),
                                     reporting_path = file.path(dirname(output_path),
                                                                "csm_reporting.html")) {

  # Ensure output directory exists
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # The per-account payload the page embeds as `ACCOUNTS`. Built by
  # .rah_account_records() rather than inline, so output_dust_payload() can
  # write byte-identical records to disk — one builder means the Dust frame and
  # this page can never be handed different data.
  accounts        <- op_list$accounts
  account_records <- .rah_account_records(op_list)

  # Emit JSON ------------------------------------------------------------------
  json_data <- .to_json(account_records)

  # Regions for filter pills ---------------------------------------------------
  regions <- sort(unique(accounts$Region))
  regions <- regions[!is.na(regions) & regions != ""]

  # CSMs for filter pills ------------------------------------------------------
  csms <- sort(unique(accounts$`Customer Success Manager`))
  csms <- csms[!is.na(csms) & csms != ""]

  # Lifecycle stages for the Self-Managed tab's filter pills -------------------
  # (scoped to self-managed accounts only, since that's the only tab that uses it)
  self_managed_is_saas <- vapply(accounts$`Is SaaS Customer`, function(v) {
    isTRUE(v %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
  }, logical(1))
  lifecycle_stages <- sort(unique(accounts$`Lifecycle Stage`[!self_managed_is_saas]))
  lifecycle_stages <- lifecycle_stages[!is.na(lifecycle_stages) &
                                       !lifecycle_stages %in% c("", "-")]

  # Snapshot dates for the diff-date picker (newest first) --------------------
  snap_files <- list.files(dirname(output_path),  # look relative to output, fall back to data/
                            pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
  if (length(snap_files) == 0)
    snap_files <- list.files("data/", pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
  snap_dates <- sort(gsub("opportunities_(.+)\\.RData", "\\1", snap_files), decreasing = TRUE)

  # Changes diff JSON ----------------------------------------------------------
  changes_json     <- .build_changes_json(op_list)
  consumption_json <- .build_consumption_json(op_list)
  saas_json        <- .build_saas_json(op_list)
  escalations_json <- .build_escalations_json(op_list)
  new_logos_json   <- .build_new_logos_json(op_list)
  agentic_json     <- .build_agentic_json(op_list)

  # Documentation tab: reader's guide + CS Doctrine ---------------------------
  # Both knitted pages (report_user_guide(), report_cs_doctrine()) are embedded
  # whole, base64-encoded, and shown in iframes on the Documentation tab, so
  # they work in a downloaded copy with nothing else alongside it. A missing
  # page falls back to a relative link, with a warning so the build order gets
  # fixed.
  .embed_b64 <- function(path, what, fn) {
    if (!is.null(path) && file.exists(path)) {
      return(gsub("[\r\n]", "", jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size))))
    }
    warning(what, " not found at ", path, " - run ", fn,
            " before report_all_accounts_html().", call. = FALSE)
    ""
  }
  guide_b64    <- .embed_b64(guide_path,    "Reader's guide", "report_user_guide()")
  doctrine_b64 <- .embed_b64(doctrine_path, "CS Doctrine",    "report_cs_doctrine()")
  reporting_b64 <- .embed_b64(reporting_path, "CSM Reporting", "report_csm_reporting()")

  # Build HTML -----------------------------------------------------------------
  html <- .rah_html(json_data, regions, csms, snap_dates, changes_json, consumption_json,
                    lifecycle_stages, saas_json, escalations_json,
                    new_logos_json = new_logos_json,
                    agentic_json = agentic_json,
                    updated = .op_list_updated(op_list),
                    favicon = favicon,
                    guide_b64 = guide_b64,
                    doctrine_b64 = doctrine_b64,
                    reporting_b64 = reporting_b64)

  writeLines(html, output_path, useBytes = FALSE)
  message(".. wrote account HTML dashboard to ", output_path)
  invisible(output_path)
}


# ---- Internal: the per-account records ---------------------------------------
# Shared by report_all_accounts_html() (which embeds them as the page's
# `ACCOUNTS` array) and output_dust_payload() (which writes them to JSON for a
# Dust frame to render). Presentation-ready rather than raw: currency is
# formatted, missing values are already em-dashes, platforms arrive as HTML.
# Keeping it in one place is what stops the two surfaces drifting apart.

#' One record per account in `op_list$accounts`, sorted by account name.
#' @noRd
.rah_account_records <- function(op_list) {

  # Return the first candidate column present on `row` (a one-row data frame
  # slice), or NULL if none of the candidates exist. Lets us tolerate slightly
  # different Salesforce field labels (e.g. "Next Step" vs "Next Steps")
  # without erroring when a report hasn't been refreshed with the new field yet.
  .first_col <- function(row, candidates) {
    for (cn in candidates) {
      if (cn %in% names(row)) return(row[[cn]])
    }
    NULL
  }

  # Salesforce reports emit a literal "-" for an unset field rather than a
  # true blank/NA. Treat that as missing so it renders as an em-dash instead
  # of a stray hyphen.
  .blank_dash <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    if (!is.na(x) && trimws(as.character(x)) == "-") return(NA)
    x
  }

  # Snapshots saved before product adoption existed lack its columns; score
  # them here (on this local copy only) so re-rendering an old op_list still
  # draws the Adoption page.
  pa <- op_list$consumption$pbac_adoption
  if (is.data.frame(pa) && nrow(pa) > 0 && !("product_adoption" %in% names(pa))) {
    op_list$consumption$pbac_adoption <- op_list_product_adoption(pa, op_list$accounts)
  }

  accounts     <- op_list$accounts
  renewals     <- op_list$renewals
  expansions   <- op_list$expansions
  active_orders <- op_list$active_orders

  # Consumption + support lookups, so the Overview tab can carry the same three
  # blocks the PNG health summary does. Both are optional — an SFDC-only
  # op_list still renders, with em-dashes where the figures would be.
  consumption <- op_list$consumption$low_consumption
  support     <- op_list$support

  # Row lookup by account name (consumption) / account ID (support).
  .lookup_row <- function(df, key_col, key) {
    if (is.null(df) || !is.data.frame(df) || !key_col %in% names(df)) return(NULL)
    hit <- df[!is.na(df[[key_col]]) & df[[key_col]] == key, , drop = FALSE]
    if (nrow(hit) == 0) NULL else hit[1, ]
  }

  .num_or_na <- function(row, col) {
    if (is.null(row) || !col %in% names(row)) return(NA_real_)
    suppressWarnings(as.numeric(row[[col]][1]))
  }

  # Comma-grouped integer, em-dash when missing — matches .tile_int().
  .int_or_dash <- function(x) {
    if (length(x) == 0 || is.na(x)) "—"
    else format(round(x), big.mark = ",", trim = TRUE)
  }

  accounts_sorted <- accounts[order(accounts$`Account Name`), ]

  # Build per-account JSON data ------------------------------------------------
  lapply(seq_len(nrow(accounts_sorted)), function(i) {

    acct    <- accounts_sorted[i, ]
    acct_id <- acct$`Account ID`
    aname   <- acct$`Account Name`

    # --- Renewals ---
    ren_list <- list()
    if (!is.null(renewals) && is.data.frame(renewals) && nrow(renewals) > 0) {
      acct_ren <- renewals[!is.na(renewals$`Account ID`) &
                           renewals$`Account ID` == acct_id, ]
      if (nrow(acct_ren) > 0) {
        acct_ren <- acct_ren[order(as.Date(acct_ren$`Current Contract End Date`)), ]
        ren_list <- lapply(seq_len(nrow(acct_ren)), function(j) {
          r <- acct_ren[j, ]
          list(
            name          = safe_val(r[["Opportunity Name"]]),
            url           = safe_val(r[["opportunity_url"]]),
            amount        = fmt_usd(r[["Amount"]]),
            amount_raw    = suppressWarnings(as.numeric(r[["Amount"]])),
            end_date      = fmt_date(r[["Current Contract End Date"]]),
            end_date_raw  = tryCatch(format(as.Date(r[["Current Contract End Date"]]), "%Y-%m-%d"), error = function(e) ""),
            churn_quarter = safe_val(r[["Churn Quarter"]]),
            risk          = safe_val(r[["Account Risk"]]),
            next_steps    = safe_val(.first_col(r, c("Next Step", "Next Steps")))
          )
        })
      }
      # churn summary for card header chip
      qs <- acct_ren$`Churn Quarter`
    } else {
      acct_ren <- data.frame()
      qs       <- character(0)
    }

    # --- Expansions ---
    exp_list <- list()
    if (!is.null(expansions) && is.data.frame(expansions) && nrow(expansions) > 0) {
      acct_exp <- expansions[!is.na(expansions$`Account ID`) &
                             expansions$`Account ID` == acct_id, ]
      if (nrow(acct_exp) > 0) {
        acct_exp <- acct_exp[order(as.Date(acct_exp$`Close Date`)), ]
        exp_list <- lapply(seq_len(nrow(acct_exp)), function(j) {
          e <- acct_exp[j, ]
          list(
            name             = safe_val(e[["Opportunity Name"]]),
            url              = safe_val(e[["opportunity_url"]]),
            owner            = safe_val(e[["Opportunity Owner"]]),
            amount           = fmt_usd(e[["Amount"]]),
            amount_raw       = suppressWarnings(as.numeric(e[["Amount"]])),
            fiscal_period    = safe_val(e[["Fiscal Period"]]),
            close_date       = fmt_date(e[["Close Date"]]),
            close_date_raw   = tryCatch(format(as.Date(e[["Close Date"]]), "%Y-%m-%d"), error = function(e) ""),
            next_steps       = safe_val(.first_col(e, c("Next Step", "Next Steps")))
          )
        })
      }
    }

    # --- Products ---
    prod_list <- list()
    if (!is.null(active_orders) &&
        is.data.frame(active_orders) &&
        nrow(active_orders) > 0 &&
        "Account Name: Account Name" %in% names(active_orders)) {
      ord <- active_orders[
        !is.na(active_orders$`Account Name: Account Name`) &
        active_orders$`Account Name: Account Name` == aname, ]
      if (!is.null(ord) && nrow(ord) > 0) {
        prods <- ord[order(ord$`Product: Product Name`), ]
        prod_list <- lapply(seq_len(nrow(prods)), function(j) {
          list(
            name     = safe_val(prods[j, "Product: Product Name"]),
            quantity = safe_val(prods[j, "Quantity"])
          )
        })
      }
    }

    # --- SaaS / version ---
    is_saas <- isTRUE(acct$`Is SaaS Customer` %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
    unsupported <- if (!is_saas) {
      isTRUE(acct$`Customer on Unsupported Version` %in%
               c(TRUE, "Yes", "TRUE", "true", "yes"))
    } else NA

    # --- Churn quarters summary (for card header chip) ---
    # qs was populated inside the renewals block above
    qs <- qs[!is.na(qs) & qs != ""]
    churn_summary <- if (length(qs) == 0) "" else paste(qs, collapse = ", ")

    # --- Consumption + support rows for the health-summary blocks ---
    c_row <- .lookup_row(consumption, "Account Name", aname)
    s_row <- .lookup_row(support,     "Account Name", aname)
    p_row <- pbac_row(op_list, aname)   # op_list$consumption$pbac_adoption

    reg_raw <- .num_or_na(c_row, "Registered Users")
    lic_raw <- .num_or_na(c_row, "Licensed Users")
    rat_raw <- .num_or_na(c_row, "Consumption Ratio")
    # consumption_ratio() applies the ELA rule (Licensed Users of 0 is
    # unlimited seats, so the ratio is 1) — the same helper every other caller
    # uses, so an account cannot show two different ratios in two places.
    if (is.na(rat_raw)) rat_raw <- consumption_ratio(reg_raw, lic_raw)

    list(
      id            = gsub("[^A-Za-z0-9]", "_", aname),
      name          = aname,
      account_id    = acct_id,
      customer_central_url = {
        cc <- acct$customer_central_url
        if (!is.null(cc) && !is.na(cc) && nchar(as.character(cc)) > 0) as.character(cc) else ""
      },
      region        = safe_val(acct$Region),
      arr           = fmt_usd(acct$`Contract ARR Rollup`),
      arr_raw       = suppressWarnings(as.numeric(acct$`Contract ARR Rollup`)),
      # Tenure from the earliest contract start (op_list_customer_tenure()).
      # Snapshots built before tenure existed lack the columns -> NA / em-dash.
      months_as_customer = if (is.null(acct$months_as_customer)) NA_real_ else
                             suppressWarnings(as.numeric(acct$months_as_customer[1])),
      customer_since     = fmt_date(acct$customer_since),
      # TRUE when the account has an agentic PoV (op_list_agentic_pov()): an OBO revenue target.
      obo_revenue_tgt    = isTRUE(acct$OBO_revenue_TGT[1]),
      csm           = safe_val(acct$`Customer Success Manager`),
      account_owner = safe_val(acct$`Account Owner`),
      field_cto        = safe_val(acct$`Field CTO`),
      consumption_link = {
        curl <- acct$consumption_url
        if (!is.null(curl) && !is.na(curl) && nchar(as.character(curl)) > 0) {
          paste0('<a class="consumption-link" href="', html_escape(as.character(curl)),
                 '" target="_blank" rel="noopener"',
                 ' onclick="event.stopPropagation()">Current Consumption &#8599;</a>')
        } else ""
      },
      is_saas       = is_saas,
      unsupported   = isTRUE(unsupported),
      churn_summary = churn_summary,
      lifecycle_stage = safe_val(.blank_dash(.first_col(acct, c("Lifecycle Stage")))),
      # PBAC is rendered from the Looker-derived rating rather than the
      # Salesforce picklist; the picklist is still sent so the card can fall
      # back to it on a looker = FALSE run.
      pbac_adoption   = safe_val(.blank_dash(.first_col(acct, c("PBAC Adoption")))),
      pbac_rating     = safe_val(if (is.null(p_row)) NA else p_row$pbac_adoption[1]),
      # The weighted complexity score behind the rating — the Adoption tab's
      # PBAC scatter plots it on y, against consumption_ratio_raw on x.
      pbac_score      = if (is.null(p_row)) NA_real_ else
                          suppressWarnings(as.numeric(p_row$complexity_score[1])),
      # Product adoption = pbac_score + Request + Comply points (op_list_product_adoption()).
      # The Adoption page scatter plots it on y and colours by its rating.
      product_adoption = if (is.null(p_row) || !("product_adoption" %in% names(p_row))) NA_real_ else
                           suppressWarnings(as.numeric(p_row$product_adoption[1])),
      product_rating   = safe_val(if (is.null(p_row) || !("product_adoption_rating" %in% names(p_row))) NA
                                  else p_row$product_adoption_rating[1]),
      request_points   = if (is.null(p_row) || !("request_points" %in% names(p_row))) NA_real_ else
                           suppressWarnings(as.numeric(p_row$request_points[1])),
      comply_points    = if (is.null(p_row) || !("comply_points" %in% names(p_row))) NA_real_ else
                           suppressWarnings(as.numeric(p_row$comply_points[1])),
      pbac_features   = {
        f <- pbac_feature_labels(p_row)
        if (length(f) == 0) "" else paste(f, collapse = " · ")
      },
      comply_adoption = safe_val(.blank_dash(.first_col(acct, c("Comply Adoption")))),
      request_adoption = safe_val(.blank_dash(.first_col(acct, c("Request Adoption")))),
      # Request is rendered from the Looker stage (op_list_request_adoption(),
      # dashboard 1009) — present only when the pipeline ran with looker = TRUE.
      request_adoption_derived = safe_val(.blank_dash(.first_col(acct, c("request_adoption_looker")))),
      request_status           = safe_val(.blank_dash(.first_col(acct, c("request_adoption_looker_stage")))),
      agentic_governance_adoption = safe_val(.blank_dash(.first_col(acct, c("Agentic Governance Adoption")))),
      # Agentic profile, from the account report (fallback for Adoption > Agentic Engagement Overview).
      # Em-dash when blank or when the report predates the fields.
      agentic_owner        = safe_val(.blank_dash(.first_col(acct, c("Agentic Owner - Customer",
                                                                     "Agentic Owner")))),
      agentic_owner_title  = safe_val(.blank_dash(.first_col(acct, c("Title",
                                                                     "Agentic Owner - Customer: Title")))),
      agentic_vision       = safe_val(.blank_dash(.first_col(acct, c("Customer Agentic Vision")))),
      agentic_architecture = safe_val(.blank_dash(.first_col(acct, c("Agentic Architecture")))),

      # --- Health-summary blocks, mirroring generate_slide_tile() ---
      platform_html = {
        ph <- if (!is.null(c_row) && "platform_html" %in% names(c_row))
          c_row[["platform_html"]] else NA_character_
        if (is.na(ph) || length(ph) == 0) "—" else as.character(ph)
      },
      multiplatform = isTRUE(acct$multiplatform),
      # From data/input/critical_technologies.json via enrich_opportunities().
      # "" (not an em-dash) when absent, so the PBAC block can show "None".
      iam         = { v <- .first_col(acct, c("iam"))
                      if (is.null(v) || is.na(v[1]) || !nzchar(v[1])) "" else as.character(v[1]) },
      ext_catalog = { v <- .first_col(acct, c("ext_catalog"))
                      if (is.null(v) || is.na(v[1]) || !nzchar(v[1])) "" else as.character(v[1]) },

      # --- Health-summary blocks, mirroring generate_slide_tile() ---
      registered_users = .int_or_dash(reg_raw),
      licensed_users   = .int_or_dash(lic_raw),
      consumption_ratio = if (is.na(rat_raw)) "—" else paste0(round(rat_raw * 1000) / 10, "%"),
      consumption_ratio_raw = rat_raw,
      immuta_version   = safe_val(.blank_dash(
        if (!is.null(c_row)) c_row[["Immuta Version"]] else NA)),
      inc_90d          = .int_or_dash(.num_or_na(s_row, "cases_90d")),
      inc_360d         = .int_or_dash(.num_or_na(s_row, "cases_360d")),
      custx_90d        = .int_or_dash(.num_or_na(s_row, "escalations_90d")),
      custx_open       = .int_or_dash(.num_or_na(s_row, "escalations_open")),
      custx_90d_raw    = .num_or_na(s_row, "escalations_90d"),
      custx_open_raw   = .num_or_na(s_row, "escalations_open"),
      # Connections migration, joined onto low_consumption by
      # op_list_consumption() from dashboard 995. Already a display string
      # ("12 / 40", "None", or several comma-separated) — the ratio beside it
      # is the sortable form.
      connections      = safe_val(.blank_dash(
        if (!is.null(c_row) && "connections" %in% names(c_row)) c_row[["connections"]] else NA)),
      connections_raw  = .num_or_na(c_row, "connections_ratio"),
      # Contact roles. Two roles, the same three fields each: who they are,
      # how strong the relationship is, how much influence they carry. Named
      # by role rather than leaving the buyer's pair unprefixed, so neither
      # reads as "the account's" relationship.
      #
      # Names come from the contacts report (op_list$contacts), rolled up onto
      # accounts as `champion` / `executive_buyer` by enrich_opportunities();
      # Relationship / Influence are fields on the account report itself.
      # .first_col() returns NULL for a missing column, so an older snapshot
      # still renders — with em-dashes.
      executive_buyer  = safe_val(.blank_dash(.first_col(acct,
                           c("executive_buyer", "Executive Buyer")))),
      buyer_relationship = safe_val(.blank_dash(.first_col(acct,
                           c("Executive Buyer Relationship")))),
      buyer_influence  = safe_val(.blank_dash(.first_col(acct,
                           c("Executive Buyer Influence")))),

      champion         = safe_val(.blank_dash(.first_col(acct,
                           c("champion", "Champion")))),
      champion_relationship = safe_val(.blank_dash(.first_col(acct,
                           c("Champion Relationship")))),
      champion_influence = safe_val(.blank_dash(.first_col(acct,
                           c("Champion Influence")))),

      last_qbr         = fmt_date(.first_col(acct, c("Date Last QBR"))),
      next_qbr         = fmt_date(.first_col(acct, c("Date Next QBR"))),
      qbr_note         = safe_val(.blank_dash(.first_col(acct, c("QBR Note")))),

      renewals      = ren_list,
      expansions    = exp_list,
      products      = prod_list
    )
  })
}


# ---- Internal: minimal JSON serialiser (no jsonlite dependency) -------------

.to_json <- function(x) {
  if (is.null(x)) return("null")

  # ── logical ──────────────────────────────────────────────────────────────────
  if (is.logical(x) && length(x) == 1)
    return(if (is.na(x)) "null" else if (isTRUE(x)) "true" else "false")
  if (is.logical(x))
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))

  # ── numeric — use | not || so vectors don't error ────────────────────────────
  if (is.numeric(x) && length(x) == 1)
    return(if (is.na(x) | is.nan(x) | is.infinite(x)) "null" else as.character(x))
  if (is.numeric(x))
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))

  # ── character ─────────────────────────────────────────────────────────────────
  if (is.character(x) && length(x) == 1) {
    if (is.na(x)) return("null")
    x <- gsub("\\\\", "\\\\\\\\", x)
    x <- gsub('"',  '\\\\"',  x)
    x <- gsub('\n', '\\\\n', x)
    x <- gsub('\r', '\\\\r', x)
    x <- gsub('\t', '\\\\t', x)
    return(paste0('"', x, '"'))
  }
  if (is.character(x))
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))

  # ── data.frame — serialize as array of row objects ───────────────────────────
  # Must come before is.list() since data frames are also lists.
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      row <- as.list(x[i, , drop = FALSE])
      # as.list on a 1-row data frame gives 1-element lists per column; unwrap them
      row <- lapply(row, function(v) if (is.list(v)) v[[1L]] else v)
      pairs <- mapply(function(k, v) paste0('"', k, '":', .to_json(v)),
                      names(row), row, SIMPLIFY = TRUE)
      paste0("{", paste(pairs, collapse = ","), "}")
    })
    return(paste0("[", paste(rows, collapse = ","), "]"))
  }

  # ── list ──────────────────────────────────────────────────────────────────────
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms)) {
      pairs <- mapply(function(k, v) paste0('"', k, '":', .to_json(v)),
                      nms, x, SIMPLIFY = TRUE)
      return(paste0("{", paste(pairs, collapse = ","), "}"))
    } else {
      return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","), "]"))
    }
  }

  '"?"'
}


# ---- Internal: convert a data frame to a list of row objects ---------------

.df_to_rows <- function(df, cols = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(list())
  if (!is.null(cols)) df <- df[, intersect(cols, names(df)), drop = FALSE]
  if (ncol(df) == 0)  return(list())
  lapply(seq_len(nrow(df)), function(i) {
    row <- lapply(names(df), function(nm) {
      v <- df[[nm]][i]
      if (is.factor(v))        return(as.character(v))
      if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
      if (is.logical(v))       return(isTRUE(v))
      if (is.numeric(v))       return(if (is.na(v)) NULL else as.numeric(v))
      s <- tryCatch(as.character(v), error = function(e) "")
      if (length(s) == 0 || is.na(s) || s == "NA") return(NULL)
      s
    })
    names(row) <- names(df)
    row
  })
}


# ---- Internal: build the changes JSON blob from op_list --------------------

.build_changes_json <- function(op_list) {
  cd <- op_list[["changes_detail"]]
  cs <- op_list[["changes"]]          # summary data frame

  if (is.null(cd)) {
    return(.to_json(list(has_data = FALSE, diff_date = "", summary = list())))
  }

  diff_date_str <- tryCatch(
    format(as.Date(cd$diff_date), "%Y-%m-%d"),
    error = function(e) as.character(cd$diff_date)
  )

  .to_json(list(
    has_data      = TRUE,
    diff_date     = diff_date_str,
    summary       = .df_to_rows(cs),
    accounts      = list(
      added         = .df_to_rows(cd$accounts$added,
                        c("Account ID", "Account Name", "Region",
                          "Contract ARR Rollup", "Customer Success Manager")),
      removed       = .df_to_rows(cd$accounts$removed,
                        c("Account ID", "Account Name", "Region",
                          "Contract ARR Rollup", "Customer Success Manager")),
      field_changes = .df_to_rows(cd$accounts$field_changes,
                        c("Account Name",
                          "Contract ARR Rollup.new",      "Contract ARR Rollup.old",
                          "Customer Success Manager.new", "Customer Success Manager.old",
                          "Account Owner.new",            "Account Owner.old",
                          "Field CTO.new",                "Field CTO.old",
                          "Region.new",                   "Region.old",
                          "Adoption Phase.new",           "Adoption Phase.old",
                          "request_adoption_looker_stage.new",
                          "request_adoption_looker_stage.old",
                          "comply_adoption_looker_stage.new",
                          "comply_adoption_looker_stage.old"))
    ),
    renewals      = list(
      added         = .df_to_rows(cd$renewals$added,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Close Date", "Account Risk")),
      removed       = .df_to_rows(cd$renewals$removed,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Close Date", "Account Risk")),
      field_changes = .df_to_rows(cd$renewals$field_changes,
                        c("Opportunity Name", "Account Name",
                          "Account Risk.new",    "Account Risk.old",
                          "Churn Forecast.new",  "Churn Forecast.old",
                          "Amount.new",          "Amount.old",
                          "Close Date.new",      "Close Date.old",
                          "risk_changed", "forecast_changed",
                          "amount_changed", "date_changed", "forecast_delta"))
    ),
    expansions    = list(
      added         = .df_to_rows(cd$expansions$added,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Fiscal Period", "Close Date", "Opportunity Owner")),
      removed       = .df_to_rows(cd$expansions$removed,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Fiscal Period", "Close Date", "Opportunity Owner")),
      field_changes = .df_to_rows(cd$expansions$field_changes,
                        c("Opportunity Name", "Account Name",
                          "Amount.new", "Amount.old",
                          "Fiscal Period.new", "Fiscal Period.old",
                          "amount_changed", "period_changed"))
    ),
    new_logos     = list(
      added         = .df_to_rows(cd$new_logos$added,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Fiscal Period", "Close Date", "Opportunity Owner")),
      removed       = .df_to_rows(cd$new_logos$removed,
                        c("Opportunity Name", "Account Name", "Amount",
                          "Fiscal Period", "Close Date", "Opportunity Owner")),
      field_changes = .df_to_rows(cd$new_logos$field_changes,
                        c("Opportunity Name", "Account Name",
                          "Amount.new", "Amount.old",
                          "Fiscal Period.new", "Fiscal Period.old",
                          "amount_changed", "period_changed"))
    ),
    csm_table     = list(
      added         = .df_to_rows(cd$csm_table$added),
      removed       = .df_to_rows(cd$csm_table$removed),
      reassignments = .df_to_rows(cd$csm_table$reassignments)
    ),
    saas_pipeline = list(
      added         = .df_to_rows(cd$saas_pipeline$added),
      removed       = .df_to_rows(cd$saas_pipeline$removed),
      field_changes = .df_to_rows(cd$saas_pipeline$field_changes)
    ),
    # Keyed on the Jira issue key, so "added" is an escalation raised since the
    # last run and "removed" is one that closed. The column lists are explicit
    # because the escalations frame carries fourteen columns and the Changes
    # tab only wants the triage-relevant ones.
    escalations   = list(
      added         = .df_to_rows(cd$escalations$added,
                        c("key", "Account Name", "severity", "status",
                          "age_days", "summary", "url")),
      removed       = .df_to_rows(cd$escalations$removed,
                        c("key", "Account Name", "severity", "status",
                          "age_days", "summary", "url")),
      field_changes = .df_to_rows(cd$escalations$field_changes,
                        c("key", "Account Name",
                          "severity.new", "severity.old",
                          "status.new",   "status.old"))
    ),
    forecast      = list(
      added         = .df_to_rows(cd$forecast$added,
                        c("Opportunity Name", "Account Name", "Churn Quarter",
                          "Churn Forecast", "Amount", "Account Risk", "Stage",
                          "Close Date")),
      removed       = .df_to_rows(cd$forecast$removed,
                        c("Opportunity Name", "Account Name", "Churn Quarter",
                          "Churn Forecast", "Amount", "Account Risk", "Stage",
                          "Close Date")),
      field_changes = .df_to_rows(cd$forecast$field_changes,
                        c("Opportunity Name", "Account Name",
                          "Churn Quarter.new",   "Churn Quarter.old",
                          "Churn Forecast.new",  "Churn Forecast.old",
                          "Account Risk.new",    "Account Risk.old",
                          "Amount.new",          "Amount.old",
                          "Stage.new",           "Stage.old",
                          "Close Date.new",      "Close Date.old",
                          "quarter_changed", "forecast_changed", "risk_changed",
                          "amount_changed", "stage_changed", "date_changed",
                          "forecast_delta"))
    ),
    # Agentic engagement (R/diff_agentic.R); NULL until both snapshots have it.
    agentic       = if (is.null(cd$agentic)) NULL else list(
      added         = .df_to_rows(cd$agentic$added),
      removed       = .df_to_rows(cd$agentic$removed),
      field_changes = .df_to_rows(cd$agentic$field_changes)
    )
  ))
}


.build_consumption_json <- function(op_list) {
  con <- op_list$consumption
  cd  <- op_list[["changes_detail"]]

  has_data <- !is.null(con) && !is.null(con$low_consumption) && nrow(con$low_consumption) > 0

  diff_date_str <- if (!is.null(cd)) {
    tryCatch(format(as.Date(cd$diff_date), "%Y-%m-%d"),
             error = function(e) as.character(cd$diff_date))
  } else {
    ""
  }

  rows_df <- if (has_data) con$low_consumption else NULL

  # Merge in the per-account "Changes in ..." deltas (every account present in
  # both snapshots, not just the ones flagged in cd$consumption$field_changes)
  # so the main Consumption tab table can show them as ordinary columns.
  deltas <- cd$consumption$all_deltas
  if (!is.null(rows_df) && !is.null(deltas) && nrow(deltas) > 0) {
    key <- if ("Account ID" %in% names(deltas)) "Account ID" else "Account Name"
    if (key %in% names(rows_df)) {
      rows_df <- merge(rows_df, deltas, by = key, all.x = TRUE)
    }
  }

  .to_json(list(
    has_data  = has_data,
    rows      = if (!is.null(rows_df)) .df_to_rows(rows_df) else list(),
    diff_date = diff_date_str,
    changes   = list(
      field_changes = .df_to_rows(cd$consumption$field_changes)
    )
  ))
}

.build_escalations_json <- function(op_list) {
  es <- op_list$escalations
  if (is.null(es) || !is.data.frame(es) || nrow(es) == 0) {
    return(.to_json(list(has_data = FALSE, rows = list())))
  }
  # op_list_escalations() already joined Region and CSM and computed age_days
  # and severity_rank, so this is a straight pass-through.
  .to_json(list(has_data = TRUE, rows = .df_to_rows(es)))
}

.build_saas_json <- function(op_list) {
  sp <- op_list$saas_pipeline
  if (is.null(sp) || !is.data.frame(sp) || nrow(sp) == 0) {
    return(.to_json(list(has_data = FALSE, rows = list())))
  }

  # The report carries Region but not CSM, so join it from accounts. Without it
  # the CSM facet would silently show nothing on this tab while working on every
  # other one — worse than not offering the facet at all.
  accts <- op_list$accounts
  if (!is.null(accts) && all(c("Account Name", "Customer Success Manager") %in% names(accts))) {
    idx <- match(trimws(as.character(sp$`Account Name`)),
                 trimws(as.character(accts$`Account Name`)))
    sp$csm <- as.character(accts$`Customer Success Manager`)[idx]
  } else {
    sp$csm <- NA_character_
  }

  .to_json(list(has_data = TRUE, rows = .df_to_rows(sp)))
}

# New logo (New Business) opportunities — op_list$new_logos. These sit on
# prospects rather than customer accounts, so they can't hang off ACCOUNTS the
# way renewals/expansions do; the page gets its own presentation-ready rows.
.build_new_logos_json <- function(op_list) {
  nl <- op_list$new_logos
  if (is.null(nl) || !is.data.frame(nl) || nrow(nl) == 0) {
    return(.to_json(list(has_data = !is.null(nl), rows = list())))
  }
  col <- function(df, nm) if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))

  nl <- nl[order(suppressWarnings(as.Date(col(nl, "Close Date")))), , drop = FALSE]
  rows <- lapply(seq_len(nrow(nl)), function(i) {
    r    <- nl[i, , drop = FALSE]
    prob <- suppressWarnings(as.numeric(col(r, "Probability (%)")))
    list(
      account        = safe_val(col(r, "Account Name")),
      name           = safe_val(col(r, "Opportunity Name")),
      url            = safe_val(col(r, "opportunity_url")),
      owner          = safe_val(col(r, "Opportunity Owner")),
      region         = safe_val(col(r, "Region")),
      stage          = safe_val(col(r, "Stage")),
      probability    = if (length(prob) == 0 || is.na(prob)) "—" else paste0(round(prob * 100), "%"),
      amount         = fmt_usd(col(r, "Amount")),
      amount_raw     = suppressWarnings(as.numeric(col(r, "Amount"))),
      fiscal_period  = safe_val(as.character(col(r, "Fiscal Period"))),
      close_date     = fmt_date(col(r, "Close Date")),
      close_date_raw = tryCatch(format(as.Date(col(r, "Close Date")), "%Y-%m-%d"),
                                error = function(e) ""),
      next_steps     = safe_val(if ("Next Step" %in% names(r)) r[["Next Step"]] else col(r, "Next Steps"))
    )
  })
  .to_json(list(has_data = TRUE, rows = rows))
}

# Agentic profile (op_list$agentic_profile, the raw 00OVV000007LHjN2AW rows)
# rolled up one row per account by .ao_build() — the same builder as
# report_agentic_owner_html(), so both pages count the same accounts. Churned
# accounts (Account Risk Verbally Confirmed / Will Churn) go to `removed`.
# has_data = FALSE (an op_list built before the pull existed, or a failed
# pull) makes the tab fall back to the four agentic fields on ACCOUNTS.
.build_agentic_json <- function(op_list,
                                exclude_risk = c("Verbally Confirmed", "Will Churn")) {
  ap <- op_list$agentic_profile
  if (!is.data.frame(ap) || nrow(ap) == 0) return(.to_json(list(has_data = FALSE)))
  built <- tryCatch(
    .ao_build(ap, op_list = op_list, exclude_risk = exclude_risk),
    error = function(e) {
      warning("Agentic Engagement Overview tab: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  if (is.null(built)) return(.to_json(list(has_data = FALSE)))
  .agx_json(built, exclude_risk, povs = op_list$agentic_pov,
            delta = op_list$changes_detail$agentic)
}

# .ao_build() output -> the AGENTIC_DATA payload the shared Agentic Engagement
# Overview script reads (R/agentic_overview_assets.R). Also used by
# report_agentic_owner_html().
.agx_json <- function(built, exclude_risk, povs = NULL, delta = NULL) {
  # delta = op_list$changes_detail$agentic (.diff_agentic()): the previous
  # rollup and what changed, for the deltas on the page's roll-up numbers.
  has_delta <- is.list(delta) && is.data.frame(delta$prev)
  j <- .to_json(list(
    has_data = TRUE,
    exclude  = if (length(exclude_risk)) paste(exclude_risk, collapse = " or ") else "",
    rows     = .df_to_rows(built$data),
    removed  = .df_to_rows(built$removed),
    # Agentic PoV projects (op_list$agentic_pov), joined to rows in the page by account.
    povs     = if (is.data.frame(povs)) .df_to_rows(povs) else list(),
    prev_date = if (has_delta) delta$prev_date else NULL,
    prev      = if (has_delta) .df_to_rows(delta$prev) else NULL,
    prev_povs = if (has_delta && is.data.frame(delta$prev_povs)) .df_to_rows(delta$prev_povs) else NULL,
    changes   = if (has_delta) list(added = .df_to_rows(delta$added), removed = .df_to_rows(delta$removed),
                                    field_changes = .df_to_rows(delta$field_changes)) else NULL
  ))
  # Free text from Salesforce must not close the page's <script> block.
  gsub("</", "<\\/", j, fixed = TRUE)
}

# ---- Internal: full HTML page template -------------------------------------

.rah_html <- function(json_data, regions, csms = character(0), snap_dates = character(0), changes_json = "null", consumption_json = "null", lifecycle_stages = character(0), saas_json = "null", escalations_json = "null", new_logos_json = "null", agentic_json = "null", updated = "", favicon = NULL, guide_b64 = "", doctrine_b64 = "", reporting_b64 = "") {

  region_pills <- paste(
    vapply(regions, function(r) {
      re <- html_escape(r)
      paste0('<button class="pill" data-region="', re, '">', re, '</button>')
    }, character(1)),
    collapse = "\n    "
  )

  csm_pills <- paste(
    vapply(csms, function(cm) {
      ce <- html_escape(cm)
      paste0('<button class="pill" data-csm="', ce, '">', ce, '</button>')
    }, character(1)),
    collapse = "\n    "
  )

  sm_lifecycle_pills <- paste(
    vapply(lifecycle_stages, function(l) {
      le <- html_escape(l)
      paste0('<button class="flat-pill" data-sm-lifecycle="', le,
             '" onclick="setSMLifecycle(\'', le, '\')">', le, '</button>')
    }, character(1)),
    collapse = "\n    "
  )

  tmpl <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Immuta Customer 360</title>
{{FAVICON}}
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap" rel="stylesheet">
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

  :root {
    /* ── Immuta brand palette (from immuta.com) ── */
    --im-black:    #0D0D0D;   /* near-black — topbar / hero bg */
    --im-lime:     #C8F135;   /* neon chartreuse — THE brand accent */
    --im-lime-dk:  #A8CC18;   /* darker lime for hover */
    --im-lime-lt:  #F5FCD8;   /* pale lime tint — soft backgrounds */
    --im-white:    #FFFFFF;

    /* ── Semantic colors ── */
    --amber:       #F59E0B;
    --amber-lt:    #FEF3C7;
    --red:         #EF4444;
    --red-lt:      #FEE2E2;

    /* ── Neutrals ── */
    --text-1:      #111111;   /* primary text */
    --text-2:      #555555;   /* secondary */
    --text-3:      #999999;   /* muted / labels */
    --border:      #E5E5E5;
    --surface:     #F8F8F6;   /* warm off-white page bg */
    --surface-2:   #F0F0EE;   /* slightly deeper for hover */

    /* ── Tokens ── */
    --radius:      10px;
    --radius-sm:   6px;
    --shadow:      0 1px 3px rgba(0,0,0,.06), 0 4px 12px rgba(0,0,0,.06);
    --shadow-lg:   0 8px 28px rgba(0,0,0,.12);
    --sidebar:     264px;
    --topbar-h:    60px;
    --tabs-h:      48px;
    --font:        "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }

  body {
    font-family: var(--font);
    font-size: 14px;
    line-height: 1.5;
    color: var(--text-1);
    background: var(--surface);
    display: flex;
    flex-direction: column;
    min-height: 100vh;
    -webkit-font-smoothing: antialiased;
  }

  /* ── Top bar ──────────────────────────────────────────────────────────── */
  .topbar {
    position: sticky;
    top: 0;
    z-index: 100;
    background: var(--im-black);
    color: var(--im-white);
    padding: 0 24px;
    height: var(--topbar-h);
    display: flex;
    align-items: center;
    gap: 20px;
  }

  .topbar-logo {
    display: flex;
    align-items: center;
    gap: 14px;
    text-decoration: none;
    flex-shrink: 0;
  }
  .topbar-logo svg { display: block; flex-shrink: 0; }
  .topbar-section {
    font-size: 13px;
    font-weight: 400;
    color: rgba(255,255,255,.4);
    padding-left: 14px;
    border-left: 1px solid rgba(255,255,255,.15);
  }

  .search-wrap {
    flex: 1;
    max-width: 420px;
    position: relative;
    margin-left: 4px;
  }
  .search-wrap input {
    width: 100%;
    padding: 8px 14px 8px 38px;
    border: 1px solid rgba(255,255,255,.12);
    border-radius: var(--radius-sm);
    font-size: 13px;
    font-family: var(--font);
    background: rgba(255,255,255,.06);
    color: var(--im-white);
    outline: none;
    transition: background .15s, border-color .15s;
  }
  .search-wrap input::placeholder { color: rgba(255,255,255,.3); }
  .search-wrap input:focus {
    background: rgba(255,255,255,.1);
    border-color: var(--im-lime);
  }
  .search-icon {
    position: absolute;
    left: 11px;
    top: 50%;
    transform: translateY(-50%);
    color: rgba(255,255,255,.3);
    font-size: 14px;
    pointer-events: none;
  }
  .count-badge {
    font-size: 12px;
    color: rgba(255,255,255,.38);
    white-space: nowrap;
    margin-left: auto;
  }

  /* ── Layout ───────────────────────────────────────────────────────────── */
  .layout { display: flex; flex: 1; }

  /* ── Sidebar ──────────────────────────────────────────────────────────── */
  .sidebar {
    width: var(--sidebar);
    flex-shrink: 0;
    background: var(--im-white);
    border-right: 1px solid var(--border);
    position: sticky;
    top: calc(var(--topbar-h) + var(--tabs-h));
    height: calc(100vh - var(--topbar-h) - var(--tabs-h));
    overflow-y: auto;
    padding: 20px 0 40px;
    scrollbar-width: thin;
    scrollbar-color: var(--border) transparent;
  }
  .sidebar-section { margin-bottom: 28px; }
  .sidebar-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    color: var(--text-3);
    padding: 0 18px 10px;
  }
  .region-filters {
    display: flex;
    flex-direction: column;
    gap: 1px;
    padding: 0 10px;
  }
  .pill {
    text-align: left;
    padding: 7px 10px;
    border-radius: var(--radius-sm);
    border: none;
    background: transparent;
    font-size: 13px;
    font-family: var(--font);
    color: var(--text-2);
    cursor: pointer;
    transition: background .12s, color .12s;
  }
  .pill:hover { background: var(--im-lime-lt); color: var(--text-1); }
  .pill.active {
    background: var(--im-black);
    color: var(--im-lime);
    font-weight: 600;
  }

  .alpha-jump {
    display: flex;
    flex-wrap: wrap;
    gap: 2px;
    padding: 0 14px;
  }
  .alpha-jump a {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 25px;
    height: 25px;
    border-radius: 4px;
    font-size: 11.5px;
    font-weight: 600;
    color: var(--text-2);
    text-decoration: none;
    transition: background .12s, color .12s;
  }
  .alpha-jump a:hover { background: var(--im-lime); color: var(--im-black); }
  .alpha-jump a.has-accounts { color: var(--text-1); font-weight: 700; }
  .alpha-jump a.disabled { color: var(--border); pointer-events: none; }

  /* ── Main content ─────────────────────────────────────────────────────── */
  .main {
    flex: 1;
    padding: 24px 28px 48px;
    min-width: 0;
  }
  .letter-group { margin-bottom: 6px; }
  /* Churn-quarter group headings (Accounts in End Date order) carry a
     count + ARR on the right, so the heading reads as a quarter subtotal. */
  .letter-heading.cq-heading { display: flex; align-items: baseline; gap: 10px; }
  .cq-heading .cq-label  { color: var(--text-1); }
  .cq-heading .cq-months { color: var(--text-3); font-weight: 600; letter-spacing: .6px; }
  .cq-heading .cq-meta   { margin-left: auto; color: var(--text-2); letter-spacing: .6px; }
  .alpha-jump a.cq-jump  { width: auto; padding: 0 7px; }
  /* Same grouping inside the Renewals / Expansions tables. */
  .flat-table tr.cq-group-row td {
    background: var(--im-lime-lt);
    font-size: 11px;
    font-weight: 700;
    letter-spacing: .6px;
    text-transform: uppercase;
    color: var(--text-1);
    padding: 8px 12px;
    border-bottom: 1px solid var(--border);
  }
  .flat-table tr.cq-group-row:hover td { background: var(--im-lime-lt); }
  .flat-table tr.cq-group-row .cq-months { color: var(--text-3); font-weight: 600; margin-left: 8px; }
  .flat-table tr.cq-group-row .cq-meta   { float: right; color: var(--text-2); }
  .letter-heading {
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 1.4px;
    text-transform: uppercase;
    color: var(--text-3);
    padding: 14px 0 8px;
    border-bottom: 1px solid var(--border);
    margin-bottom: 12px;
  }

  /* ── Account card ─────────────────────────────────────────────────────── */
  .card {
    background: var(--im-white);
    border-radius: var(--radius);
    box-shadow: var(--shadow);
    margin-bottom: 8px;
    overflow: hidden;
    border: 1px solid var(--border);
    transition: box-shadow .18s, border-color .18s;
  }
  .card:hover { box-shadow: var(--shadow-lg); border-color: #BBBBBB; }
  .card.open  { border-color: var(--im-black); }

  .card-header {
    display: flex;
    align-items: center;
    gap: 14px;
    padding: 14px 18px;
    cursor: pointer;
    user-select: none;
    transition: background .12s;
  }
  .card-header:hover { background: var(--surface); }

  .mp-badge {
    font-size: 11px;
    font-weight: 700;
    color: var(--im-lime);
    letter-spacing: .03em;
  }
  .card-name {
    font-size: 14.5px;
    font-weight: 600;
    color: var(--text-1);
    flex: 1;
    min-width: 0;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    letter-spacing: -.15px;
  }
  .card-arr {
    font-size: 14px;
    font-weight: 700;
    color: var(--text-1);
    white-space: nowrap;
    letter-spacing: -.2px;
  }

  /* Consumption link — lime pill matching "Book a Demo" CTA style */
  .consumption-link {
    display: inline-flex;
    align-items: center;
    gap: 3px;
    font-size: 11.5px;
    font-weight: 700;
    font-family: var(--font);
    color: var(--im-black);
    text-decoration: none;
    white-space: nowrap;
    flex-shrink: 0;
    padding: 5px 12px;
    background: var(--im-lime);
    border-radius: 4px;
    transition: background .14s;
  }
  .consumption-link:hover { background: var(--im-lime-dk); }

  /* Customer Central link — dark outline style to distinguish from lime CTA */
  .cc-link {
    display: inline-flex;
    align-items: center;
    gap: 3px;
    font-size: 11.5px;
    font-weight: 700;
    font-family: var(--font);
    color: var(--im-black);
    text-decoration: none;
    white-space: nowrap;
    flex-shrink: 0;
    padding: 5px 12px;
    background: var(--im-white);
    border: 1.5px solid var(--im-black);
    border-radius: 4px;
    transition: background .14s, color .14s;
  }
  .cc-link:hover { background: var(--im-black); color: var(--im-lime); }

  .card-meta {
    display: flex;
    align-items: center;
    gap: 8px;
    flex-shrink: 0;
  }
  .tag {
    font-size: 10.5px;
    font-weight: 600;
    padding: 3px 9px;
    border-radius: 4px;
    white-space: nowrap;
    letter-spacing: .1px;
    border: 1px solid transparent;
  }
  .tag-region   { background: #F0F0EE; color: var(--text-2); border-color: var(--border); }
  .tag-saas     { background: var(--im-lime-lt); color: #5A7A00; border-color: #C8E870; }
  .tag-sm-sup   { background: var(--amber-lt);   color: #92400E; border-color: #FCD34D; }
  .tag-sm-unsup { background: var(--red-lt);      color: #991B1B; border-color: #FCA5A5; }
  .tag-churn    { background: var(--amber-lt);   color: #92400E; border-color: #FCD34D; }
  .tag-renew    { background: var(--red-lt);     color: #991B1B; border-color: #FCA5A5; }
  .tag-obo      { background: #EDE9FE;          color: #5B21B6; border-color: #C4B5FD; }

  .expand-icon {
    font-size: 10px;
    color: var(--text-3);
    transition: transform .2s;
    flex-shrink: 0;
  }
  .card.open .expand-icon { transform: rotate(180deg); }

  .card-body {
    display: none;
    padding: 0 18px 18px;
    border-top: 1px solid var(--border);
  }
  .card.open .card-body { display: block; }

  /* ── CS Coverage ───────────────────────────────────────────────────────── */
  #coverage-list { padding-top: 16px; }
  .cov-card .card-name { flex: 0 1 200px; }
  .cov-chips { display: flex; flex-wrap: wrap; gap: 6px; flex: 1; justify-content: flex-end; }
  .cov-chip {
    display: inline-flex; align-items: baseline; gap: 5px;
    font-size: 12.5px; font-weight: 700; color: var(--text-1);
    padding: 3px 9px; border: 1px solid var(--border);
    border-radius: 4px; background: var(--surface); white-space: nowrap;
  }
  .cov-chip-k {
    font-size: 9.5px; font-weight: 700; letter-spacing: .6px;
    text-transform: uppercase; color: var(--text-3);
  }
  .cov-chip-onb { background: var(--amber-lt); border-color: #FCD34D; }
  .cov-body { padding: 0 18px 8px; }
  .cov-table { margin-top: 8px; }
  .cov-table td.cov-quad { width: 34px; }
  .cov-num { text-align: right; white-space: nowrap; }
  .cov-table tr.cov-onboarding td { background: var(--amber-lt); }
  .cov-table tr.cov-onboarding td:first-child { box-shadow: inset 3px 0 0 #F59E0B; }
  .cov-onb-tag { margin-left: 8px; background: #FDE68A; color: #92400E; border-color: #F59E0B; }
  .cov-empty { color: var(--text-3); padding: 24px 0; }

  /* ── Tabs ─────────────────────────────────────────────────────────────── */
  .tabs {
    display: flex;
    border-bottom: 1px solid var(--border);
    margin: 14px 0;
  }
  .tab-btn {
    padding: 9px 16px;
    border: none;
    background: none;
    font-size: 13px;
    font-weight: 500;
    font-family: var(--font);
    color: var(--text-3);
    cursor: pointer;
    border-bottom: 2px solid transparent;
    margin-bottom: -1px;
    transition: color .12s;
  }
  .tab-btn:hover { color: var(--text-1); }
  .tab-btn.active {
    color: var(--text-1);
    border-bottom-color: var(--im-lime);
    font-weight: 600;
  }
  .tab-pane { display: none; }
  .tab-pane.active { display: block; }

  /* ── Field grid ───────────────────────────────────────────────────────── */
  .field-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
    gap: 16px;
    padding: 6px 0;
  }
  /* Overview tab: three fixed columns, each stacking its fields top-to-bottom.
     Commercial on the left, account team in the middle, adoption on the right.
     Deliberately not auto-fill — the grouping is the point, so the columns must
     not reflow into an arbitrary number of tracks as the card widens. */
  .overview-cols {
    display: grid;
    grid-template-columns:
      minmax(150px, .9fr)    /* commercial + account team */
      minmax(180px, 1.1fr)   /* adoption: evidence        */
      minmax(200px, 1.1fr)   /* adoption: ratings + plot  */
      minmax(180px, 1.05fr)  /* performance               */
      minmax(180px, 1.05fr); /* engagement                */
    gap: 22px;
    padding: 6px 0;
    align-items: start;
  }
  .overview-col {
    display: flex;
    flex-direction: column;
    gap: 13px;
    min-width: 0;
  }
  /* Section heading above the three health-summary columns, so the Overview
     tab reads with the same structure as the PNG health summary. */
  .ov-head {
    font-size: 11px;
    font-weight: 700;
    letter-spacing: .6px;
    text-transform: uppercase;
    color: var(--text-1);
    padding-bottom: 5px;
    border-bottom: 2px solid var(--im-lime);
    margin-bottom: 2px;
  }
  /* Compact label:value pairs inside the health-summary columns. */
  .ov-pair {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    gap: 10px;
    font-size: 12.5px;
  }
  .ov-pair-k { color: var(--text-3); white-space: nowrap; }
  .ov-pair-v { color: var(--text-1); font-weight: 600; text-align: right; }
  .ov-pair-v.bad { color: #991B1B; }
  .ov-note {
    font-size: 12px;
    color: var(--text-2);
    line-height: 1.45;
  }
  /* Adoption rows carry the full picklist definition under the stoplight, so
     the criteria are readable without hovering for the tooltip. */
  .ov-adopt {
    display: flex;
    flex-direction: column;
    gap: 3px;
  }
  .ov-adopt-top {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    gap: 10px;
    font-size: 12.5px;
  }
  .ov-adopt-def {
    font-size: 10.5px;
    line-height: 1.4;
    color: var(--text-3);
  }
  /* Evidence under the PBAC rating: which platforms are in play and which
     features are in use. Indented behind a rule so it reads as support for the
     row above rather than as three sibling rows. */
  .ov-adopt-sub {
    margin: 2px 0 4px 0;
    padding-left: 8px;
    border-left: 2px solid var(--border);
    border-radius: 0;
  }
  .ov-adopt-subrow {
    display: flex;
    gap: 8px;
    font-size: 10.5px;
    line-height: 1.5;
  }
  .ov-adopt-subk { color: var(--text-3); flex: none; min-width: 54px; }
  /* PBAC evidence: wide enough for "External Catalogue" so values line up. */
  .ov-adopt-sub-pbac .ov-adopt-subk { min-width: 98px; }
  .ov-adopt-subv { color: var(--text-2); }
  .ov-adopt-none { color: var(--text-3); }

  /* Per-account PBAC plot. Sized in the SVG viewBox rather than here so the
     band geometry stays in one place; this only caps how wide it may grow. */
  .ov-mini-plot { width: 100%; max-width: 230px; height: auto; display: block; }
  .ov-mini-caption { max-width: 210px; margin-top: 3px; font-size: 10.5px; font-weight: 700; letter-spacing: .2px; }
  .ov-mini-frame { fill: none; stroke: var(--border); stroke-width: 1; }
  .ov-mini-grid  { stroke: var(--border); stroke-width: 1; }
  .ov-mini-pin   { stroke: var(--border); stroke-width: 1; stroke-dasharray: 2 2; }
  .ov-mini-tick  { font-size: 7px; fill: var(--text-3); }
  .ov-mini-dot   { stroke: #111827; stroke-width: 1.2; }
  /* A dashed ring means the ratio was never measured — the dot is sitting at
     the axis origin as a placeholder, not reporting zero consumption. */
  .ov-mini-dot.missing { stroke-dasharray: 2 1.5; }
  .ov-mini-dot.over    { stroke-width: 2; }
  .ov-mini-empty {
    font-size: 10.5px;
    color: var(--text-3);
    padding: 10px 0;
  }

  /* One tile per open escalation, linking out to Jira. Deliberately denser
     than the Escalations tab rows: this is a count made browsable, not a
     second triage surface. */
  .ov-esc-tiles { display: flex; flex-direction: column; gap: 5px; margin-top: 3px; }
  .ov-esc-tile {
    display: block;
    text-decoration: none;
    border: 1px solid var(--border);
    border-left-width: 3px;
    border-radius: 4px;
    padding: 5px 7px;
    background: var(--bg-1, transparent);
  }
  .ov-esc-tile:hover { border-color: var(--text-3); }
  .ov-esc-tile.sev-1 { border-left-color: #EF4444; }
  .ov-esc-tile.sev-2 { border-left-color: #F59E0B; }
  .ov-esc-tile.sev-3 { border-left-color: #9CA3AF; }
  .ov-esc-tile.sev-4 { border-left-color: var(--border); }
  .ov-esc-line { display: flex; align-items: center; gap: 6px; }
  .ov-esc-key  { font-size: 10.5px; font-weight: 600; color: var(--text-2); }
  .ov-esc-age  { font-size: 10px; color: var(--text-3); margin-left: auto; }
  .ov-esc-age.stale { color: #B91C1C; font-weight: 600; }
  .ov-esc-sum {
    display: block;
    font-size: 10.5px;
    line-height: 1.4;
    color: var(--text-2);
    margin-top: 2px;
    overflow: hidden;
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
  }

  /* Five columns need more room than three did before wrapping. The two
     Adoption columns are kept adjacent at the intermediate width so the plot
     stays beside the evidence it is drawn from. */
  @media (max-width: 1700px) {
    .overview-cols { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  }
  @media (max-width: 900px) {
    .overview-cols { grid-template-columns: 1fr; gap: 18px; }
  }
  .field-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    margin-bottom: 3px;
  }
  .field-value {
    font-size: 13.5px;
    color: var(--text-1);
    font-weight: 500;
  }
  .field-value.arr-value {
    font-size: 20px;
    font-weight: 800;
    color: var(--text-1);
    letter-spacing: -.5px;
  }
  .em-dash { color: var(--border); }

  /* ── Data table ───────────────────────────────────────────────────────── */
  .data-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 13px;
  }
  .data-table th {
    text-align: left;
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    padding: 7px 12px;
    border-bottom: 1.5px solid var(--border);
    background: var(--surface);
  }
  .data-table td {
    padding: 9px 12px;
    border-bottom: 1px solid var(--surface);
    color: var(--text-1);
    vertical-align: top;
  }
  .data-table tr:last-child td { border-bottom: none; }
  .data-table tr:hover td { background: var(--im-lime-lt); }
  .empty-msg {
    font-size: 13px;
    color: var(--text-3);
    font-style: italic;
    padding: 10px 2px;
  }

  /* ── Architecture badges ──────────────────────────────────────────────── */
  .arch-badge {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    padding: 5px 12px;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 600;
    margin: 3px;
  }

  /* ── Adoption stoplights ─────────────────────────────────────────────── */
  .stoplight-badge {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    font-size: 13px;
    font-weight: 600;
  }
  .stoplight-dot.square { border-radius: 2px; }
  .stoplight-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    display: inline-block;
    flex-shrink: 0;
    border: 1px solid rgba(0,0,0,.15);
  }
  .stoplight-green  { background: #22C55E; }
  .stoplight-amber  { background: #F59E0B; }
  .stoplight-yellow { background: #F59E0B; }
  .stoplight-red    { background: #EF4444; }
  .stoplight-black  { background: #111111; }

  /* 2x2 adoption glyph left of the account name on each card:
     PBAC | Request
     Comply | Agentic
     Unrated quadrants stay neutral grey. */
  .adopt-quad {
    display: grid;
    grid-template-columns: 9px 9px;
    grid-template-rows: 9px 9px;
    gap: 2px;
    flex: none;
  }
  .adopt-quad > span { border-radius: 2px; background: var(--border); position: relative; }
  /* Instant hover label. A native title= tooltip has a ~1s delay and is easy
     to miss on a 9px target. The label sits to the right of the square, not
     above it, because .card has overflow: hidden and would clip it. */
  .adopt-quad > span:hover { outline: 1px solid var(--text-1); }
  .adopt-quad > span:hover::after {
    content: attr(data-tip);
    position: absolute;
    left: calc(100% + 6px);
    top: 50%;
    transform: translateY(-50%);
    z-index: 20;
    padding: 3px 7px;
    border-radius: 4px;
    background: #111111;
    color: #FFFFFF;
    font-size: 11px;
    font-weight: 600;
    white-space: nowrap;
    pointer-events: none;
  }
  /* Re-declared at .adopt-quad specificity: the grey default above
     (class + element) otherwise outranks the bare .stoplight-* classes. */
  .adopt-quad > .stoplight-green { background: #22C55E; }
  .adopt-quad > .stoplight-amber { background: #F59E0B; }
  .adopt-quad > .stoplight-red   { background: #EF4444; }
  .adopt-quad > .stoplight-black { background: #111111; }
  .arch-yes  { background: var(--im-lime); color: var(--im-black); }
  .arch-no   { background: var(--surface); color: var(--text-3); border: 1px solid var(--border); }
  .arch-warn { background: var(--red-lt);  color: #991B1B; border: 1px solid #FCA5A5; }

  /* ── Diff-date picker ────────────────────────────────────────────────────── */
  .diff-date-wrap {
    display: flex;
    align-items: center;
    gap: 7px;
    flex-shrink: 0;
  }
  .diff-date-label {
    font-size: 11.5px;
    color: rgba(255,255,255,.4);
    white-space: nowrap;
  }
  .diff-date-outer {
    position: relative;
    display: inline-flex;
    align-items: center;
  }
  .diff-date-select {
    appearance: none;
    -webkit-appearance: none;
    background: rgba(255,255,255,.08);
    color: var(--im-white);
    border: 1px solid rgba(255,255,255,.12);
    border-radius: var(--radius-sm);
    padding: 6px 28px 6px 10px;
    font-size: 12.5px;
    font-family: var(--font);
    cursor: pointer;
    outline: none;
    transition: border-color .15s, background .15s;
  }
  .diff-date-select:focus { border-color: var(--im-lime); background: rgba(255,255,255,.12); }
  .diff-date-select option { background: #1a1a1a; color: var(--im-white); }
  .diff-date-chevron {
    position: absolute;
    right: 8px;
    pointer-events: none;
    color: rgba(255,255,255,.35);
    font-size: 10px;
    line-height: 1;
  }

  /* ── Changes tab ─────────────────────────────────────────────────────────── */
  .changes-header {
    background: var(--im-white);
    border-bottom: 1px solid var(--border);
    padding: 16px 28px;
    display: flex;
    align-items: center;
    gap: 32px;
    flex-wrap: wrap;
  }
  .changes-date-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    margin-bottom: 3px;
  }
  .changes-date-val {
    font-size: 20px;
    font-weight: 800;
    color: var(--text-1);
    letter-spacing: -.4px;
  }
  .changes-body {
    flex: 1;
    overflow-y: auto;
    padding: 20px 28px 48px;
  }
  .change-section {
    background: var(--im-white);
    border: 1px solid var(--border);
    border-radius: var(--radius);
    margin-bottom: 10px;
    overflow: hidden;
  }
  .change-section-hdr {
    padding: 13px 18px;
    display: flex;
    align-items: center;
    gap: 8px;
    cursor: pointer;
    user-select: none;
    transition: background .12s;
  }
  .change-section-hdr:hover { background: var(--surface); }
  .change-section-title { font-size: 13.5px; font-weight: 600; flex: 1; }
  .change-section-chevron { font-size: 11px; color: var(--text-3); flex-shrink: 0; }
  .chg-badge {
    font-size: 10.5px;
    font-weight: 700;
    padding: 2px 9px;
    border-radius: 4px;
    white-space: nowrap;
  }
  .chg-added   { background: var(--im-lime-lt); color: #5A7A00; }
  .chg-removed { background: var(--red-lt);     color: #991B1B; }
  .chg-changed { background: var(--amber-lt);   color: #92400E; }
  .change-section-body { border-top: 1px solid var(--border); }
  .chg-group {
    padding: 14px 18px;
    border-bottom: 1px solid var(--surface-2);
  }
  .chg-group:last-child { border-bottom: none; }
  .chg-group-title { margin-bottom: 10px; }
  .chg-old  { color: var(--text-3); }
  .chg-new  { font-weight: 600; color: var(--text-1); }
  .chg-null { color: var(--border); font-style: italic; }

  /* ── Refresh button ──────────────────────────────────────────────────────── */
  .refresh-btn {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 7px 15px;
    background: var(--im-lime);
    color: var(--im-black);
    border: none;
    border-radius: var(--radius-sm);
    font-size: 12.5px;
    font-weight: 700;
    font-family: var(--font);
    cursor: pointer;
    white-space: nowrap;
    flex-shrink: 0;
    transition: background .14s, opacity .14s;
  }
  .refresh-btn:hover:not(:disabled) { background: var(--im-lime-dk); }
  .refresh-btn:disabled { opacity: .55; cursor: wait; }

  /* The buttons and the build date stack in the top-right corner. The stack
     is what keeps the date pinned under the buttons rather than sitting
     beside them in the flex row the topbar itself lays out. */
  .topbar-actions {
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    gap: 4px;
    flex-shrink: 0;
    margin-left: auto;
  }
  .topbar-btns { display: flex; gap: 10px; }
  /* Quiet on purpose. This is provenance, not a metric — it should answer
     "how stale is this?" when asked and be invisible otherwise. */
  .topbar-updated {
    font-size: 10.5px;
    font-weight: 500;
    letter-spacing: .3px;
    color: rgba(255,255,255,.45);
    white-space: nowrap;
    padding-right: 2px;
  }

  /* ── Modal ────────────────────────────────────────────────────────────────── */
  .modal-overlay {
    position: fixed;
    inset: 0;
    background: rgba(0,0,0,.45);
    z-index: 999;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .modal-box {
    background: var(--im-white);
    border-radius: var(--radius);
    padding: 28px 32px;
    max-width: 440px;
    width: 90%;
    box-shadow: var(--shadow-lg);
  }
  .modal-title { font-size: 15px; font-weight: 700; margin-bottom: 10px; color: var(--text-1); }
  .modal-body  { font-size: 13.5px; color: var(--text-2); line-height: 1.55; margin-bottom: 4px; }
  .modal-code {
    display: block;
    background: var(--surface);
    border: 1px solid var(--border);
    border-radius: var(--radius-sm);
    padding: 10px 14px;
    font-family: "Menlo", "Consolas", monospace;
    font-size: 13px;
    color: var(--text-1);
    margin: 10px 0 4px;
    cursor: pointer;
    user-select: all;
    transition: background .12s;
  }
  .modal-code:hover { background: var(--im-lime-lt); }
  .modal-hint  { font-size: 11.5px; color: var(--text-3); margin-bottom: 16px; }
  .modal-close {
    padding: 8px 20px;
    background: var(--im-black);
    color: var(--im-lime);
    border: none;
    border-radius: var(--radius-sm);
    font-size: 13px;
    font-weight: 600;
    font-family: var(--font);
    cursor: pointer;
  }
  .modal-close:hover { opacity: .85; }

  /* ── No results ───────────────────────────────────────────────────────── */
  #no-results {
    display: none;
    text-align: center;
    padding: 80px 20px;
    color: var(--text-3);
    font-size: 16px;
    font-weight: 500;
  }

  /* ── Page-level tab nav ───────────────────────────────────────────────── */
  .page-tabs {
    background: var(--im-white);
    border-bottom: 1px solid var(--border);
    display: flex;
    align-items: stretch;
    padding: 0 24px;
    position: sticky;
    top: var(--topbar-h);
    height: var(--tabs-h);
    z-index: 90;
    box-shadow: 0 1px 0 var(--border);
  }
  .page-tab {
    padding: 0 20px;
    border: none;
    background: none;
    font-size: 13.5px;
    font-weight: 500;
    font-family: var(--font);
    color: var(--text-3);
    cursor: pointer;
    border-bottom: 3px solid transparent;
    display: flex;
    align-items: center;
    gap: 8px;
    transition: color .12s;
    white-space: nowrap;
  }
  .page-tab:hover { color: var(--text-1); }
  .page-tab.active {
    color: var(--text-1);
    border-bottom-color: var(--im-lime);
    font-weight: 600;
  }
  .page-tab-count {
    font-size: 11px;
    font-weight: 700;
    background: var(--surface-2);
    color: var(--text-3);
    padding: 1px 7px;
    border-radius: 10px;
    min-width: 24px;
    text-align: center;
  }
  .page-tab.active .page-tab-count {
    background: var(--im-lime);
    color: var(--im-black);
  }

  /* ── Page panels ──────────────────────────────────────────────────────── */
  .page { display: none; flex: 1; }
  .page.active { display: flex; }
  /* Every page is now a sub-tab bar stacked above its panels. */
  #page-engagement.active,
  #page-adoption.active,
  #page-performance.active { flex-direction: column; }

  /* ── Documentation: Confluence banner + embedded doc iframes ──────────── */
  /* Fixed to the viewport so each document scrolls inside its own frame. */
  #page-documentation.active {
    flex-direction: column;
    height: calc(100vh - var(--topbar-h) - var(--tabs-h));
  }
  .doc-banner {
    display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 10px 20px;
    padding: 12px 24px; background: var(--im-lime-lt); border-bottom: 1px solid var(--border); flex-shrink: 0;
  }
  .doc-banner-label { font-weight: 700; font-size: 14px; }
  .doc-banner-sub { font-size: 12.5px; color: var(--text-2); }
  .doc-banner-link {
    font-size: 12.5px; font-weight: 700; text-decoration: none; white-space: nowrap;
    padding: 7px 14px; border-radius: 4px; background: var(--im-black); color: var(--im-lime);
    transition: background .14s;
  }
  .doc-banner-link:hover { background: #2A2A2A; }
  .doc-open {
    margin-left: auto; align-self: center; border: 1px solid var(--border); background: var(--im-white);
    font-family: var(--font); font-size: 12px; font-weight: 600; color: var(--text-2);
    padding: 5px 12px; border-radius: 4px; cursor: pointer; white-space: nowrap;
  }
  .doc-open:hover { border-color: var(--im-black); color: var(--text-1); }
  #page-documentation .subpanel { overflow: hidden; }
  #page-documentation .sub-tabs { overflow-y: hidden; }
  .doc-frame { flex: 1; width: 100%; min-height: 0; border: 0; background: var(--surface); display: block; }
  .doc-frame[hidden], .doc-missing[hidden] { display: none; }
  .doc-missing { padding: 24px; color: var(--text-2); font-size: 13.5px; }

  /* ── Sub-tabs, shared by all three pages ──────────────────────────────── */
  .subpage { flex: 1; display: flex; flex-direction: column; min-height: 0; }
  .sub-tabs {
    background: var(--im-white); border-bottom: 1px solid var(--border);
    padding: 0 24px; display: flex; flex-shrink: 0; overflow-x: auto;
  }
  .sub-btn {
    padding: 10px 18px; border: none; background: none; font-size: 13px;
    font-weight: 500; font-family: var(--font); color: var(--text-3);
    cursor: pointer; border-bottom: 2px solid transparent; margin-bottom: -1px;
    transition: color .12s; white-space: nowrap;
    display: flex; align-items: center; gap: 7px;
  }
  .sub-btn:hover { color: var(--text-1); }
  .sub-btn.active { color: var(--text-1); border-bottom-color: var(--im-lime); font-weight: 600; }
  .sub-count {
    font-size: 10.5px; font-weight: 700; background: var(--surface-2);
    color: var(--text-3); padding: 1px 6px; border-radius: 9px;
  }
  .sub-btn.active .sub-count { background: var(--im-lime); color: var(--im-black); }

  .subpanel { display: none; flex: 1; flex-direction: column; min-height: 0; overflow-y: auto; }
  .subpanel.active { display: flex; }
  /* Accounts is the one panel laid out as sidebar + main rather than stacked,
     and it scrolls inside .main, not at the panel level. */
  .subpanel-row.active { flex-direction: row; overflow-y: hidden; }
  .subpanel-row > .layout { flex: 1; min-height: 0; }

  /* ── PBAC panel: scatter + write-up ───────────────────────────────────── */
  .pbac-scatter-wrap { display: flex; gap: 18px; padding: 20px 24px; align-items: flex-start; }
  /* The plot takes 90% of the width left after the side panel, and the panel
     absorbs the rest — so the scatter is ~10% smaller (and shorter, since the
     SVG keeps its aspect ratio) and the hover call-out stays on screen. */
  .pbac-plot { flex: 0 1 calc(0.9 * (100% - 276px)); min-width: 0; overflow-x: auto; }
  .pbac-plot svg { display: block; width: 100%; min-width: 540px; height: auto; }
  .pbac-side {
    flex: 1 0 258px; border: 1px solid var(--border); border-radius: var(--radius-sm);
    background: var(--im-white); padding: 14px 15px; font-size: 12.5px; min-height: 300px;
  }
  .pbac-side.pinned { border-color: var(--im-lime-dk); box-shadow: 0 0 0 2px var(--im-lime-lt); }
  .ps-empty { color: var(--text-3); font-size: 12px; line-height: 1.6; }
  .ps-name  { font-size: 14.5px; font-weight: 700; line-height: 1.3; margin-bottom: 6px; }
  .ps-block { padding-top: 10px; margin-top: 10px; border-top: 1px solid var(--border); }
  .ps-h {
    font-size: 9.5px; font-weight: 700; letter-spacing: .1em; text-transform: uppercase;
    color: var(--text-3); margin-bottom: 6px;
  }
  .ps-kv { display: flex; justify-content: space-between; gap: 10px; padding: 2px 0; }
  .ps-kv span:first-child { color: var(--text-2); }
  .ps-kv span:last-child  { font-weight: 600; font-variant-numeric: tabular-nums; text-align: right; }
  .ps-sub { margin-top: 7px; }
  .ps-sub-k { color: var(--text-2); display: block; margin-bottom: 2px; }
  .ps-note  { font-size: 11px; color: var(--text-3); margin-top: 4px; line-height: 1.45; }
  .ps-hint  { font-size: 11px; color: var(--text-3); margin-top: 12px; padding-top: 9px; border-top: 1px solid var(--border); }
  .ps-hint button {
    font: inherit; font-size: 11px; background: none; border: 0; padding: 0;
    color: var(--text-1); text-decoration: underline; cursor: pointer;
  }
  .pbac-facet-note { font-size: 11.5px; color: var(--text-3); padding: 6px 2px 0; }
  .pbac-pt { cursor: pointer; }
  .pbac-pt.dim { opacity: .12; }
  .pbac-hit { fill: transparent; stroke: none; }
  .pbac-axis-text { fill: var(--text-3); font-size: 10px; }
  .pbac-axis-title { fill: var(--text-2); font-size: 11px; font-weight: 700; letter-spacing: .06em; }
  .pbac-band-label { font-size: 9px; font-weight: 700; letter-spacing: .1em; }

  .pbac-writeup { padding: 4px 24px 40px; max-width: 760px; }
  .pbac-writeup h3 {
    font-size: 15px; font-weight: 700; margin: 22px 0 6px; color: var(--text-1);
  }
  .pbac-writeup p  { font-size: 13px; line-height: 1.65; color: var(--text-2); margin-bottom: 10px; }
  .pbac-writeup ul { margin: 0 0 10px 18px; }
  .pbac-writeup li { font-size: 13px; line-height: 1.6; color: var(--text-2); margin-bottom: 6px; }
  .pbac-writeup b, .pbac-writeup strong { color: var(--text-1); }
  .pbac-matrix { border-collapse: separate; border-spacing: 3px; margin: 6px 0 12px; }
  .pbac-matrix th {
    font-size: 9.5px; font-weight: 700; letter-spacing: .07em; text-transform: uppercase;
    color: var(--text-3); padding: 4px 10px; text-align: center; white-space: nowrap;
  }
  .pbac-matrix th.rowh { text-align: right; color: var(--text-2); text-transform: none; letter-spacing: 0; font-size: 11.5px; }
  .pbac-matrix td {
    border-radius: var(--radius-sm); text-align: center; padding: 13px 16px;
    font-weight: 700; font-size: 11px; letter-spacing: .06em; text-transform: uppercase;
  }
  .pbac-weights { margin: 4px 0 12px; }
  .pbac-weights td { font-size: 12.5px; padding: 5px 12px 5px 0; border-bottom: 1px solid var(--border); }
  .pbac-weights td.w { text-align: right; font-weight: 700; font-variant-numeric: tabular-nums; padding-right: 0; }

  /* ── Product adoption table (under the scatter) ───────────────────────── */
  .pa-table-wrap { padding: 0 24px 40px; overflow-x: auto; }
  .pa-table { min-width: 1500px; }
  .pa-table th { cursor: pointer; white-space: nowrap; position: sticky; top: 0; background: var(--im-white); z-index: 1; }
  .pa-table td { vertical-align: top; }
  .pa-table tbody tr { cursor: pointer; }
  .pa-table tbody tr:hover td { background: var(--im-lime-lt); }
  .pa-table tr.pa-pinned td { background: var(--im-lime-lt); box-shadow: inset 0 1px 0 var(--im-lime-dk), inset 0 -1px 0 var(--im-lime-dk); }
  .pa-table tr.pa-onboarding td:first-child { box-shadow: inset 3px 0 0 #F59E0B; }
  .pa-num { font-variant-numeric: tabular-nums; white-space: nowrap; }
  .pa-score { font-weight: 700; }
  .pa-of { color: var(--text-3); font-size: 11px; }
  #sub-panel-overview .adopt-stats { border-top: 1px solid var(--border); }
  @media (max-width: 900px) {
    .pbac-scatter-wrap { flex-direction: column; align-items: stretch; }
    .pbac-plot { flex: none; }
    .pbac-side { flex: none; min-height: 0; }
  }

  .adopt-stats {
    padding: 14px 24px; display: flex; gap: 32px; border-bottom: 1px solid var(--border);
    background: var(--surface); flex-wrap: wrap; flex-shrink: 0;
  }

  /* ── Critical churn risk (Adoption sub-tab) ───────────────────────────── */
  .ccr-wrap { padding: 20px 24px 6px; flex-shrink: 0; }
  .ccr-brief {
    background: var(--im-white); border: 1px solid var(--border); border-radius: var(--radius);
    box-shadow: var(--shadow); overflow: hidden; max-width: 1240px;
  }
  .ccr-head {
    background: var(--im-black); color: var(--im-white); padding: 18px 22px 16px;
    display: flex; align-items: flex-end; justify-content: space-between; gap: 20px; flex-wrap: wrap;
  }
  .ccr-eyebrow {
    font-size: 10.5px; font-weight: 700; letter-spacing: .14em; text-transform: uppercase; color: var(--im-lime);
  }
  .ccr-title { font-size: 22px; font-weight: 800; letter-spacing: -.02em; line-height: 1.2; margin: 6px 0 4px; }
  .ccr-sub { font-size: 12.5px; color: #A8A8A8; }
  .ccr-toggle { display: inline-flex; background: #1F1F1F; border-radius: 8px; padding: 3px; gap: 2px; }
  .ccr-tbtn {
    font-family: var(--font); font-size: 12px; font-weight: 600; color: #BDBDBD; background: none;
    border: 0; border-radius: 6px; padding: 6px 12px; cursor: pointer; white-space: nowrap;
  }
  .ccr-tbtn:hover { color: var(--im-white); }
  .ccr-tbtn.active { background: var(--im-lime); color: var(--im-black); }
  .ccr-kpis {
    display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    border-bottom: 1px solid var(--border);
  }
  .ccr-kpi { padding: 12px 22px; border-right: 1px solid var(--border); }
  .ccr-kpi:last-child { border-right: 0; }
  .ccr-kpi-l { font-size: 10px; font-weight: 700; letter-spacing: .1em; text-transform: uppercase; color: var(--text-3); }
  .ccr-kpi-v { font-size: 22px; font-weight: 800; letter-spacing: -.02em; font-variant-numeric: tabular-nums; }
  .ccr-kpi-s { font-size: 11.5px; color: var(--text-3); }
  .ccr-view { padding: 14px 18px 12px; overflow-x: auto; }
  .ccr-view > svg { display: block; width: 100%; min-width: 640px; height: auto; }
  .ccr-view .ccr-pt { cursor: pointer; }
  .ccr-view .ccr-pt:hover .ccr-mark { stroke: var(--im-lime-dk); stroke-width: 3; }
  .ccr-bar-val { font-size: 12px; font-weight: 800; fill: var(--text-1); font-family: var(--font); font-variant-numeric: tabular-nums; }
  .ccr-seg-val { font-size: 10.5px; font-weight: 700; font-family: var(--font); font-variant-numeric: tabular-nums; }
  .ccr-bar-n { font-size: 10.5px; fill: var(--text-3); font-family: var(--font); }
  .ccr-lbl { cursor: pointer; }
  .ccr-lbl-name { font-size: 12.5px; font-weight: 700; fill: var(--text-1); font-family: var(--font); }
  .ccr-lbl-mo   { font-size: 10.5px; font-weight: 700; fill: var(--im-lime); font-family: var(--font); }
  .ccr-lbl:hover .ccr-lbl-name { text-decoration: underline; }
  .ccr-leader { stroke: #9A9A9A; stroke-width: .8; fill: none; }
  .ccr-axis { fill: var(--text-3); font-size: 11px; font-variant-numeric: tabular-nums; font-family: var(--font); }
  .ccr-axis-title { fill: var(--text-2); font-size: 11px; font-weight: 700; letter-spacing: .08em; font-family: var(--font); }
  .ccr-legend { display: flex; flex-wrap: wrap; gap: 6px 20px; margin-top: 10px; font-size: 11.5px; color: var(--text-3); }
  .ccr-key { display: inline-flex; align-items: center; gap: 6px; }
  .ccr-key svg { width: 16px; height: 14px; }
  .ccr-mo {
    display: inline-block; min-width: 44px; text-align: center; font-size: 11.5px; font-weight: 700;
    background: var(--im-black); color: var(--im-lime); border-radius: 999px; padding: 1px 8px;
    font-variant-numeric: tabular-nums;
  }
  .ccr-tables { padding: 10px 24px 40px; display: flex; flex-direction: column; gap: 26px; flex-shrink: 0; }
  .ccr-thead { display: flex; align-items: center; gap: 10px; }
  .ccr-badge {
    font-size: 12px; font-weight: 800; background: var(--im-black); color: var(--im-lime);
    border-radius: 5px; padding: 2px 8px; letter-spacing: .04em;
  }
  .ccr-ttitle { font-size: 14px; font-weight: 700; }
  .ccr-tmeta { margin-left: auto; font-size: 12.5px; color: var(--text-2); font-variant-numeric: tabular-nums; }
  .ccr-tmeta b { color: var(--text-1); }
  .ccr-table tbody tr { cursor: pointer; }
  .ccr-table tbody tr:hover td { background: var(--im-lime-lt); }
  .ccr-table th { cursor: default; }
  .ccr-table.ccr-sortable th { cursor: pointer; }
  .ccr-table td { white-space: nowrap; }
  .ccr-table.ccr-sortable th:hover { color: var(--text-1); }
  .ccr-rating { display: inline-flex; align-items: center; gap: 7px; white-space: nowrap; font-weight: 600; }
  .ccr-swatch { width: 11px; height: 11px; border-radius: 3px; flex: none; }
  .ccr-table .num { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
  .ccr-empty { color: var(--text-3); font-size: 13px; padding: 14px 0 0; }
  .ccr-foot { font-size: 11.5px; color: var(--text-3); line-height: 1.6; }

  /* ── SaaS migration board ─────────────────────────────────────────────── */
  .saas-board-wrap { flex: 1; overflow: auto; padding: 18px 24px 40px; min-height: 0; }
  .saas-board { display: flex; gap: 14px; align-items: flex-start; min-width: min-content; }
  .saas-col {
    flex: 0 0 258px; background: var(--surface); border: 1px solid var(--border);
    border-radius: var(--radius-sm); display: flex; flex-direction: column;
  }
  /* The quarter that has already started reads differently from the ones that
     have not: it is the one that can still slip. */
  .saas-col.current { border-color: var(--im-lime-dk); background: var(--im-lime-lt); }
  .saas-col-head {
    padding: 10px 12px; border-bottom: 1px solid var(--border);
    display: flex; align-items: baseline; justify-content: space-between; gap: 8px;
  }
  .saas-q { font-size: 13px; font-weight: 700; color: var(--text-1); }
  .saas-q-sub { font-size: 10.5px; color: var(--text-3); }
  .saas-q-n {
    font-size: 10.5px; font-weight: 700; background: var(--im-black); color: var(--im-lime);
    padding: 1px 7px; border-radius: 9px;
  }
  .saas-col-body { padding: 8px; display: flex; flex-direction: column; gap: 8px; }
  /* Target SaaS share if every migration up to this quarter lands. */
  .saas-tgt {
    padding: 7px 12px; border-bottom: 1px solid var(--border);
    background: var(--im-lime-lt); font-size: 11px; color: var(--text-2);
    display: flex; align-items: baseline; justify-content: space-between; gap: 8px;
  }
  .saas-tgt-k   { font-size: 9.5px; font-weight: 700; letter-spacing: .8px; text-transform: uppercase; color: var(--text-3); }
  .saas-tgt-v   { font-size: 15px; font-weight: 800; color: var(--text-1); }
  .saas-tgt-sub { font-size: 10.5px; color: var(--text-3); white-space: nowrap; }
  .saas-churn-tag {
    display: inline-block; margin-left: 6px; font-size: 9.5px; font-weight: 700;
    padding: 1px 6px; border-radius: 4px; background: #FEE2E2; color: #991B1B;
  }

  .saas-card {
    background: var(--im-white); border: 1px solid var(--border);
    border-radius: var(--radius-sm); padding: 9px 10px; cursor: pointer;
    border-left: 3px solid var(--text-3); transition: box-shadow .12s, border-color .12s;
  }
  .saas-card:hover { box-shadow: 0 2px 8px rgba(0,0,0,.08); }
  .saas-card.open { box-shadow: 0 2px 10px rgba(0,0,0,.1); }
  .saas-card-name { font-size: 12.5px; font-weight: 700; line-height: 1.3; }
  .saas-card-meta {
    font-size: 10.5px; color: var(--text-3); margin-top: 3px;
    display: flex; flex-wrap: wrap; gap: 4px 8px;
  }
  .saas-stage {
    display: inline-block; font-size: 10px; font-weight: 700; letter-spacing: .03em;
    padding: 1px 7px; border-radius: 9px; margin-top: 6px;
  }
  .saas-note {
    display: none; margin-top: 7px; padding-top: 7px; border-top: 1px solid var(--border);
    font-size: 11.5px; line-height: 1.5; color: var(--text-2); white-space: pre-wrap;
  }
  .saas-card.open .saas-note { display: block; }
  .saas-note-hint { font-size: 10.5px; color: var(--text-3); margin-top: 5px; font-style: italic; }
  .saas-card.open .saas-note-hint { display: none; }

  /* Stage colours: grey while it is only a conversation, amber once someone has
     committed, lime once the migration is actually running. */
  .saas-s-pipeline  { background: var(--surface-2); color: var(--text-2); }
  .saas-s-talking   { background: #E8EEF6; color: #33507A; }
  .saas-s-committed { background: var(--amber-lt); color: #92400E; }
  .saas-s-running   { background: var(--im-lime); color: var(--im-black); }
  .saas-card.st-pipeline  { border-left-color: #C3CBD6; }
  .saas-card.st-talking   { border-left-color: #6C8FC7; }
  .saas-card.st-committed { border-left-color: var(--amber); }
  .saas-card.st-running   { border-left-color: var(--im-lime-dk); }

  .saas-overdue { color: #991B1B; font-weight: 700; }

  /* ── Escalations ──────────────────────────────────────────────────────── */
  .esc-list-wrap { flex: 1; overflow-y: auto; padding: 14px 24px 40px; min-height: 0; }
  .esc-list { display: flex; flex-direction: column; gap: 8px; max-width: 1100px; }
  .esc-row {
    background: var(--im-white); border: 1px solid var(--border);
    border-radius: var(--radius-sm); padding: 10px 12px;
    border-left: 3px solid var(--text-3);
    display: grid; grid-template-columns: 86px 1fr auto; gap: 4px 14px; align-items: start;
  }
  .esc-row.sev-1 { border-left-color: #EF4444; }
  .esc-row.sev-2 { border-left-color: var(--amber); }
  .esc-row.sev-3 { border-left-color: #6C8FC7; }
  .esc-row.sev-4 { border-left-color: #C3CBD6; }
  .esc-key {
    font-size: 11.5px; font-weight: 700; font-variant-numeric: tabular-nums;
    color: var(--text-1); text-decoration: none; white-space: nowrap;
  }
  .esc-key:hover { text-decoration: underline; }
  .esc-key::after { content: " \\2197"; font-weight: 400; color: var(--text-3); }
  .esc-summary { font-size: 12.5px; line-height: 1.45; color: var(--text-1); }
  .esc-meta {
    grid-column: 2 / 4; font-size: 10.5px; color: var(--text-3);
    display: flex; flex-wrap: wrap; gap: 3px 10px; margin-top: 2px;
  }
  .esc-meta b { color: var(--text-2); font-weight: 600; }
  .esc-sev {
    font-size: 10px; font-weight: 700; letter-spacing: .04em; padding: 2px 8px;
    border-radius: 9px; white-space: nowrap; justify-self: start;
  }
  .esc-sev-1 { background: var(--red-lt);   color: #991B1B; }
  .esc-sev-2 { background: var(--amber-lt); color: #92400E; }
  .esc-sev-3 { background: #E8EEF6;         color: #33507A; }
  .esc-sev-4 { background: var(--surface-2); color: var(--text-2); }
  .esc-age { font-size: 10.5px; color: var(--text-3); white-space: nowrap; text-align: right; }
  /* Anything open for a quarter is the tail that stops getting looked at. */
  .esc-age.stale { color: #991B1B; font-weight: 700; }
  .esc-unmatched { color: #92400E; font-style: italic; }
  .pct-high { color: #166534; font-weight: 700; }
  .pct-med  { color: #92400E; font-weight: 700; }
  .pct-low  { color: #991B1B; font-weight: 700; }
  .pct-none { color: var(--text-3); }
  .flat-table tr.low-con td { background: #FEF3C7; }
  .flat-table tr.low-con:hover td { background: #FDE68A; }

{{AGX_CSS}}

  /* ── Flat table pages (Renewals / Expansions) ─────────────────────────── */
  .flat-page { flex: 1; display: flex; flex-direction: column; min-height: 0; }
  .flat-filters {
    background: var(--im-white);
    border-bottom: 1px solid var(--border);
    padding: 10px 24px;
    display: flex;
    align-items: center;
    gap: 6px;
    flex-wrap: wrap;
  }
  .flat-pill {
    padding: 5px 12px;
    border-radius: var(--radius-sm);
    border: 1px solid var(--border);
    background: transparent;
    font-size: 12.5px;
    font-family: var(--font);
    color: var(--text-2);
    cursor: pointer;
    transition: background .12s, color .12s, border-color .12s;
    white-space: nowrap;
  }
  .flat-pill:hover { background: var(--im-lime-lt); border-color: #C8E870; color: var(--text-1); }
  .flat-pill.active { background: var(--im-black); color: var(--im-lime); border-color: var(--im-black); font-weight: 600; }

  .flat-filters-csm {
    padding-top: 6px;
    border-top: 1px solid var(--surface-2);
  }

  .flat-stats {
    padding: 14px 24px;
    display: flex;
    gap: 32px;
    border-bottom: 1px solid var(--border);
    background: var(--surface);
    flex-wrap: wrap;
  }
  .stat-item { display: flex; flex-direction: column; gap: 2px; }
  .stat-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
  }
  .stat-value {
    font-size: 20px;
    font-weight: 800;
    color: var(--text-1);
    letter-spacing: -.5px;
  }

  .flat-table-wrap { flex: 1; overflow-y: auto; padding: 0 24px 40px; }
  .flat-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 13px;
    margin-top: 16px;
  }
  .flat-table th {
    text-align: left;
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-3);
    padding: 9px 12px;
    border-bottom: 1.5px solid var(--border);
    background: var(--im-white);
    position: sticky;
    top: 0;
    cursor: pointer;
    user-select: none;
    white-space: nowrap;
    transition: color .12s;
  }
  .flat-table th:hover { color: var(--text-1); }
  .flat-table th.sort-asc::after  { content: " \\2191"; color: var(--im-lime-dk); }
  .flat-table th.sort-desc::after { content: " \\2193"; color: var(--im-lime-dk); }
  .flat-table td {
    padding: 10px 12px;
    border-bottom: 1px solid var(--surface);
    color: var(--text-1);
    vertical-align: middle;
  }
  .flat-table tr:hover td { background: var(--im-lime-lt); }
  .flat-table tr:last-child td { border-bottom: none; }
  .flat-table .acct-link {
    font-weight: 600;
    color: var(--text-1);
    text-decoration: none;
    cursor: pointer;
  }
  .flat-table .acct-link:hover { text-decoration: underline; text-decoration-color: var(--im-lime); }
  .opp-link { color: var(--text-1); text-decoration: none; font-weight: 500; }
  .opp-link:hover { text-decoration: underline; text-decoration-color: var(--im-lime); color: var(--im-black); }
  .risk-high     { color: #991B1B; font-weight: 600; }
  .risk-moderate { color: #92400E; font-weight: 600; }
  .risk-low      { color: #166534; font-weight: 600; }
  .churn-q {
    display: inline-block;
    background: var(--amber-lt);
    color: #92400E;
    border: 1px solid #FCD34D;
    border-radius: 4px;
    font-size: 11px;
    font-weight: 700;
    padding: 2px 7px;
  }
</style>
</head>
<body>

<!-- TOP BAR -->
<div class="topbar">
  <a class="topbar-logo" href="https://www.immuta.com" target="_blank" rel="noopener">
    <svg width="114" height="20" viewBox="0 0 142 24" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M95.968 3.99802H99.8179V11.656C99.8179 16.8192 96.57 20.0561 92.1339 20.0561C87.6978 20.0561 84.5925 16.8192 84.5925 11.656V3.99802H88.4424V11.656C88.4424 14.7508 89.7733 16.5192 92.1339 16.5192C94.4946 16.5192 95.9838 14.7508 95.9838 11.656V3.99802H95.968ZM40.675 15.635L35.0507 3.99802H30.7571V19.7561H34.6071V11.7981L38.457 19.8982H42.7505L46.6004 11.7981V19.7561H50.4503V3.99802H46.1568L40.6909 15.635H40.675ZM77.4789 3.99802H73.1854L67.7195 15.4929L62.0951 3.99802H57.8016V19.7561H61.6515V11.7981L65.5014 19.8982H69.7949L73.6448 11.7981V19.7561H77.4947V3.99802H77.4789ZM120.509 3.99802H105.426V7.53491H111.051V19.7561H114.901V7.53491H120.525V3.99802H120.509ZM141.2 19.8982H137.065L136.036 17.3877H128.352L127.322 19.8982H123.329L130.126 4.14013H134.42L141.216 19.8982H141.2ZM134.689 14.3087L132.17 8.41913L129.651 14.3087H134.673H134.689ZM24.1188 0.161133V23.7351H0.464844V0.161133H24.1188ZM19.0965 3.99802H15.2466V19.7561H19.0965V3.99802Z" fill="white"/>
    </svg>
    <span class="topbar-section">Customer 360</span>
  </a>
  <div class="search-wrap">
    <span class="search-icon">&#128269;</span>
    <input type="text" id="search" placeholder="Search accounts..." autocomplete="off">
  </div>
  <div class="count-badge" id="count-badge"></div>
  <div class="diff-date-wrap">
    <span class="diff-date-label">Compare to</span>
    <div class="diff-date-outer">
      <select id="diff-date" class="diff-date-select"></select>
      <span class="diff-date-chevron">&#9660;</span>
    </div>
  </div>
  <div class="topbar-actions">
    <div class="topbar-btns">
      <!-- Drive file 1K5pNbz2TPrxGSvU8l2hbEjbMFfFShQ09, as the uc?export=download
           form so the button downloads directly rather than opening the Drive
           preview page. Swap to /file/d/<id>/view to land on the preview instead. -->
      <a class="refresh-btn" href="https://drive.google.com/uc?export=download&id=1K5pNbz2TPrxGSvU8l2hbEjbMFfFShQ09" target="_blank" rel="noopener" style="text-decoration:none">&#11123; Download Update</a>
      <button class="refresh-btn" id="refresh-btn" onclick="triggerRefresh()" title="Only works for the product owner: needs the local R refresh server (scripts/serve.R) running. Everyone else: use Download Update.">&#8635; Refresh</button>
    </div>
    <!-- When op_list was built, not when this file was written. A refresh
         that fails leaves the old page on screen; a render date would keep
         advancing and hide that. -->
    <div class="topbar-updated" id="topbar-updated">{{UPDATED}}</div>
  </div>
</div>

<!-- REFRESH MODAL -->
<div id="refresh-modal" class="modal-overlay" style="display:none" onclick="closeRefreshModal()">
  <div class="modal-box" onclick="event.stopPropagation()">
    <div class="modal-title">Start the refresh server first</div>
    <p class="modal-body">In a separate R session, run this from your project root:</p>
    <code class="modal-code" onclick="copyModalCmd(this)">source("scripts/serve.R")</code>
    <p class="modal-hint">Click the code to copy &bull; Leave that R session running while you use the dashboard</p>
    <p class="modal-body">Then click <strong>Refresh</strong> again.</p>
    <br>
    <button class="modal-close" onclick="closeRefreshModal()">Got it</button>
  </div>
</div>

<!-- PAGE TABS -->
<!-- Four pages, each a set of sub-tabs. The grouping is by question asked:
     who are they and what is moving (Engagement), what are they using
     (Adoption), how is it running (Performance), how do we work
     (Documentation: the CS Doctrine and this page\'s user guide). -->
<div class="page-tabs">
  <button class="page-tab active" data-page="engagement" onclick="switchPage(\'engagement\')">
    Accounts and Engagement <span class="page-tab-count" id="tab-count-engagement">0</span>
  </button>
  <button class="page-tab" data-page="adoption" onclick="switchPage(\'adoption\')">
    Adoption <span class="page-tab-count" id="tab-count-adoption">—</span>
  </button>
  <button class="page-tab" data-page="performance" onclick="switchPage(\'performance\')">
    Performance <span class="page-tab-count" id="tab-count-performance">—</span>
  </button>
  <button class="page-tab" data-page="documentation" onclick="switchPage(\'documentation\')">
    Documentation
  </button>
</div>

<!-- ACCOUNTS AND ENGAGEMENT -->
<div id="page-engagement" class="page active">
  <div class="subpage">
    <div class="sub-tabs">
      <button class="sub-btn active" data-sub="accounts" onclick="switchSub(\'accounts\')">
        Accounts <span class="sub-count" id="sub-count-accounts">0</span>
      </button>
      <button class="sub-btn" data-sub="renewals" onclick="switchSub(\'renewals\')">
        Renewals <span class="sub-count" id="sub-count-renewals">0</span>
      </button>
      <button class="sub-btn" data-sub="expansions" onclick="switchSub(\'expansions\')">
        Expansions <span class="sub-count" id="sub-count-expansions">0</span>
      </button>
      <button class="sub-btn" data-sub="new_logos" onclick="switchSub(\'new_logos\')">
        New Logos <span class="sub-count" id="sub-count-new_logos">0</span>
      </button>
      <button class="sub-btn" data-sub="changes" onclick="switchSub(\'changes\')">
        Changes <span class="sub-count" id="sub-count-changes">0</span>
      </button>
      <button class="sub-btn" data-sub="coverage" onclick="switchSub(\'coverage\')">
        CS Coverage <span class="sub-count" id="sub-count-coverage">0</span>
      </button>
    </div>

    <!-- Accounts keeps its own sidebar rather than the pill rows the other
         panels use: it is the only view with the A-Z jump bar, which needs the
         vertical space. -->
    <div class="subpanel subpanel-row active" id="sub-panel-accounts">
      <div class="layout">
        <aside class="sidebar">
          <div class="sidebar-section">
            <div class="sidebar-label">Filter by Region</div>
            <div class="region-filters">
              <button class="pill active" data-region="all">All Regions</button>
              {{REGION_PILLS}}
            </div>
          </div>
          <div class="sidebar-section">
            <div class="sidebar-label">Filter by CSM</div>
            <div class="region-filters" id="csm-filters">
              <button class="pill active" data-csm="all">All CSMs</button>
              {{CSM_PILLS}}
            </div>
          </div>
          <div class="sidebar-section">
            <div class="sidebar-label">Sort by</div>
            <div class="region-filters">
              <button class="pill active" data-acctsort="name">Name (A&ndash;Z)</button>
              <button class="pill" data-acctsort="end_date">End Date</button>
            </div>
          </div>
          <div class="sidebar-section">
            <div class="sidebar-label" id="jump-label">Jump to Letter</div>
            <div class="alpha-jump" id="alpha-jump"></div>
          </div>
        </aside>
        <main class="main" id="main-content">
          <div id="account-list"></div>
          <div id="no-results">No accounts match your search.</div>
        </main>
      </div>
    </div>

    <!-- One collapsible section per CSM: book-of-business totals in the
         header, accounts with their four-square adoption glyph in the body.
         Totals mirror op_list_csm_coverage() but are computed here so they
         follow the region facet. -->
    <div class="subpanel" id="sub-panel-coverage">
      <div class="flat-page">
        <div class="flat-filters" id="coverage-region-filters"></div>
        <div class="flat-stats" id="coverage-stats"></div>
        <div class="flat-table-wrap" id="coverage-list"></div>
      </div>
    </div>

    <div class="subpanel" id="sub-panel-renewals">
      <div class="flat-page">
        <div class="flat-filters" id="renewal-filters"></div>
        <div class="flat-filters flat-filters-csm" id="renewal-csm-filters"></div>
        <div class="flat-stats" id="renewal-stats"></div>
        <div class="flat-table-wrap" id="renewal-table"></div>
      </div>
    </div>

    <div class="subpanel" id="sub-panel-expansions">
      <div class="flat-page">
        <div class="flat-filters" id="expansion-filters"></div>
        <div class="flat-filters flat-filters-csm" id="expansion-csm-filters"></div>
        <div class="flat-stats" id="expansion-stats"></div>
        <div class="flat-table-wrap" id="expansion-table"></div>
      </div>
    </div>

    <!-- New logos are prospects, not accounts on this page: region pills only
         (no CSM facet), and account names are not links to an account card. -->
    <div class="subpanel" id="sub-panel-new_logos">
      <div class="flat-page">
        <div class="flat-filters" id="new-logo-filters"></div>
        <div class="flat-stats" id="new-logo-stats"></div>
        <div class="flat-table-wrap" id="new-logo-table"></div>
      </div>
    </div>

    <div class="subpanel" id="sub-panel-changes">
      <div class="flat-page">
        <div class="changes-header" id="changes-header"></div>
        <div class="changes-body"   id="changes-content"></div>
      </div>
    </div>
  </div>
</div>

<!-- ADOPTION -->
<!-- Two sub-tabs: the overview (product adoption scatter over a sortable table
     of every adoption/consumption field) and the write-up of how the score and
     rating are derived. -->
<div id="page-adoption" class="page">
  <div class="subpage">
    <div class="sub-tabs">
      <button class="sub-btn active" data-sub="overview" onclick="switchSub(\'overview\')">
        Product Adoption Overview <span class="sub-count" id="sub-count-overview">—</span>
      </button>
      <button class="sub-btn" data-sub="derivation" onclick="switchSub(\'derivation\')">
        How the Product Adoption Score Is Derived
      </button>
      <button class="sub-btn" data-sub="agentic" onclick="switchSub(\'agentic\')">
        Agentic Engagement Overview <span class="sub-count" id="sub-count-agentic">—</span>
      </button>
      <button class="sub-btn" data-sub="critical_churn" onclick="switchSub(\'critical_churn\')">
        Critical Churn Risk <span class="sub-count" id="sub-count-critical_churn">—</span>
      </button>
    </div>

    <!-- One facet pair for the whole page rather than per sub-tab: activeRegion
         and activeCSM are global, so a region picked here follows you onto
         Accounts and Renewals, and back again. -->
    <div class="flat-filters" id="adopt-region-filters"></div>
    <div class="flat-filters flat-filters-csm" id="adopt-csm-filters"></div>

    <div class="subpanel active" id="sub-panel-overview">
      <div class="pbac-scatter-wrap">
        <div class="pbac-plot">
          <svg id="pbac-plot" viewBox="0 0 880 520" role="img"
               aria-label="Accounts plotted by consumption ratio and product adoption score"></svg>
          <div class="pbac-facet-note" id="pbac-facet-note"></div>
        </div>
        <aside class="pbac-side" id="pbac-side" aria-live="polite"></aside>
      </div>
      <div class="adopt-stats" id="adopt-stats-overview"></div>
      <div class="pa-table-wrap" id="adopt-table-overview"></div>
    </div>

    <div class="subpanel" id="sub-panel-derivation">
      <div class="pbac-writeup" id="pbac-writeup"></div>
    </div>

    <!-- Agentic profile from report 00OVV000007LHjN2AW (op_list$agentic_profile),
         built by the same rules as agentic_owner_summary.html: KPIs, the
         Coverage funnel and Conversation Status chart, evaluation / deployment
         status, then one expandable card per account with every field. The
         by-CSM table lives on Engagement > CS Coverage. -->
    <div class="subpanel" id="sub-panel-agentic">
{{AGX_PANEL}}
    </div>

    <!-- Black-rated accounts past their first six months: the Black cell of the
         product adoption grid on its own (same axes, cropped to that cell),
         labelled with tenure, over S1 and S2 tables. -->
    <!-- The .ccr-brief card is laid out to be screenshotted whole for the
         weekly ELT brief: title, as-of date, filter context, KPIs and chart in
         one frame. The toggle switches between the Black-cell plot and the
         ARR-at-risk-by-churn-quarter bars. -->
    <div class="subpanel" id="sub-panel-critical_churn">
      <!-- Churn quarter multi-select: filters the bars, the Black plot and
           the tables under them. No selection = the default view. -->
      <div class="flat-filters flat-filters-csm" id="ccr-quarter-filters"></div>
      <div class="ccr-wrap">
        <div class="ccr-brief">
          <div class="ccr-head">
            <div>
              <div class="ccr-eyebrow">Product adoption &middot; critical churn risk</div>
              <h2 class="ccr-title" id="ccr-title"></h2>
              <div class="ccr-sub" id="ccr-sub"></div>
            </div>
            <div class="ccr-toggle" role="tablist">
              <button class="ccr-tbtn active" data-view="bars" onclick="setCcrView(\'bars\')">ARR by churn quarter</button>
              <button class="ccr-tbtn" data-view="plot" onclick="setCcrView(\'plot\')">Black accounts</button>
            </div>
          </div>
          <div class="ccr-kpis" id="ccr-stats"></div>
          <div class="ccr-view" id="ccr-view-plot" style="display:none">
            <svg id="ccr-plot" viewBox="0 0 1000 500" role="img"
                 aria-label="Black-rated accounts by consumption ratio and product adoption score"></svg>
            <div class="ccr-legend">
              <span class="ccr-key"><svg viewBox="0 0 16 14"><circle cx="8" cy="7" r="5" fill="#111" stroke="#fff" stroke-width="1.2"/></svg> account (size = contract ARR)</span>
              <span class="ccr-key"><svg viewBox="0 0 16 14"><rect x="3" y="2" width="10" height="10" fill="#111" stroke="#fff" stroke-width="1.2"/></svg> lifecycle S1 (Installation and Config.)</span>
              <span class="ccr-key"><svg viewBox="0 0 16 14"><circle cx="8" cy="7" r="5" fill="#111" fill-opacity=".4" stroke="#111" stroke-dasharray="2 2"/></svg> consumption ratio unavailable (drawn at zero)</span>
              <span class="ccr-key"><span class="ccr-mo">38 mo</span> months as a customer</span>
            </div>
          </div>
          <div class="ccr-view" id="ccr-view-bars">
            <svg id="ccr-bars" viewBox="0 0 1000 500" role="img"
                 aria-label="Renewal ARR of Black and Red accounts by churn quarter"></svg>
            <div class="ccr-legend" id="ccr-bars-legend"></div>
          </div>
        </div>
      </div>
      <div class="ccr-tables" id="ccr-tables"></div>
    </div>
  </div>
</div>

<!-- PERFORMANCE -->
<div id="page-performance" class="page">
  <div class="subpage">
    <div class="sub-tabs">
      <button class="sub-btn active" data-sub="self_managed" onclick="switchSub(\'self_managed\')">
        Self-Managed <span class="sub-count" id="sub-count-self_managed">—</span>
      </button>
      <button class="sub-btn" data-sub="saas_migration" onclick="switchSub(\'saas_migration\')">
        SaaS Migration <span class="sub-count" id="sub-count-saas_migration">—</span>
      </button>
      <button class="sub-btn" data-sub="escalations" onclick="switchSub(\'escalations\')">
        Escalations <span class="sub-count" id="sub-count-escalations">—</span>
      </button>
    </div>

    <!-- Open CUSTX Jira issues. Severity pills first because that is the axis
         people triage on; customer and status narrow from there. -->
    <div class="subpanel" id="sub-panel-escalations">
      <div class="flat-filters" id="esc-sev-filters"></div>
      <div class="flat-filters flat-filters-csm" id="esc-status-filters"></div>
      <div class="flat-filters flat-filters-csm" id="esc-region-filters"></div>
      <div class="flat-filters flat-filters-csm" id="esc-csm-filters"></div>
      <div class="flat-stats" id="esc-stats"></div>
      <div class="esc-list-wrap"><div class="esc-list" id="esc-list"></div></div>
    </div>

    <!-- Migrations laid out as one column per estimated quarter. The question
         this answers is "what lands when", so the quarter is the axis and the
         stage is what a card carries, not the other way round. -->
    <div class="subpanel" id="sub-panel-saas_migration">
      <div class="flat-filters" id="saas-region-filters"></div>
      <div class="flat-filters flat-filters-csm" id="saas-csm-filters"></div>
      <div class="flat-stats" id="saas-stats"></div>
      <div class="saas-board-wrap"><div class="saas-board" id="saas-board"></div></div>
    </div>

    <div class="subpanel active" id="sub-panel-self_managed">
      <div class="flat-page">
        <div class="flat-filters" id="sm-support-filters">
          <button class="flat-pill active" data-sm-support="all" onclick="setSMSupport(\'all\')">All Support Statuses</button>
          <button class="flat-pill" data-sm-support="Supported" onclick="setSMSupport(\'Supported\')">Supported</button>
          <button class="flat-pill" data-sm-support="Unsupported" onclick="setSMSupport(\'Unsupported\')">Unsupported</button>
          <button class="flat-pill" data-sm-support="Unknown" onclick="setSMSupport(\'Unknown\')">Unknown</button>
        </div>
        <div class="flat-filters flat-filters-csm" id="sm-lifecycle-filters">
          <button class="flat-pill active" data-sm-lifecycle="all" onclick="setSMLifecycle(\'all\')">All Lifecycle Stages</button>
          {{SM_LIFECYCLE_PILLS}}
        </div>
        <div class="flat-stats" id="sm-stats"></div>
        <div class="flat-table-wrap" id="sm-table"></div>
      </div>
    </div>
  </div>
</div>

<!-- DOCUMENTATION -->
<!-- The CS Doctrine and the user guide are knitted from inst/guide/ (report_cs_doctrine(),
     report_user_guide()), embedded as base64 (DOCTRINE_B64 / GUIDE_B64) and shown in
     iframes built from Blobs on first view, so they keep their own styling and work in a
     downloaded copy. -->
<div id="page-documentation" class="page">
  <div class="subpage">
    <div class="doc-banner">
      <div class="doc-banner-text">
        <div class="doc-banner-label">Customer Success on Confluence</div>
        <div class="doc-banner-sub">Team process, playbooks and anything not covered here.</div>
      </div>
      <a class="doc-banner-link" href="https://immuta.atlassian.net/wiki/spaces/CSD/overview?homepageId=4118053175" target="_blank" rel="noopener">Open the CSD Confluence space &#8599;</a>
    </div>
    <div class="sub-tabs">
      <button class="sub-btn active" data-sub="doctrine" onclick="switchSub(\'doctrine\')">CS Doctrine</button>
      <button class="sub-btn" data-sub="reporting" onclick="switchSub(\'reporting\')">CSM Reporting</button>
      <button class="sub-btn" data-sub="guide" onclick="switchSub(\'guide\')">User Guide</button>
      <button class="doc-open" onclick="openDoc()" title="Open this document in its own tab">Open in new tab &#8599;</button>
    </div>
    <div class="subpanel active" id="sub-panel-doctrine">
      <iframe class="doc-frame" id="doc-frame-doctrine" title="CS Doctrine"></iframe>
      <div class="doc-missing" id="doc-missing-doctrine" hidden>
        The CS Doctrine was not built into this page. Run <code>report_cs_doctrine()</code> before
        <code>report_all_accounts_html()</code>, or open <a href="cs_doctrine.html" target="_blank" rel="noopener">cs_doctrine.html</a>.
      </div>
    </div>
    <div class="subpanel" id="sub-panel-reporting">
      <iframe class="doc-frame" id="doc-frame-reporting" title="CSM Reporting Requirements"></iframe>
      <div class="doc-missing" id="doc-missing-reporting" hidden>
        CSM Reporting was not built into this page. Run <code>report_csm_reporting()</code> before
        <code>report_all_accounts_html()</code>, or open <a href="csm_reporting.html" target="_blank" rel="noopener">csm_reporting.html</a>.
      </div>
    </div>
    <div class="subpanel" id="sub-panel-guide">
      <iframe class="doc-frame" id="doc-frame-guide" title="User Guide and Definitions"></iframe>
      <div class="doc-missing" id="doc-missing-guide" hidden>
        The user guide was not built into this page. Run <code>report_user_guide()</code> before
        <code>report_all_accounts_html()</code>, or open <a href="immuta_accounts_guide.html" target="_blank" rel="noopener">immuta_accounts_guide.html</a>.
      </div>
    </div>
  </div>
</div>

<script>
const ACCOUNTS          = {{JSON_DATA}};
const SNAP_DATES        = {{SNAP_DATES}};
const CHANGES_DATA      = {{CHANGES}};
const CONSUMPTION_DATA  = {{CONSUMPTION}};
const SAAS_DATA         = {{SAAS}};
const ESCALATIONS_DATA  = {{ESCALATIONS}};
const NEW_LOGOS_DATA    = {{NEW_LOGOS}};
const AGENTIC_DATA      = {{AGENTIC}};
const GUIDE_B64         = "{{GUIDE_B64}}";
const DOCTRINE_B64      = "{{DOCTRINE_B64}}";
const REPORTING_B64     = "{{REPORTING_B64}}";

// ── Documentation tab ─────────────────────────────────────────────────────
// The embedded pages are decoded once, on first view, into the iframe\'s
// srcdoc (works in sandboxed and file:// contexts, where Blob-URL frames can be
// blocked) and, on demand, a Blob URL for "Open in new tab". Either way a
// downloaded copy of this page works with nothing beside it. Section links
// inside a srcdoc frame are handled by the page itself (c360_doc_head.html).
const DOC_B64  = { doctrine: DOCTRINE_B64, reporting: REPORTING_B64, guide: GUIDE_B64 };
const DOC_FILE = { doctrine: "cs_doctrine.html", reporting: "csm_reporting.html",
                   guide: "immuta_accounts_guide.html" };
const docBytes = {};
const docUrls  = {};

function docData(key) {
  if (docBytes[key]) return docBytes[key];
  const b64 = DOC_B64[key];
  if (!b64) return null;
  const bin   = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return (docBytes[key] = bytes);
}

function docUrl(key) {
  if (docUrls[key]) return docUrls[key];
  const bytes = docData(key);
  if (!bytes) return null;
  return (docUrls[key] = URL.createObjectURL(new Blob([bytes], { type: "text/html;charset=utf-8" })));
}

function renderDocumentation() {
  const key   = PAGES.documentation.active;
  const frame = document.getElementById("doc-frame-" + key);
  const bytes = docData(key);
  document.getElementById("doc-missing-" + key).hidden = !!bytes;
  frame.hidden = !bytes;
  if (bytes && !frame.dataset.loaded) {
    frame.srcdoc = new TextDecoder("utf-8").decode(bytes);
    frame.dataset.loaded = "1";
  }
}

function openDoc() {
  const key = PAGES.documentation.active;
  window.open(docUrl(key) || DOC_FILE[key], "_blank");
}

// ── State ──────────────────────────────────────────────────────────────────
let activeRegion    = "all";
let activeCSM       = "all";
let searchTerm      = "";
let activePage      = "engagement";
let renewalSort         = { col: "end_date_raw", dir: 1 };
// Accounts sub-tab order: "name" (A-Z, grouped by letter) or "end_date"
// (next renewal end date, grouped by that renewal\'s churn quarter).
let accountSort         = "name";
let expansionSort       = { col: "close_date_raw", dir: 1 };
let newLogoSort         = { col: "close_date_raw", dir: 1 };
let adoptSort           = { col: null, dir: 1 };
let smSort              = { col: null, dir: 1 };
let activeSMSupport     = "all";
let activeSMLifecycle   = "all";
const SM_COLS = [
  "Account Name", "Immuta Version", "Support Status", "Lifecycle Stage",
  "Registered Users", "Licensed Users", "Consumption Ratio",
  "Data Sources Count", "Active Policies (Total)"
];
// ── Flat data derived from ACCOUNTS ────────────────────────────────────────
const ALL_RENEWALS   = [];
const ALL_EXPANSIONS = [];
ACCOUNTS.forEach(a => {
  a.renewals.forEach(r => ALL_RENEWALS.push(
    Object.assign({}, r, { account: a.name, region: a.region, csm: a.csm })
  ));
  a.expansions.forEach(e => ALL_EXPANSIONS.push(
    Object.assign({}, e, { account: a.name, region: a.region, csm: a.csm })
  ));
});

// Update global tab counts once
setCount("tab-count-engagement", ACCOUNTS.length);
setCount("sub-count-accounts",   ACCOUNTS.length);
setCount("sub-count-renewals",   ALL_RENEWALS.length);
setCount("sub-count-expansions", ALL_EXPANSIONS.length);
const ALL_NEW_LOGOS = (NEW_LOGOS_DATA && NEW_LOGOS_DATA.rows) || [];
setCount("sub-count-new_logos",  ALL_NEW_LOGOS.length);
setCount("sub-count-changes",
  (CHANGES_DATA && CHANGES_DATA.has_data && CHANGES_DATA.summary)
    ? CHANGES_DATA.summary.reduce((s, r) => s + (r.Total_Moves || 0), 0)
    : 0);
// Adoption and Performance don\'t depend on a render pass to be countable, so
// fill their badges up front rather than leaving an em-dash until first visit.
setCount("tab-count-adoption", ACCOUNTS.length);
{
  const sm = ((CONSUMPTION_DATA && CONSUMPTION_DATA.rows) || []).filter(r =>
    !(r["Is SaaS Customer"] === true || r["Is SaaS Customer"] === "TRUE")
  ).length;
  setCount("tab-count-performance",  sm);
  setCount("sub-count-self_managed", sm);
  setCount("sub-count-saas_migration", ((SAAS_DATA && SAAS_DATA.rows) || []).length);
  setCount("sub-count-escalations", ((ESCALATIONS_DATA && ESCALATIONS_DATA.rows) || []).length);
}

// ── Helpers ────────────────────────────────────────────────────────────────
function esc(s) {
  if (!s) return "";
  return String(s)
    .replace(/&/g,"&amp;").replace(/</g,"&lt;")
    .replace(/>/g,"&gt;").replace(/"/g,"&quot;");
}

function fmtBool(v) {
  if (v === true)  return \'<span class="arch-badge arch-yes">&#10003; Yes</span>\';
  if (v === false) return \'<span class="arch-badge arch-no">No</span>\';
  return \'<span class="arch-badge arch-no">—</span>\';
}

function fieldHtml(label, value, extraClass) {
  const v = (value === null || value === undefined || value === "" || value === "\\u2014")
    ? \'<span class="em-dash">—</span>\' : esc(value);
  return `<div class="field-item">
    <div class="field-label">${label}</div>
    <div class="field-value${extraClass ? \' \' + extraClass : \'\'}">${v}</div>
  </div>`;
}

// Customer age for account summaries: "50 months (since March 03, 2022)".
// Same wording as fmt_tenure() in R/utils_internal.R; em-dash when unknown.
function tenureText(a) {
  const m = covNum(a.months_as_customer);
  if (m === null) return "\\u2014";
  const since = adoptBlank(a.customer_since) ? "" : ` (since ${a.customer_since})`;
  return `${m} month${m === 1 ? "" : "s"}${since}`;
}

function fieldHtmlRaw(label, innerHtml) {
  return `<div class="field-item">
    <div class="field-label">${label}</div>
    <div class="field-value">${innerHtml}</div>
  </div>`;
}

// Compact label:value row for the three health-summary columns. The stacked
// label-over-value of fieldHtml() is too tall once a column carries 6-8 rows.
function pairHtml(label, innerHtml, bad) {
  return `<div class="ov-pair">
    <span class="ov-pair-k">${label}</span>
    <span class="ov-pair-v${bad ? \' bad\' : \'\'}">${innerHtml}</span>
  </div>`;
}

// Adoption row: label + stoplight on the first line, then the definition text
// from the picklist underneath. The leading colour word is stripped because the
// stoplight already says it — "Black: Less than 25% consumption" renders as a
// black dot labelled Black, with "Less than 25% consumption" beneath.
function adoptionHtml(label, v) {
  const raw = (v === null || v === undefined) ? "" : String(v).trim();
  const blank = !raw || raw === "-" || raw === "\\u2014";
  const def = blank ? "" : raw.replace(/^(green|amber|yellow|red|black)\\b[\\s:\\u2013\\u2014-]*/i, "");
  return `<div class="ov-adopt">
    <div class="ov-adopt-top">
      <span class="ov-pair-k">${label}</span>
      <span class="ov-pair-v">${fmtStoplight(v)}</span>
    </div>
    ${def ? `<div class="ov-adopt-def">${esc(def)}</div>` : ""}
  </div>`;
}

// True for the placeholders a missing value can arrive as.
function adoptBlank(v) {
  if (v === null || v === undefined) return true;
  const s = String(v).trim();
  return !s || s === "-" || s === "\\u2014" || s === "null";
}

function adoptCap(s) {
  const v = String(s).trim();
  return v ? v.charAt(0).toUpperCase() + v.slice(1) : v;
}

// Label half of a "<colour>: <label>" rating, sentence-cased:
// "amber: moderate adoption" -> "Moderate adoption".
function ratingLabel(v) {
  if (adoptBlank(v)) return "";
  return adoptCap(String(v).trim().replace(/^(green|amber|yellow|red|black)\\s*:\\s*/i, ""));
}

// The stoplight badge, but labelled with caller-supplied text instead of the
// colour word. Used wherever a derived value names its own state ("Tried",
// "Moderate adoption") and the colour is carrying severity only.
function fmtStoplightAs(colourSource, text, title) {
  const raw = adoptBlank(colourSource) ? "" : String(colourSource).trim();
  const m = raw.match(/^(green|amber|yellow|red|black)/i);
  if (!m || !text) return \'<span class="em-dash">—</span>\';
  const c = m[1].toLowerCase() === "yellow" ? "amber" : m[1].toLowerCase();
  return `<span class="stoplight-badge" title="${esc(title || raw)}"><span class="stoplight-dot stoplight-${c}"></span>${esc(text)}</span>`;
}

// Request: the stage tile an account appears on in Looker dashboard 1009 IS
// the rating, so the dot carries severity and the stage word is the label —
// "Tried", "Adopted", "Retained", "None". No definition line: the stage word
// already is the definition. Falls back to the Salesforce picklist when the
// pipeline ran without Looker.
function requestAdoptionHtml(a) {
  const drv = a.request_adoption_derived;
  if (adoptBlank(drv)) return adoptionHtml("Request", a.request_adoption);

  const stage = adoptBlank(a.request_status) ? "" : adoptCap(a.request_status);

  return `<div class="ov-adopt">
    <div class="ov-adopt-top">
      <span class="ov-pair-k">Request</span>
      <span class="ov-pair-v">${fmtStoplightAs(drv, stage || ratingLabel(drv), drv)}</span>
    </div>
  </div>`;
}

// PBAC: rated from op_list$consumption$pbac_adoption, with the evidence for
// the rating listed underneath — which platforms are in play and which
// features are in use. Platforms come through as pre-built HTML (dev-only
// platforms greyed), so that string is inserted rather than escaped.
function pbacAdoptionHtml(a) {
  const r = a.pbac_rating;
  if (adoptBlank(r)) return adoptionHtml("PBAC", a.pbac_adoption);

  const none  = \'<span class="ov-adopt-none">None</span>\';
  const plats = (a.platform_html && a.platform_html !== "\\u2014") ? a.platform_html : none;
  const feats = adoptBlank(a.pbac_features) ? none : esc(a.pbac_features);
  // From data/input/critical_technologies.json (accounts$iam / $ext_catalog).
  const iam   = adoptBlank(a.iam)         ? none : esc(a.iam);
  const cat   = adoptBlank(a.ext_catalog) ? none : esc(a.ext_catalog);

  // The complexity score is the plot\'s y axis and half the rating rule, so it
  // belongs with the evidence rather than buried in the tooltip. Shown out of
  // its maximum: a bare "3" reads as a rank, "3 / 7" reads as a measurement.
  const score = (a.pbac_score === null || a.pbac_score === undefined || isNaN(a.pbac_score))
    ? none
    : `${a.pbac_score} / ${PBAC_CFG.scoreMax}`;

  return `<div class="ov-adopt">
    <div class="ov-adopt-top">
      <span class="ov-pair-k">PBAC</span>
      <span class="ov-pair-v">${pbacBadge(a)}</span>
    </div>
    <div class="ov-adopt-sub ov-adopt-sub-pbac">
      <div class="ov-adopt-subrow"><span class="ov-adopt-subk">Platforms</span><span class="ov-adopt-subv">${plats}</span></div>
      <div class="ov-adopt-subrow"><span class="ov-adopt-subk">IAM</span><span class="ov-adopt-subv">${iam}</span></div>
      <div class="ov-adopt-subrow"><span class="ov-adopt-subk">External Catalogue</span><span class="ov-adopt-subv">${cat}</span></div>
      <div class="ov-adopt-subrow"><span class="ov-adopt-subk">Features</span><span class="ov-adopt-subv">${feats}</span></div>
      <div class="ov-adopt-subrow"><span class="ov-adopt-subk">Complexity</span><span class="ov-adopt-subv">${score}</span></div>
    </div>
  </div>`;
}

// ── Overview: per-account product adoption plot ────────────────────────────
// The Adoption tab\'s scatter at card size (product adoption score 0-12 against
// consumption ratio), with only this account drawn. The bands come from
// paRatingAt() rather than a copied colour table, so moving a threshold moves
// both plots together. Same marker convention as the Adoption page: a square
// for an account in lifecycle S1, a circle otherwise; dashed outline when the
// ratio is unknown, heavy outline when it is off the right of the axis.
const ADOPT_MINI = { w: 210, h: 140, l: 20, r: 7, t: 7, b: 16, pin: 13 };

function adoptionMiniSvg(a) {
  if (!hasProduct(a)) {
    return `<div class="ov-mini-empty">No product adoption score \\u2014 needs a Looker run.</div>`;
  }

  const M = ADOPT_MINI;
  const pw = M.w - M.l - M.r, ph = M.h - M.t - M.b;
  const X = v => M.l + Math.min(v, PA_CFG.ratioMax) / PA_CFG.ratioMax * (pw - M.pin);
  const Y = s => M.t + ph - (Math.min(s, PA_CFG.scoreMax) / PA_CFG.scoreMax) * ph;

  // Six bands, same row/column split as the full plot.
  const xEdges = [0, PA_CFG.ratioMinimal, PA_CFG.ratioModerate, PA_CFG.ratioMax];
  let bands = "";
  for (let r = 0; r < 2; r++) {
    for (let c = 0; c < 3; c++) {
      const key  = paRatingAt(r === 1 ? PA_CFG.scoreMax : 0, (xEdges[c] + xEdges[c + 1]) / 2);
      const x0   = X(xEdges[c]);
      const x1   = c === 2 ? M.l + pw : X(xEdges[c + 1]);
      const yTop = Y(r === 1 ? PA_CFG.scoreMax : PA_CFG.adoptionMin);
      const yBot = Y(r === 1 ? PA_CFG.adoptionMin : 0);
      bands += `<rect x="${x0}" y="${yTop}" width="${x1 - x0}" height="${yBot - yTop}" fill="${PBAC_COLOURS[key].bg}"/>`;
    }
  }

  const ratio   = a.consumption_ratio_raw;
  const missing = ratio === null || ratio === undefined || isNaN(ratio);
  const over    = !missing && ratio > PA_CFG.ratioMax;
  const colour  = ratingColour(a.product_rating) || "black";
  const score   = covNum(a.product_adoption);

  // Held a dot-radius inside the frame so an extreme value is not sliced in
  // half by the border; the nudge is smaller than the dot, so it cannot move a
  // point across a band edge.
  const R = 4.5;
  const clamp = (v, lo, hi) => Math.min(Math.max(v, lo), hi);
  const cx = clamp(missing ? X(0) : (over ? M.l + pw - M.pin / 2 : X(ratio)),
                   M.l + R, M.l + pw - R);
  const cy = clamp(Y(score), M.t + R, M.t + ph - R);

  const tip = `${a.name} \\u2014 product adoption ${score} / ${PA_CFG.scoreMax}, ratio ${a.consumption_ratio}` +
              (isInstallStage(a) ? " (lifecycle S1)" : "");
  const cls = `ov-mini-dot${missing ? " missing" : ""}${over ? " over" : ""}`;

  return `<svg class="ov-mini-plot" viewBox="0 0 ${M.w} ${M.h}" role="img" aria-label="${esc(tip)}">
    <title>${esc(tip)}</title>
    ${bands}
    <line x1="${X(PA_CFG.ratioMax)}" y1="${M.t}" x2="${X(PA_CFG.ratioMax)}" y2="${M.t + ph}" class="ov-mini-pin"/>
    <line x1="${M.l}" y1="${Y(PA_CFG.adoptionMin)}" x2="${M.l + pw}" y2="${Y(PA_CFG.adoptionMin)}" class="ov-mini-grid"/>
    <rect x="${M.l}" y="${M.t}" width="${pw}" height="${ph}" class="ov-mini-frame"/>
    ${isInstallStage(a)
      ? `<rect x="${cx - R * 0.885}" y="${cy - R * 0.885}" width="${R * 1.77}" height="${R * 1.77}" fill="${PBAC_COLOURS[colour].dot}" class="${cls}"/>`
      : `<circle cx="${cx}" cy="${cy}" r="${R}" fill="${PBAC_COLOURS[colour].dot}" class="${cls}"/>`}
    <text x="${M.l - 4}" y="${Y(PA_CFG.scoreMax) + 3}" class="ov-mini-tick" text-anchor="end">${PA_CFG.scoreMax}</text>
    <text x="${M.l - 4}" y="${Y(PA_CFG.adoptionMin) + 3}" class="ov-mini-tick" text-anchor="end">${PA_CFG.adoptionMin}</text>
    <text x="${M.l - 4}" y="${Y(0) + 3}" class="ov-mini-tick" text-anchor="end">0</text>
    <text x="${M.l}" y="${M.h - 4}" class="ov-mini-tick" text-anchor="start">0</text>
    <text x="${M.l + pw}" y="${M.h - 4}" class="ov-mini-tick" text-anchor="end">&gt;${PA_CFG.ratioMax}</text>
  </svg>
  <div class="ov-mini-caption" style="color:${PBAC_COLOURS[colour].ink}">${esc(pbacFullLabel(a.product_rating))}</div>`;
}

// PBAC rating badge with the scatter plot\'s labels and symbology: the full
// label ("Red: High Risk - Adoption") and a square marker for accounts in
// Installation and Config., a round one otherwise.
function pbacBadge(a) {
  const html = fmtStoplightAs(a.pbac_rating, pbacFullLabel(a.pbac_rating), a.pbac_rating);
  return isInstallStage(a)
    ? html.replace("stoplight-dot ", "stoplight-dot square ")
    : html;
}

// The stored rating as a full caption, colour word included and capitalised:
// "Red: High Risk - Adoption", "Amber: Moderate adoption".
function pbacFullLabel(v) {
  if (adoptBlank(v)) return "";
  const m = String(v).trim().match(/^(green|amber|yellow|red|black)\\s*:\\s*(.*)$/i);
  if (!m) return String(v).trim();
  return adoptCap(m[1].toLowerCase()) + ": " + adoptCap(m[2]);
}

// ── Overview: open escalation tiles ────────────────────────────────────────
// Read from ESCALATIONS_DATA at render time rather than joined onto the account
// record, so the card and the Escalations tab cannot fall out of step. Sorted
// the same way that tab sorts: severity first, then oldest.
function escTilesHtml(a) {
  const rows = ((ESCALATIONS_DATA && ESCALATIONS_DATA.rows) || [])
    .filter(r => r["Account Name"] === a.name)
    .sort((x, y) => (x.severity_rank - y.severity_rank) || ((y.age_days || 0) - (x.age_days || 0)));

  if (!rows.length) {
    // An account with a non-zero count but no rows means the Jira pull did not
    // run, which is a different thing from having nothing open.
    return a.custx_open_raw > 0
      ? `<div class="ov-mini-empty">${a.custx_open_raw} open, detail not loaded</div>`
      : "";
  }

  return `<div class="ov-esc-tiles">` + rows.map(r => {
    const rank = Math.min(Math.max(r.severity_rank || 4, 1), 4);
    const stale = (r.age_days || 0) >= 90;
    return `<a class="ov-esc-tile sev-${rank}" href="${esc(r.url)}" target="_blank" rel="noopener"
               title="${esc(r.summary || "")}" onclick="event.stopPropagation()">
      <span class="ov-esc-line">
        <span class="esc-sev esc-sev-${rank}">${esc(r.severity || "\\u2014")}</span>
        <span class="ov-esc-key">${esc(r.key)}</span>
        <span class="ov-esc-age${stale ? " stale" : ""}">${r.age_days != null ? r.age_days + "d" : ""}</span>
      </span>
      <span class="ov-esc-sum">${esc(r.summary || "")}</span>
    </a>`;
  }).join("") + `</div>`;
}

// ── Overview: relationship strength ────────────────────────────────────────
// Salesforce will supply strong / neutral / weak. Mapped onto the same three
// stoplight colours every other rating on the card uses, so the column reads
// consistently rather than introducing a fourth visual language.
const STRENGTH_COLOUR = { strong: "green", neutral: "amber", weak: "red" };

function strengthValue(v) {
  if (adoptBlank(v)) return `<span class="em-dash">\\u2014</span>`;
  const key    = String(v).trim().toLowerCase();
  const colour = STRENGTH_COLOUR[key];
  // An unrecognised value is shown as-is rather than silently dropped: the
  // picklist gaining a fourth option should be visible, not invisible.
  if (!colour) return esc(v);
  return fmtStoplightAs(colour, adoptCap(key), adoptCap(key));
}

// A contact role — who holds it, then how strong the relationship is and how
// much influence they carry, indented beneath. Same shape for every role, so
// Executive Buyer and Champion stay comparable at a glance; same indent idiom
// as the PBAC evidence block, so the card has one way of saying "this
// supports the line above" rather than two.
function contactRoleHtml(label, name, relationship, influence) {
  return `<div class="ov-adopt">
    <div class="ov-adopt-top">
      <span class="ov-pair-k">${label}</span>
      <span class="ov-pair-v">${adoptBlank(name) ? `<span class="em-dash">\\u2014</span>` : esc(name)}</span>
    </div>
    <div class="ov-adopt-sub">
      <div class="ov-adopt-subrow"><span class="ov-adopt-subk">Relationship</span><span class="ov-adopt-subv">${strengthValue(relationship)}</span></div>
      <div class="ov-adopt-subrow"><span class="ov-adopt-subk">Influence</span><span class="ov-adopt-subv">${strengthValue(influence)}</span></div>
    </div>
  </div>`;
}

// The same SaaS / SM: Supported / SM: Unsupported badge used in the card header
// and on the PNG health summary, so all three describe an account identically.
function deployTag(a) {
  if (a.is_saas) return \'<span class="tag tag-saas">SaaS</span>\';
  return a.unsupported
    ? \'<span class="tag tag-sm-unsup">SM: Unsupported</span>\'
    : \'<span class="tag tag-sm-sup">SM: Supported</span>\';
}

function fmtStoplight(v) {
  const raw = (v === null || v === undefined) ? "" : String(v).trim();
  if (!raw || raw === "-" || raw === "\\u2014") return \'<span class="em-dash">—</span>\';
  // The Salesforce picklist value is a full sentence, e.g. "Green (any 2): ..."
  // — pull the leading color word and keep the rest as a hover tooltip.
  // The picklists say AMBER, not "yellow" — omitting it rendered every amber
  // account as an em-dash. "yellow" stays as an alias.
  const m = raw.match(/^(green|amber|yellow|red|black)\\b/i);
  const labels = { green: "Green", amber: "Amber", yellow: "Yellow", red: "Red", black: "Black" };
  if (!m) return `<span class="em-dash" title="${esc(raw)}">—</span>`;
  const color = m[1].toLowerCase();
  return `<span class="stoplight-badge" title="${esc(raw)}"><span class="stoplight-dot stoplight-${color}"></span>${labels[color]}</span>`;
}

// Four-square adoption glyph for the card header. Each quadrant uses the same
// rating source the Overview tab shows, falling back to the Salesforce
// picklist when the derived rating is missing.
function adoptionQuadHtml(a) {
  const pbac    = adoptBlank(a.pbac_rating) ? a.pbac_adoption : a.pbac_rating;
  const request = adoptBlank(a.request_adoption_derived) ? a.request_adoption : a.request_adoption_derived;
  const cells = [
    ["PBAC",    pbac],
    ["Request", request],
    ["Comply",  a.comply_adoption],
    ["Agentic", a.agentic_governance_adoption]
  ];
  // Each square carries its own tooltip naming the rating it shows.
  return `<div class="adopt-quad">` +
    cells.map(([k, v]) => {
      const c   = ratingColour(v);
      const tip = `${k} Adoption: ${c ? adoptCap(c) : "\\u2014"}`;
      return `<span data-tip="${esc(tip)}" aria-label="${esc(tip)}"${c ? ` class="stoplight-${c}"` : ""}></span>`;
    }).join("") +
    `</div>`;
}

// ── Build cards ────────────────────────────────────────────────────────────
function buildCard(a) {
  const hasRenewals   = a.renewals.length > 0;
  const hasExpansions = a.expansions.length > 0;

  // Renewal rows
  let renewRows = "";
  if (hasRenewals) {
    renewRows = a.renewals.map(r => `
      <tr>
        <td>${r.url ? \'<a class="opp-link" href="\' + esc(r.url) + \'" target="_blank" rel="noopener">\' + esc(r.name) + \'</a>\' : esc(r.name)}</td>
        <td>${esc(r.amount)}</td>
        <td>${esc(r.end_date)}</td>
        <td><strong>${esc(r.churn_quarter)}</strong></td>
        <td>${esc(r.next_steps)}</td>
      </tr>`).join("");
  }

  // Expansion rows
  let expRows = "";
  if (hasExpansions) {
    expRows = a.expansions.map(e => `
      <tr>
        <td>${e.url ? \'<a class="opp-link" href="\' + esc(e.url) + \'" target="_blank" rel="noopener">\' + esc(e.name) + \'</a>\' : esc(e.name)}</td>
        <td>${esc(e.owner)}</td>
        <td>${esc(e.amount)}</td>
        <td>${esc(e.fiscal_period)}</td>
        <td>${esc(e.next_steps)}</td>
      </tr>`).join("");
  }

  // Products rows
  let prodRows = "";
  if (a.products.length > 0) {
    prodRows = a.products.map(p => `
      <tr><td>${esc(p.name)}</td><td>${esc(p.quantity)}</td></tr>`).join("");
  }

  // Header tags
  const regionTag = a.region && a.region !== "\\u2014"
    ? `<span class="tag tag-region">${esc(a.region)}</span>` : "";
  const saasTag = a.is_saas
    ? \'<span class="tag tag-saas">SaaS</span>\' : "";
  const smTag = !a.is_saas
    ? (a.unsupported
        ? \'<span class="tag tag-sm-unsup">SM: Unsupported</span>\'
        : \'<span class="tag tag-sm-sup">SM: Supported</span>\')
    : "";
  // Customer age (months since the first contract start), like the other tags.
  const tenureTag = covNum(a.months_as_customer) === null ? ""
    : `<span class="tag tag-region" title="Customer since ${esc(a.customer_since)}">${covNum(a.months_as_customer)} mo</span>`;
  // OBO session revenue target: the account has an agentic PoV (accounts$OBO_revenue_TGT).
  const oboTag = a.obo_revenue_tgt
    ? `<span class="tag tag-obo" title="OBO_revenue_TGT: active agentic PoV">OBO target</span>` : "";
  const churnTag = a.churn_summary
    ? `<span class="tag tag-churn">${esc(a.churn_summary)}</span>` : "";

  return `
<div class="card" id="acct-${esc(a.id)}" data-region="${esc(a.region)}" data-name="${esc(a.name.toLowerCase())}">
  <div class="card-header" onclick="toggleCard(this)">
    ${adoptionQuadHtml(a)}
    <div class="card-name">${esc(a.name)}${a.multiplatform ? \' <span class="mp-badge">(M)</span>\' : \'\'}</div>
    <div class="card-arr">${esc(a.arr)}</div>
    ${a.consumption_link}
    ${a.customer_central_url ? `<a class="cc-link" href="${esc(a.customer_central_url)}" target="_blank" rel="noopener" onclick="event.stopPropagation()">Customer Central &#8599;</a>` : \'\'}
    <div class="card-meta">
      ${regionTag}${tenureTag}${oboTag}${saasTag}${smTag}${churnTag}
    </div>
    <span class="expand-icon">&#9660;</span>
  </div>
  <div class="card-body">
    <div class="tabs">
      <button class="tab-btn active" onclick="switchTab(this,\'overview-${esc(a.id)}\')">Overview</button>
      <button class="tab-btn" onclick="switchTab(this,\'arch-${esc(a.id)}\')">Architecture${a.products.length > 0 ? \' (\' + a.products.length + \')\' : \'\'}</button>
      <button class="tab-btn" onclick="switchTab(this,\'renew-${esc(a.id)}\')">Renewals${hasRenewals ? \' (\' + a.renewals.length + \')\' : \'\'}</button>
      <button class="tab-btn" onclick="switchTab(this,\'expand-${esc(a.id)}\')">Expansions${hasExpansions ? \' (\' + a.expansions.length + \')\' : \'\'}</button>
    </div>

    <!-- Overview tab -->
    <div class="tab-pane active" id="overview-${esc(a.id)}">
      <div class="overview-cols">
        <div class="overview-col">
          ${fieldHtml("Contract ARR", a.arr, "arr-value")}
          ${fieldHtml("Customer for", tenureText(a))}
          ${fieldHtml("Region", a.region)}
          ${fieldHtml("Customer Success Manager", a.csm)}
          ${fieldHtml("Account Owner", a.account_owner)}
          ${fieldHtml("Field CTO", a.field_cto)}
        </div>

        <!-- Adoption runs across two columns: the product adoption plot (the
             Adoption page scatter, this account only) with the lifecycle stage
             and the consumption figures behind its x axis; then the per-product
             ratings (PBAC, Request, Agentic, Comply). -->
        <div class="overview-col">
          <div class="ov-head">Adoption</div>
          ${adoptionMiniSvg(a)}
          ${fieldHtml("Lifecycle Stage", a.lifecycle_stage)}
          ${pairHtml("Registered", esc(a.registered_users))}
          ${pairHtml("Licensed", esc(a.licensed_users))}
          ${pairHtml("Ratio", `<span class="${pctClass(a.consumption_ratio_raw)}">${esc(a.consumption_ratio)}</span>`)}
        </div>
        <div class="overview-col">
          <div class="ov-head">Products</div>
          ${pbacAdoptionHtml(a)}
          ${adoptBlank(a.pbac_rating) ? pairHtml("Platforms", a.platform_html) : ""}
          ${requestAdoptionHtml(a)}
          ${adoptionHtml("Agentic", a.agentic_governance_adoption)}
          ${adoptionHtml("Comply", a.comply_adoption)}
        </div>

        <div class="overview-col">
          <div class="ov-head">Performance</div>
          <div>${deployTag(a)}</div>
          ${pairHtml("Version", esc(a.immuta_version))}
          ${pairHtml("Connections", esc(a.connections))}
          ${pairHtml("Incidents 90d", esc(a.inc_90d))}
          ${pairHtml("Incidents 360d", esc(a.inc_360d))}
          ${pairHtml("CustX 90d", esc(a.custx_90d), a.custx_90d_raw > 0)}
          ${pairHtml("CustX open", esc(a.custx_open), a.custx_open_raw > 0)}
          ${escTilesHtml(a)}
        </div>

        <div class="overview-col">
          <div class="ov-head">Engagement</div>
          ${contactRoleHtml("Executive Buyer", a.executive_buyer, a.buyer_relationship, a.buyer_influence)}
          ${contactRoleHtml("Champion", a.champion, a.champion_relationship, a.champion_influence)}
          ${pairHtml("Last QBR", esc(a.last_qbr))}
          ${pairHtml("Next QBR", esc(a.next_qbr))}
          ${fieldHtmlRaw("QBR Note", `<span class="ov-note">${esc(a.qbr_note)}</span>`)}
        </div>
      </div>
    </div>

    <!-- Architecture tab -->
    <div class="tab-pane" id="arch-${esc(a.id)}">
      <div style="margin-bottom:12px">
        ${fmtBool(a.is_saas).replace("Yes","SaaS Customer").replace("No","Self-Hosted")}
        ${a.unsupported ? \'<span class="arch-badge arch-warn">&#9888; Unsupported Version</span>\' : \'\'}
      </div>
      ${a.products.length > 0 ? `
      <table class="data-table">
        <thead><tr><th>Product</th><th>Qty</th></tr></thead>
        <tbody>${prodRows}</tbody>
      </table>` : \'<p class="empty-msg">No active orders on record.</p>\'}
    </div>

    <!-- Renewals tab -->
    <div class="tab-pane" id="renew-${esc(a.id)}">
      ${hasRenewals ? `
      <table class="data-table">
        <thead><tr><th>Opportunity</th><th>Amount</th><th>End Date</th><th>Churn Quarter</th><th>Next Steps</th></tr></thead>
        <tbody>${renewRows}</tbody>
      </table>` : \'<p class="empty-msg">No open renewals.</p>\'}
    </div>

    <!-- Expansions tab -->
    <div class="tab-pane" id="expand-${esc(a.id)}">
      ${hasExpansions ? `
      <table class="data-table">
        <thead><tr><th>Opportunity</th><th>Owner</th><th>Amount</th><th>Fiscal Period</th><th>Next Steps</th></tr></thead>
        <tbody>${expRows}</tbody>
      </table>` : \'<p class="empty-msg">No open expansion opportunities.</p>\'}
    </div>
  </div>
</div>`;
}

// ── CS Coverage ────────────────────────────────────────────────────────────
// One collapsible section per CSM. The per-CSM rules mirror
// op_list_csm_coverage() in R/op_list_csm_coverage.R, recomputed here so the
// totals follow the region facet. Keep the two in step.
const coverageOpen = new Set();

// Lifecycle stage 1 — "S1: Installation and Config." in Salesforce.
function covIsOnboarding(a) {
  const s = adoptBlank(a.lifecycle_stage) ? "" : String(a.lifecycle_stage);
  return /^\\s*S1\\b|install|onboard/i.test(s);
}

// Looker stage where the account has one, else the SFDC picklist colour —
// the same precedence the card glyph uses.
function covRequestStage(a) {
  if (!adoptBlank(a.request_status)) return String(a.request_status).trim().toLowerCase();
  return { green: "retained", amber: "adopted", red: "tried", black: "none" }[ratingColour(a.request_adoption)] || "";
}

function covNum(v) {
  const n = Number(v);
  return (v === null || v === undefined || v === "" || !isFinite(n)) ? null : n;
}

function covSummarise(list) {
  const scores = list.map(a => covNum(a.pbac_score)).filter(n => n !== null);
  return {
    accounts:   list.length,
    arr:        list.reduce((s, a) => s + (covNum(a.arr_raw) || 0), 0),
    complexity: scores.length ? scores.reduce((s, n) => s + n, 0) : null,
    onboarding: list.filter(covIsOnboarding).length,
    retained:   list.filter(a => covRequestStage(a) === "retained").length,
    // Active Request = adopted or retained.
    active:     list.filter(a => ["adopted", "retained"].includes(covRequestStage(a))).length,
    agentic:    list.filter(a => ["amber", "green"].includes(ratingColour(a.agentic_governance_adoption))).length
  };
}

function toggleCoverage(header, csm) {
  header.parentElement.classList.toggle("open");
  if (coverageOpen.has(csm)) coverageOpen.delete(csm); else coverageOpen.add(csm);
}

function renderCoverage() {
  buildFlatPills("coverage-region-filters", "coverage");

  const csmOf = a => adoptBlank(a.csm) ? "Unassigned" : String(a.csm).trim();
  const book  = ACCOUNTS.filter(a => activeRegion === "all" || a.region === activeRegion);
  const q     = searchTerm.toLowerCase();

  const byCsm = {};
  book.forEach(a => { (byCsm[csmOf(a)] = byCsm[csmOf(a)] || []).push(a); });

  // Biggest book first, Unassigned always last.
  const csms = Object.keys(byCsm).sort((x, y) => {
    if (x === "Unassigned") return 1;
    if (y === "Unassigned") return -1;
    return covSummarise(byCsm[y]).arr - covSummarise(byCsm[x]).arr;
  });

  const tot = covSummarise(book);
  const stat = (label, value) =>
    `<div class="stat-item"><span class="stat-label">${label}</span><span class="stat-value">${value}</span></div>`;
  document.getElementById("coverage-stats").innerHTML =
    stat("CSMs", csms.filter(c => c !== "Unassigned").length) +
    stat("Accounts", tot.accounts) +
    stat("ARR", fmtUsd(tot.arr)) +
    stat("S1 Onboarding", tot.onboarding) +
    stat("Request Retained", tot.retained) +
    stat("Active Request", tot.active) +
    stat("Ongoing Agentic Adoption", tot.agentic);

  const chip = (label, value, cls) =>
    `<span class="cov-chip${cls ? " " + cls : ""}"><span class="cov-chip-k">${label}</span>${value}</span>`;

  let shown = 0;
  const html = csms.map(csm => {
    // Search narrows the account rows (or keeps the whole book when the CSM
    // name matches); the header totals always describe the full book.
    const all  = byCsm[csm];
    const rows = (!q || csm.toLowerCase().includes(q)) ? all
               : all.filter(a => a.name.toLowerCase().includes(q));
    if (!rows.length) return "";
    shown++;

    const s = covSummarise(all);
    const open = coverageOpen.has(csm) || (q && rows.length < all.length);
    const sorted = rows.slice().sort((x, y) =>
      (covIsOnboarding(y) - covIsOnboarding(x)) || x.name.localeCompare(y.name));

    const body = sorted.map(a => {
      const onb = covIsOnboarding(a);
      return `<tr class="${onb ? "cov-onboarding" : ""}">
        <td class="cov-quad">${adoptionQuadHtml(a)}</td>
        <td><a class="opp-link" href="#" onclick="event.preventDefault(); goToAccount(${esc(JSON.stringify(a.name))})">${esc(a.name)}</a>
            ${onb ? `<span class="tag cov-onb-tag">Onboarding</span>` : ""}</td>
        <td>${esc(a.lifecycle_stage)}</td>
        <td>${esc(a.region)}</td>
        <td class="cov-num" title="${esc(tenureText(a))}">${covNum(a.months_as_customer) === null ? "\\u2014" : covNum(a.months_as_customer) + " mo"}</td>
        <td class="cov-num">${esc(a.arr)}</td>
      </tr>`;
    }).join("");

    return `<div class="card cov-card${open ? " open" : ""}">
      <div class="card-header" onclick="toggleCoverage(this, ${esc(JSON.stringify(csm))})">
        <div class="card-name">${esc(csm)}</div>
        <div class="cov-chips">
          ${chip("Accounts", s.accounts)}
          ${chip("ARR", fmtUsd(s.arr))}
          ${chip("PBAC Complexity", s.complexity === null ? "\\u2014" : s.complexity)}
          ${chip("S1 Onboarding", s.onboarding, s.onboarding ? "cov-chip-onb" : "")}
          ${chip("Active Request", s.active)}
          ${chip("Ongoing Agentic", s.agentic)}
        </div>
        <span class="expand-icon">&#9660;</span>
      </div>
      <div class="card-body cov-body">
        <table class="flat-table cov-table">
          <thead><tr><th>Adoption</th><th>Account</th><th>Lifecycle Stage</th><th>Region</th><th class="cov-num">Customer for</th><th class="cov-num">ARR</th></tr></thead>
          <tbody>${body}</tbody>
        </table>
      </div>
    </div>`;
  }).join("");

  // Agentic profile by CSM first (moved here from Adoption > Agentic Engagement Overview).
  document.getElementById("coverage-list").innerHTML = covAgenticHtml() +
    (html || `<div class="cov-empty">No accounts match.</div>`);
  setCount("sub-count-coverage", shown);
}

// ── Page and sub-tab switching ─────────────────────────────────────────────
// Each page remembers which sub-tab it was on, so coming back to Engagement
// lands you where you left rather than resetting to Accounts.
const PAGES = {
  engagement:  { subs: ["accounts", "renewals", "expansions", "new_logos", "changes", "coverage"], active: "accounts" },
  adoption:    { subs: ["overview", "derivation", "agentic", "critical_churn"], active: "overview" },
  performance: { subs: ["self_managed", "saas_migration", "escalations"],  active: "self_managed" },
  documentation: { subs: ["doctrine", "reporting", "guide"], active: "doctrine" }
};

function switchPage(name) {
  activePage = name;
  document.querySelectorAll(".page").forEach(p => p.classList.toggle("active", p.id === "page-" + name));
  document.querySelectorAll(".page-tab").forEach(b => b.classList.toggle("active", b.dataset.page === name));
  syncSubTabs();
  render();
}

function switchSub(sub) {
  PAGES[activePage].active = sub;
  // Each adoption table sorts on its own terms; carrying a column over from the
  // previous one sorts on a field that table may not even have.
  if (activePage === "adoption") adoptSort = { col: null, dir: 1 };
  syncSubTabs();
  render();
}

// Scoped to the active page: sub-tab ids are unique across pages, but querying
// globally would still fight with the other pages\' buttons on every switch.
function syncSubTabs() {
  const sub  = PAGES[activePage].active;
  const page = document.getElementById("page-" + activePage);
  if (!page) return;
  page.querySelectorAll(".sub-btn").forEach(b => b.classList.toggle("active", b.dataset.sub === sub));
  page.querySelectorAll(".subpanel").forEach(p => p.classList.toggle("active", p.id === "sub-panel-" + sub));
  document.getElementById("search").placeholder =
    sub === "accounts" ? "Search accounts..." : "Filter by account name...";
}

// ── Render dispatcher ──────────────────────────────────────────────────────
function render() {
  const sub = PAGES[activePage].active;
  if (activePage === "engagement") {
    if      (sub === "accounts")   renderAccounts();
    else if (sub === "coverage")   renderCoverage();
    else if (sub === "renewals")   renderRenewals();
    else if (sub === "expansions") renderExpansions();
    else if (sub === "new_logos")  renderNewLogos();
    else if (sub === "changes")    renderChanges();
  } else if (activePage === "adoption") {
    renderAdoption();
  } else if (activePage === "performance") {
    if      (sub === "saas_migration") renderSaaSMigration();
    else if (sub === "escalations")    renderEscalations();
    else                               renderSelfManaged();
  } else if (activePage === "documentation") {
    renderDocumentation();
  }
}

// Writes a count badge, tolerating an id that isn\'t on the page.
function setCount(id, n) {
  const el = document.getElementById(id);
  if (el) el.textContent = n;
}

function sortAdopt(col) {
  adoptSort.dir = adoptSort.col === col ? -adoptSort.dir : 1;
  adoptSort.col = col;
  renderAdoption();
}

function pctClass(ratio) {
  if (ratio === null || ratio === undefined || isNaN(ratio)) return "pct-none";
  if (ratio >= 0.5)  return "pct-high";
  if (ratio >= 0.25) return "pct-med";
  return "pct-low";
}

// Leading colour word of a rating, or "" — the colour is what the stoplight,
// the band shading and the rating sort all key off.
function ratingColour(v) {
  if (adoptBlank(v)) return "";
  const m = String(v).trim().toLowerCase().match(/^(green|amber|yellow|red|black)/);
  if (!m) return "";
  return m[1] === "yellow" ? "amber" : m[1];
}
const COLOUR_RANK = { green: 4, amber: 3, red: 2, black: 1, "": 0 };

// Region / CSM facet, shared by every Adoption sub-tab. Reads the same
// activeRegion / activeCSM the Accounts and Renewals pages set, so the pills
// mean the same thing everywhere and a selection survives switching tabs.
function adoptFacet(a) {
  return (activeRegion === "all" || a.region === activeRegion) &&
         (activeCSM    === "all" || a.csm    === activeCSM);
}

// True when an account has a PBAC rating — the account card mini plot.
function hasPbac(a) {
  return a.pbac_score !== null && a.pbac_score !== undefined &&
         !isNaN(a.pbac_score) && !adoptBlank(a.pbac_rating);
}

// True when an account has a product adoption score to plot at all. It needs
// the PBAC complexity score underneath it, so a looker = FALSE run has none.
function hasProduct(a) {
  return covNum(a.product_adoption) !== null && !adoptBlank(a.product_rating);
}

function renderAdoption() {
  buildFlatPills("adopt-region-filters", "adoption");
  buildFlatCSMPills("adopt-csm-filters");

  const faceted = ACCOUNTS.filter(adoptFacet);
  setCount("tab-count-adoption", faceted.length);
  setCount("sub-count-overview", faceted.filter(hasProduct).length);
  setCount("sub-count-critical_churn", faceted.filter(ccrInCohort).length);

  if (PAGES.adoption.active === "critical_churn") {
    renderCriticalChurn();
    return;
  }
  if (PAGES.adoption.active === "agentic") {
    renderAgentic();
    return;
  }
  if (PAGES.adoption.active === "derivation") {
    const wu = document.getElementById("pbac-writeup");
    if (!wu.innerHTML) wu.innerHTML = productWriteupHtml();
    return;
  }
  renderPbacPanel(ACCOUNTS.filter(hasProduct));
  renderProductTable();
}

// ── Critical Churn Risk ────────────────────────────────────────────────────
// Accounts rated Black on product adoption (low score, low consumption) that
// have been customers for more than CCR_MIN_MONTHS. The plot is the Adoption
// scatter cropped to the Black cell: x runs 0 to PA_CFG.ratioMinimal, y runs 0
// to PA_CFG.adoptionMin, so the axes are the same scale family as the overview
// and move with it if a threshold changes.
const CCR_MIN_MONTHS = 6;
const CCR_W = 1000, CCR_H = 500;
const CCR_M = { top: 18, right: 22, bottom: 52, left: 58 };
const CCR_PW = CCR_W - CCR_M.left - CCR_M.right;
const CCR_PH = CCR_H - CCR_M.top - CCR_M.bottom;
const ccrX = r => CCR_M.left + Math.max(0, Math.min(r, PA_CFG.ratioMinimal)) / PA_CFG.ratioMinimal * CCR_PW;
const ccrY = s => CCR_M.top + CCR_PH - Math.max(0, Math.min(s, PA_CFG.adoptionMin)) / PA_CFG.adoptionMin * CCR_PH;

function ccrInCohort(a) {
  const m = covNum(a.months_as_customer);
  return hasProduct(a) && ratingColour(a.product_rating) === "black" && m !== null && m > CCR_MIN_MONTHS;
}

// "S1" / "S2" / "other". Matched on the stage number or its words, like
// isInstallStage(), so a picklist relabel does not empty the tables.
function ccrStage(a) {
  if (isInstallStage(a)) return "S1";
  const st = String((a && a.lifecycle_stage) || "").trim().toLowerCase();
  return (st.startsWith("s2") || st.includes("mvp")) ? "S2" : "other";
}

// Earliest open renewal end date on or after today, display form, or "".
function ccrNextRenewal(a) {
  const today = new Date().toISOString().slice(0, 10);
  const up = (a.renewals || []).filter(r => r.end_date_raw && r.end_date_raw >= today)
                               .sort((x, y) => x.end_date_raw.localeCompare(y.end_date_raw));
  return up.length ? up[0].end_date : "";
}

function ccrUsd(v) {
  if (v === null || !isFinite(v)) return "\\u2014";
  if (v >= 1e6) return "$" + (v / 1e6).toFixed(1) + "M";
  if (v >= 1e3) return "$" + Math.round(v / 1e3) + "K";
  return "$" + Math.round(v);
}
const ccrArr = list => list.reduce((s, a) => s + (covNum(a.arr_raw) || 0), 0);

// Which graphic the brief card shows: "bars" (renewal ARR by churn quarter,
// the default) or "plot" (Black cell). Kept across re-renders and facets.
let ccrView = "bars";
function setCcrView(v) {
  ccrView = v;
  renderCriticalChurn();
}

// "As of <date> · <facet>" line for the brief header, so a screenshot carries
// its own context.
function ccrContext() {
  const upd = (document.getElementById("topbar-updated") || {}).textContent || "";
  const bits = [upd.trim() || ("As of " + new Date().toLocaleDateString(undefined, { month: "long", day: "numeric", year: "numeric" }))];
  bits.push(activeRegion === "all" ? "All regions" : activeRegion);
  if (activeCSM !== "all") bits.push("CSM: " + activeCSM);
  if (searchTerm) bits.push("Search: \\u201C" + searchTerm + "\\u201D");
  if (ccrQuarters.size) bits.push("Churn quarter: " + ccrSortedQuarters([...ccrQuarters]).join(", "));
  return bits.map(esc).join(" &middot; ");
}

// ── Churn quarter filter (multi-select) ────────────────────────────────────
// ccrQuarters empty = no filter: the bars show the next four fiscal quarters
// and the Black plot shows every cohort account. Picking quarters narrows both
// views (and their tables) to renewals / accounts in those quarters.
let ccrQuarters = new Set();
const ccrSortedQuarters = list => list.slice().sort((x, y) => saasQuarterKey(x) - saasQuarterKey(y));

function toggleCcrQuarter(q) {
  if (q === null) ccrQuarters.clear();
  else if (ccrQuarters.has(q)) ccrQuarters.delete(q);
  else ccrQuarters.add(q);
  renderCriticalChurn();
}

// Every churn quarter on a Black / Red account renewal, plus the default
// four-quarter window so those are always pickable.
function ccrQuarterOptions() {
  const qs = new Set(ccrWindow());
  ACCOUNTS.filter(a => hasProduct(a) && ccrSeg(a)).forEach(a =>
    (a.renewals || []).forEach(r => {
      if (!adoptBlank(r.churn_quarter)) qs.add(String(r.churn_quarter).trim());
    }));
  ccrQuarters.forEach(q => qs.add(q));
  return ccrSortedQuarters([...qs].filter(q => isFinite(saasQuarterKey(q))));
}

function buildCcrQuarterPills() {
  const el = document.getElementById("ccr-quarter-filters");
  if (!el) return;
  const allLabel = ccrView === "bars" ? "Next 4 Quarters" : "All Churn Quarters";
  el.innerHTML = [
    `<button class="flat-pill${ccrQuarters.size ? "" : " active"}" onclick="toggleCcrQuarter(null)">${allLabel}</button>`
  ].concat(ccrQuarterOptions().map(q =>
    `<button class="flat-pill${ccrQuarters.has(q) ? " active" : ""}" title="${esc(saasQuarterMonths(q))}" ` +
    `onclick="toggleCcrQuarter(${esc(JSON.stringify(q))})">${esc(q)}</button>`
  )).join("");
}

// Plot view: with quarters picked, keep accounts with a renewal in one of them.
function ccrInQuarters(a) {
  if (!ccrQuarters.size) return true;
  return (a.renewals || []).some(r => !adoptBlank(r.churn_quarter) && ccrQuarters.has(String(r.churn_quarter).trim()));
}

const ccrKpi = (l, v, s) => `<div class="ccr-kpi"><div class="ccr-kpi-l">${l}</div>` +
  `<div class="ccr-kpi-v">${v}</div><div class="ccr-kpi-s">${s || "&nbsp;"}</div></div>`;

function renderCriticalChurn() {
  document.querySelectorAll(".ccr-tbtn").forEach(b => b.classList.toggle("active", b.dataset.view === ccrView));
  document.getElementById("ccr-view-plot").style.display = ccrView === "plot" ? "" : "none";
  document.getElementById("ccr-view-bars").style.display = ccrView === "bars" ? "" : "none";
  buildCcrQuarterPills();
  if (ccrView === "bars") {
    document.getElementById("ccr-tables").innerHTML = "";
    renderCcrBars();
    return;
  }

  const rows = ACCOUNTS.filter(a => ccrInCohort(a) && paInView(a) && ccrInQuarters(a))
                       .sort((x, y) => (covNum(y.arr_raw) || 0) - (covNum(x.arr_raw) || 0) || x.name.localeCompare(y.name));
  const s1 = rows.filter(a => ccrStage(a) === "S1"), s2 = rows.filter(a => ccrStage(a) === "S2");
  const other = rows.filter(a => ccrStage(a) === "other");
  const blackInView = ACCOUNTS.filter(a => paInView(a) && ccrInQuarters(a) && hasProduct(a) && ratingColour(a.product_rating) === "black");
  const noTenure = blackInView.filter(a => covNum(a.months_as_customer) === null).length;

  const mos = rows.map(a => covNum(a.months_as_customer)).sort((x, y) => x - y);
  const med = !mos.length ? null : mos.length % 2 ? mos[(mos.length - 1) / 2]
            : Math.round((mos[mos.length / 2 - 1] + mos[mos.length / 2]) / 2);

  document.getElementById("ccr-title").textContent =
    rows.length + (rows.length === 1 ? " customer" : " customers") + " past " + CCR_MIN_MONTHS +
    " months still rated Black: Critical Churn Risk";
  document.getElementById("ccr-sub").innerHTML = ccrContext() +
    " &middot; low product adoption and consumption under " + Math.round(PA_CFG.ratioMinimal * 100) + "%";
  document.getElementById("ccr-stats").innerHTML =
    ccrKpi("Accounts", rows.length, "of " + blackInView.length + " rated Black") +
    ccrKpi("ARR at Risk", ccrUsd(ccrArr(rows)), "contract ARR") +
    ccrKpi("S1 &middot; Install", s1.length, ccrUsd(ccrArr(s1)) + " ARR") +
    ccrKpi("S2 &middot; MVP", s2.length, ccrUsd(ccrArr(s2)) + " ARR") +
    ccrKpi("Median Tenure", med === null ? "\\u2014" : med + " mo", "months as a customer");

  ccrPlot(rows);

  const dash = `<span class="em-dash">\\u2014</span>`;
  const table = (key, title, list) => {
    const head = `<div class="ccr-thead"><span class="ccr-badge">${key}</span><span class="ccr-ttitle">${title}</span>` +
      `<span class="ccr-tmeta"><b>${list.length}</b> ${list.length === 1 ? "account" : "accounts"} &middot; <b>${ccrUsd(ccrArr(list))}</b> ARR</span></div>`;
    if (!list.length) return `<div>${head}<p class="ccr-empty">No ${key} accounts in view.</p></div>`;
    const body = list.map(a => {
      const ratio = covNum(a.consumption_ratio_raw);
      const ren = ccrNextRenewal(a);
      return `<tr onclick="goToAccount(${esc(JSON.stringify(a.name))})">
        <td><span class="acct-link">${esc(a.name)}</span></td>
        <td class="num"><span class="ccr-mo">${covNum(a.months_as_customer)} mo</span></td>
        <td>${adoptBlank(a.customer_since) ? dash : esc(a.customer_since)}</td>
        <td class="num">${adoptBlank(a.arr) ? dash : esc(a.arr)}</td>
        <td class="num">${ratio === null ? dash : Math.round(ratio * 1000) / 10 + "%"}</td>
        <td class="num">${a.product_adoption}<span class="pa-of"> / ${PA_CFG.scoreMax}</span></td>
        <td>${ren ? esc(ren) : dash}</td>
        <td>${adoptBlank(a.csm) ? dash : esc(a.csm)}</td>
        <td>${adoptBlank(a.region) ? dash : esc(a.region)}</td>
      </tr>`;
    }).join("");
    return `<div>${head}<table class="flat-table ccr-table"><thead><tr>
      <th>Account</th><th class="num">Tenure</th><th>Customer Since</th><th class="num">ARR</th>
      <th class="num">Consumption</th><th class="num">Product Adoption</th><th>Next Renewal</th><th>CSM</th><th>Region</th>
      </tr></thead><tbody>${body}</tbody></table></div>`;
  };

  const notes = [];
  if (other.length) notes.push(`Plotted but not tabled (other lifecycle stage): ` +
    other.map(a => esc(a.name) + " (" + (adoptBlank(a.lifecycle_stage) ? "no stage" : esc(a.lifecycle_stage)) + ")").join(", ") + ".");
  if (noTenure) notes.push(`${noTenure} Black ${noTenure === 1 ? "account has" : "accounts have"} no contract start date in the tenure report and ${noTenure === 1 ? "is" : "are"} not shown.`);
  notes.push(`Tenure is whole months since the earliest contract start date. Shown: product adoption rated Black and more than ${CCR_MIN_MONTHS} months as a customer.`);

  document.getElementById("ccr-tables").innerHTML =
    table("S1", "Installation and Config.", s1) +
    table("S2", "MVP in prod, use case pipelining", s2) +
    `<div class="ccr-foot">${notes.join("<br>")}</div>`;
}

function ccrPlot(rows) {
  const svg = document.getElementById("ccr-plot");
  svg.textContent = "";
  const xMax = PA_CFG.ratioMinimal, yMax = PA_CFG.adoptionMin;

  // The Black cell is the whole plot.
  svg.appendChild(svgEl("rect", { x: CCR_M.left, y: CCR_M.top, width: CCR_PW, height: CCR_PH,
                                  fill: PBAC_COLOURS.black.bg, stroke: "#B8B8B8", "stroke-width": 1 }));
  const xTicks = [0, 0.05, 0.10, 0.15, 0.20, 0.25].filter(v => v <= xMax + 1e-9);
  xTicks.forEach(v => {
    if (v > 0 && v < xMax) svg.appendChild(svgEl("line", { x1: ccrX(v), y1: CCR_M.top, x2: ccrX(v), y2: CCR_M.top + CCR_PH,
                                                          stroke: "#FFFFFF", "stroke-width": 1 }));
    svg.appendChild(svgEl("text", { x: ccrX(v), y: CCR_M.top + CCR_PH + 18, class: "ccr-axis", "text-anchor": "middle" }, v.toFixed(2)));
  });
  for (let s = 0; s <= yMax; s++) {
    if (s > 0 && s < yMax) svg.appendChild(svgEl("line", { x1: CCR_M.left, y1: ccrY(s), x2: CCR_M.left + CCR_PW, y2: ccrY(s),
                                                          stroke: "#FFFFFF", "stroke-width": 1 }));
    svg.appendChild(svgEl("text", { x: CCR_M.left - 10, y: ccrY(s) + 4, class: "ccr-axis", "text-anchor": "end" }, s));
  }
  svg.appendChild(svgEl("text", { x: CCR_M.left + CCR_PW / 2, y: CCR_H - 12, class: "ccr-axis-title", "text-anchor": "middle" },
                        "CONSUMPTION RATIO (REGISTERED \\u00F7 LICENSED USERS)"));
  const yT = svgEl("text", { x: 16, y: CCR_M.top + CCR_PH / 2, class: "ccr-axis-title", "text-anchor": "middle" },
                   "PRODUCT ADOPTION SCORE");
  yT.setAttribute("transform", "rotate(-90 16 " + (CCR_M.top + CCR_PH / 2) + ")");
  svg.appendChild(yT);

  if (!rows.length) {
    svg.appendChild(svgEl("text", { x: CCR_M.left + CCR_PW / 2, y: CCR_M.top + CCR_PH / 2, class: "ccr-axis-title",
                                    "text-anchor": "middle" }, "NO ACCOUNTS IN VIEW"));
    return;
  }

  // Points. Radius on sqrt(ARR) like the overview, scaled up since the plot
  // holds a handful of accounts rather than the whole book.
  const R_MIN = 5, R_MAX = 12;
  const roots = rows.map(a => Math.sqrt(Math.max(0, covNum(a.arr_raw) || 0)));
  const rLo = Math.min.apply(null, roots), rHi = Math.max.apply(null, roots);
  const step = CCR_PH / yMax;
  const nodes = rows.map((a, i) => {
    const ratio = covNum(a.consumption_ratio_raw);
    const known = ratio !== null;
    const r  = rHi > rLo ? R_MIN + (roots[i] - rLo) / (rHi - rLo) * (R_MAX - R_MIN) : (R_MIN + R_MAX) / 2;
    // Unknown ratios sit at zero by convention; spread them along the left
    // edge so they do not stack (their x is a placeholder anyway).
    const jx = known ? (pbacSeed(a.name) - 0.5) * 10 : pbacSeed(a.name) * 34;
    const jy = (pbacSeed(a.name + "y") - 0.5) * step * 0.5;
    return { a, r, known,
      cx: Math.max(CCR_M.left + r + 2, Math.min(CCR_M.left + CCR_PW - r - 2, ccrX(known ? ratio : 0) + jx)),
      cy: Math.max(CCR_M.top + r + 2, Math.min(CCR_M.top + CCR_PH - r - 2, ccrY(covNum(a.product_adoption)) + jy)) };
  });

  const gLead = svgEl("g", {}), gPts = svgEl("g", {}), gLbl = svgEl("g", {});
  svg.appendChild(gLead); svg.appendChild(gPts); svg.appendChild(gLbl);

  nodes.forEach(n => {
    const g = svgEl("g", { class: "ccr-pt" });
    const base = { fill: "#111111", stroke: "#FFFFFF", "stroke-width": 1.8, class: "ccr-mark" };
    const side = n.r * 1.772;   // square with the same area as the dot
    const m = isInstallStage(n.a)
      ? svgEl("rect", Object.assign({ x: n.cx - side / 2, y: n.cy - side / 2, width: side, height: side, rx: 1.5 }, base))
      : svgEl("circle", Object.assign({ cx: n.cx, cy: n.cy, r: n.r }, base));
    if (!n.known) { m.setAttribute("fill-opacity", ".4"); m.setAttribute("stroke", "#111111"); m.setAttribute("stroke-dasharray", "2 2"); }
    const ratio = covNum(n.a.consumption_ratio_raw);
    g.appendChild(svgEl("title", {}, n.a.name + " \\u2014 " + covNum(n.a.months_as_customer) + " months as a customer" +
      (adoptBlank(n.a.customer_since) ? "" : " (since " + n.a.customer_since + ")") +
      ", ratio " + (ratio === null ? "n/a" : Math.round(ratio * 1000) / 10 + "%") +
      ", score " + n.a.product_adoption + " / " + PA_CFG.scoreMax + ", " + n.a.arr + " ARR"));
    g.appendChild(m);
    g.addEventListener("click", () => goToAccount(n.a.name));
    gPts.appendChild(g);
  });

  // Labels: name + months pill at the bottom right of each point. Greedy, top
  // to bottom; a label that would collide takes the cheapest free slot nearby
  // (right/down first, then up, then the left side) with a leader line back.
  const boxes = nodes.map(n => ({ x: n.cx - n.r - 2, y: n.cy - n.r - 2, w: 2 * n.r + 4, h: 2 * n.r + 4 }));
  const hits = (p, q) => p.x < q.x + q.w && q.x < p.x + p.w && p.y < q.y + q.h && q.y < p.y + p.h;
  const LH = 18;
  nodes.map((n, i) => i).sort((i, j) => (nodes[i].cy - nodes[j].cy) || (nodes[i].cx - nodes[j].cx)).forEach(i => {
    const n = nodes[i];
    const g = svgEl("g", { class: "ccr-lbl" });
    const name = svgEl("text", { x: 0, y: 13, class: "ccr-lbl-name" }, n.a.name);
    g.appendChild(name); gLbl.appendChild(g);
    const nw = name.getComputedTextLength();
    const mo = svgEl("text", { x: 0, y: 12.5, class: "ccr-lbl-mo", "text-anchor": "middle" }, covNum(n.a.months_as_customer) + " mo");
    g.appendChild(mo);
    const mw = mo.getComputedTextLength() + 14;
    g.insertBefore(svgEl("rect", { x: nw + 6, y: 2, width: mw, height: 15, rx: 7.5, fill: "#0D0D0D" }), mo);
    mo.setAttribute("x", nw + 6 + mw / 2);
    const w = nw + 6 + mw + 2, h = LH;

    const ax = n.cx + n.r * 0.7 + 3, ay = Math.min(n.cy + n.r * 0.7 - 2, CCR_M.top + CCR_PH - h - 3);
    let best = null;
    for (const dir of [1, -1]) {
      for (let dy = -10 * LH; dy <= 6 * LH; dy += LH / 2) {
        const y = ay + dy;
        if (y < CCR_M.top + 3 || y + h > CCR_M.top + CCR_PH - 3) continue;
        for (let dx = 0; dx < 600; dx += 6) {
          const x = dir > 0 ? ax + dx : n.cx - n.r * 0.7 - 3 - w - dx;
          if (x < CCR_M.left + 3 || x + w > CCR_M.left + CCR_PW - 3) break;
          const cost = dx + (dy < 0 ? 3.2 : 2) * Math.abs(dy) + (dir > 0 ? 0 : 60);
          if (best && cost >= best.cost) break;
          const box = { x: x, y: y, w: w, h: h };
          if (!boxes.some(b => hits(box, b))) { best = { box, cost, moved: dx > 8 || dy !== 0 || dir < 0 }; break; }
        }
      }
    }
    if (!best) best = { box: { x: ax, y: ay, w: w, h: h }, cost: 0, moved: false };
    boxes.push(best.box);
    g.setAttribute("transform", "translate(" + best.box.x + "," + best.box.y + ")");
    g.addEventListener("click", () => goToAccount(n.a.name));
    if (best.moved) {
      const ex = best.box.x > n.cx ? best.box.x - 2 : best.box.x + w + 2;
      gLead.appendChild(svgEl("path", { class: "ccr-leader", d: "M" + n.cx + "," + n.cy + " L" + ex + "," + (best.box.y + h / 2) }));
    }
  });
}

// ── Critical churn risk: renewal ARR by churn quarter ──────────────────────
// Every Black and Red account in view (no tenure filter: this is the whole
// exposure), each open renewal counted in its Churn Quarter at its renewal
// Amount (falling back to contract ARR when the opportunity has no amount).
// Stacked by product adoption rating so the adoption story is the colour.
const CCR_SEGS = [
  { key: "black",           label: "Black: Critical Churn Risk",        fill: "#111111", ink: "#FFFFFF" },
  { key: "red_adoption",    label: "Red: High Risk \\u2013 Adoption",    fill: "#DC2626", ink: "#FFFFFF" },
  { key: "red_consumption", label: "Red: High Risk \\u2013 Consumption", fill: "#FCA5A5", ink: "#7F1D1D" }
];
const CCR_NO_Q = "No churn quarter";

function ccrSeg(a) {
  const c = ratingColour(a.product_rating);
  if (c === "black") return "black";
  if (c !== "red") return null;
  return String(a.product_rating).toLowerCase().includes("consumption") ? "red_consumption" : "red_adoption";
}

function ccrNiceMax(v) {
  if (v <= 0) return 1;
  const p = Math.pow(10, Math.floor(Math.log10(v)));
  const m = v / p;
  return (m <= 1 ? 1 : m <= 2 ? 2 : m <= 2.5 ? 2.5 : m <= 5 ? 5 : 10) * p;
}

// Fiscal quarter n quarters after q ("Q3-2027", 1 -> "Q4-2027").
function ccrQuarterAdd(q, n) {
  const k = saasQuarterKey(q);
  if (!isFinite(k)) return q;
  const idx = Math.floor(k / 10) * 4 + (k % 10 - 1) + n;
  return "Q" + (idx % 4 + 1) + "-" + Math.floor(idx / 4);
}

// The chart window: the current fiscal quarter and the three after it.
function ccrWindow() {
  const cur = currentFiscalQuarter();
  return [0, 1, 2, 3].map(n => ccrQuarterAdd(cur, n));
}

// Table under the bars: one row per renewal in the window. Sortable; quarter
// and rating sort on their natural order, not their text.
let ccrBarSort = { col: "quarter", dir: 1 };
const CCR_SEG_RANK = { black: 1, red_adoption: 2, red_consumption: 3 };
function sortCcrBars(col) {
  ccrBarSort = ccrBarSort.col === col ? { col: col, dir: -ccrBarSort.dir }
             : { col: col, dir: ["arr", "score", "ratio"].includes(col) ? -1 : 1 };
  renderCcrBars();
}

function renderCcrBars() {
  const picked = ccrQuarters.size > 0;
  const win = picked ? ccrSortedQuarters([...ccrQuarters]) : ccrWindow();
  const accts = ACCOUNTS.filter(a => paInView(a) && hasProduct(a) && ccrSeg(a));
  const byQ = {};
  win.forEach(q => { byQ[q] = { q: q, total: 0, accts: new Set(), black: 0, red_adoption: 0, red_consumption: 0 }; });
  const tRows = [];
  accts.forEach(a => {
    const seg = ccrSeg(a);
    (a.renewals || []).forEach(r => {
      const q = adoptBlank(r.churn_quarter) ? CCR_NO_Q : String(r.churn_quarter).trim();
      if (!byQ[q]) return;   // outside the selected / default quarters
      const amt = covNum(r.amount_raw) !== null ? covNum(r.amount_raw) : (covNum(a.arr_raw) || 0);
      byQ[q][seg] += amt;
      byQ[q].total += amt;
      byQ[q].accts.add(a.name);
      tRows.push({ a: a, r: r, q: q, seg: seg, amt: amt });
    });
  });
  const qs = win.map(q => byQ[q]);
  const sum = k => qs.reduce((s, d) => s + d[k], 0);
  const total = sum("total");
  const nAccts = new Set(tRows.map(t => t.a.name)).size;

  document.getElementById("ccr-title").textContent =
    ccrUsd(total) + " of renewal ARR " +
    (picked ? "in " + (win.length === 1 ? win[0] : win.length + " selected quarters") : "in the next four quarters") +
    " sits with Black and Red adoption accounts";
  document.getElementById("ccr-sub").innerHTML = ccrContext() +
    (picked ? "" : " &middot; " + win[0] + " to " + win[win.length - 1]) +
    " &middot; open renewals stacked by product adoption rating";
  const top = qs.reduce((b, d) => (!b || d.total > b.total ? d : b), null);
  document.getElementById("ccr-stats").innerHTML =
    ccrKpi("Renewal ARR at Risk", ccrUsd(total), nAccts + " Black / Red accounts") +
    ccrKpi("Black", ccrUsd(sum("black")), "critical churn risk") +
    ccrKpi("Red \\u2013 Adoption", ccrUsd(sum("red_adoption")), "low adoption score") +
    ccrKpi("Red \\u2013 Consumption", ccrUsd(sum("red_consumption")), "low seat consumption") +
    ccrKpi("Peak Quarter", top ? esc(top.q) : "\\u2014", top ? ccrUsd(top.total) + " up for renewal" : "");

  document.getElementById("ccr-bars-legend").innerHTML =
    CCR_SEGS.map(s => `<span class="ccr-key"><svg viewBox="0 0 16 14"><rect x="2" y="2" width="12" height="10" rx="2" fill="${s.fill}"/></svg> ${s.label}</span>`).join("") +
    `<span class="ccr-key">ARR = renewal opportunity amount (contract ARR where blank)</span>`;

  ccrBarTable(tRows);

  const svg = document.getElementById("ccr-bars");
  svg.textContent = "";
  const M = { top: 34, right: 16, bottom: 56, left: 70 };
  const PW = CCR_W - M.left - M.right, PH = CCR_H - M.top - M.bottom;
  if (total <= 0) {
    svg.appendChild(svgEl("text", { x: CCR_W / 2, y: CCR_H / 2, class: "ccr-axis-title", "text-anchor": "middle" },
                          "NO OPEN RENEWALS FOR BLACK OR RED ACCOUNTS IN VIEW"));
    return;
  }
  const yMax = ccrNiceMax(Math.max.apply(null, qs.map(d => d.total)) * 1.08);
  const Yb = v => M.top + PH - v / yMax * PH;
  for (let i = 0; i <= 5; i++) {
    const v = yMax * i / 5;
    svg.appendChild(svgEl("line", { x1: M.left, y1: Yb(v), x2: M.left + PW, y2: Yb(v),
                                    stroke: i === 0 ? "#B8B8B8" : "#ECECEC", "stroke-width": 1 }));
    svg.appendChild(svgEl("text", { x: M.left - 10, y: Yb(v) + 4, class: "ccr-axis", "text-anchor": "end" }, ccrUsd(v)));
  }
  const yT = svgEl("text", { x: 16, y: M.top + PH / 2, class: "ccr-axis-title", "text-anchor": "middle" }, "RENEWAL ARR");
  yT.setAttribute("transform", "rotate(-90 16 " + (M.top + PH / 2) + ")");
  svg.appendChild(yT);

  const band = PW / qs.length, bw = Math.min(84, band * 0.62);
  qs.forEach((d, i) => {
    const x = M.left + band * i + (band - bw) / 2;
    let base = 0;
    CCR_SEGS.forEach(s => {
      const v = d[s.key];
      if (v <= 0) return;
      const y1 = Yb(base + v), y0 = Yb(base);
      const rect = svgEl("rect", { x: x, y: y1, width: bw, height: Math.max(0, y0 - y1), fill: s.fill });
      rect.appendChild(svgEl("title", {}, d.q + " \\u2014 " + s.label.replace("\\u2013", "-") + ": " + ccrUsd(v)));
      svg.appendChild(rect);
      if (y0 - y1 >= 17) svg.appendChild(svgEl("text", { x: x + bw / 2, y: (y0 + y1) / 2 + 4, class: "ccr-seg-val",
                                                          fill: s.ink, "text-anchor": "middle" }, ccrUsd(v)));
      base += v;
    });
    svg.appendChild(svgEl("text", { x: x + bw / 2, y: Yb(d.total) - 8, class: "ccr-bar-val", "text-anchor": "middle" }, ccrUsd(d.total)));
    svg.appendChild(svgEl("text", { x: x + bw / 2, y: M.top + PH + 18, class: "ccr-axis", "text-anchor": "middle",
                                    "font-weight": "700", fill: "#111111" }, d.q));
    svg.appendChild(svgEl("text", { x: x + bw / 2, y: M.top + PH + 33, class: "ccr-bar-n", "text-anchor": "middle" },
                          d.accts.size + (d.accts.size === 1 ? " account" : " accounts")));
  });
}

// Every renewal behind the bars, one row each, below the brief card.
function ccrBarTable(tRows) {
  const el = document.getElementById("ccr-tables");
  const dash = `<span class="em-dash">\\u2014</span>`;
  const segOf = k => CCR_SEGS.find(s => s.key === k);
  const cols = [
    { key: "name",    label: "Account",          val: t => t.a.name.toLowerCase() },
    { key: "quarter", label: "Churn Quarter",    val: t => saasQuarterKey(t.q) },
    { key: "rating",  label: "Adoption Rating",  val: t => CCR_SEG_RANK[t.seg] },
    { key: "arr",     label: "Renewal ARR",      val: t => t.amt, num: true },
    { key: "end",     label: "Contract End",     val: t => t.r.end_date_raw || "" },
    { key: "ratio",   label: "Consumption",      val: t => covNum(t.a.consumption_ratio_raw), num: true },
    { key: "score",   label: "Product Adoption", val: t => covNum(t.a.product_adoption), num: true },
    { key: "csm",     label: "CSM",              val: t => String(t.a.csm || "").toLowerCase() },
    { key: "region",  label: "Region",           val: t => String(t.a.region || "").toLowerCase() }
  ];
  const col = cols.find(c => c.key === ccrBarSort.col) || cols[1];
  // Ties fall back to quarter, then rating, then ARR (largest first), so a
  // sort on either key keeps the other in a sensible order.
  const tie = (x, y) => (saasQuarterKey(x.q) - saasQuarterKey(y.q)) ||
                        (CCR_SEG_RANK[x.seg] - CCR_SEG_RANK[y.seg]) || (y.amt - x.amt) ||
                        x.a.name.localeCompare(y.a.name);
  const rows = tRows.slice().sort((x, y) => {
    const xv = col.val(x), yv = col.val(y);
    const xb = xv === null || xv === "", yb = yv === null || yv === "";
    if (xb !== yb) return xb ? 1 : -1;
    const d = typeof xv === "number" && typeof yv === "number" ? xv - yv : String(xv).localeCompare(String(yv));
    return d * ccrBarSort.dir || tie(x, y);
  });

  const total = rows.reduce((s, t) => s + t.amt, 0);
  const head = `<div class="ccr-thead"><span class="ccr-ttitle">Accounts in the chart</span>` +
    `<span class="ccr-tmeta"><b>${new Set(rows.map(t => t.a.name)).size}</b> accounts &middot; ` +
    `<b>${rows.length}</b> ${rows.length === 1 ? "renewal" : "renewals"} &middot; <b>${ccrUsd(total)}</b> ARR</span></div>`;
  if (!rows.length) {
    el.innerHTML = `<div>${head}<p class="ccr-empty">No open renewals for Black or Red accounts in these quarters.</p></div>`;
    return;
  }
  const hdr = cols.map(c => {
    const arrow = ccrBarSort.col === c.key ? (ccrBarSort.dir === 1 ? " \\u2191" : " \\u2193") : "";
    return `<th class="${c.num ? "num" : ""}" onclick="sortCcrBars(${esc(JSON.stringify(c.key))})">${c.label}${arrow}</th>`;
  }).join("");
  const body = rows.map(t => {
    const s = segOf(t.seg), ratio = covNum(t.a.consumption_ratio_raw);
    return `<tr onclick="goToAccount(${esc(JSON.stringify(t.a.name))})">
      <td><span class="acct-link">${esc(t.a.name)}</span></td>
      <td><b>${esc(t.q)}</b></td>
      <td><span class="ccr-rating"><span class="ccr-swatch" style="background:${s.fill}"></span>${s.label}</span></td>
      <td class="num">${ccrUsd(t.amt)}</td>
      <td>${adoptBlank(t.r.end_date) ? dash : esc(t.r.end_date)}</td>
      <td class="num">${ratio === null ? dash : Math.round(ratio * 1000) / 10 + "%"}</td>
      <td class="num">${t.a.product_adoption}<span class="pa-of"> / ${PA_CFG.scoreMax}</span></td>
      <td>${adoptBlank(t.a.csm) ? dash : esc(t.a.csm)}</td>
      <td>${adoptBlank(t.a.region) ? dash : esc(t.a.region)}</td>
    </tr>`;
  }).join("");
  el.innerHTML = `<div>${head}<table class="flat-table ccr-table ccr-sortable"><thead><tr>${hdr}</tr></thead><tbody>${body}</tbody></table></div>`;
}

{{AGX_JS}}

// ── Product adoption: scatter of the two axes the rating is built from ─────
// Consumption ratio on x, product adoption (PBAC complexity + Request points)
// on y, with the rating grid shaded behind. The bands are computed from
// PA_CFG by the same rule op_list_product_adoption() applies, so the shading
// cannot drift from the stored rating — change a threshold there and here.
//
// PBAC_CFG stays for the PBAC-only mini plot on each account card.
const PBAC_CFG = { ratioMax: 1.25, scoreMax: 7, ratioMinimal: 0.25, ratioModerate: 0.75, complexityMin: 4 };
const PA_CFG   = { ratioMax: 1.25, scoreMax: 12, ratioMinimal: 0.25, ratioModerate: 0.75, adoptionMin: 4,
                   requestRetained: 3, requestAdopted: 1, complyMax: 2 };
const PBAC_COLOURS = {
  black: { dot: "#111111", bg: "#ECECEC", ink: "#374151" },
  red:   { dot: "#EF4444", bg: "#FEE2E2", ink: "#991B1B" },
  amber: { dot: "#F59E0B", bg: "#FEF3C7", ink: "#92400E" },
  green: { dot: "#22C55E", bg: "#E8F6EE", ink: "#166534" }
};
const PBAC_W = 880, PBAC_H = 520;
const PBAC_M = { top: 24, right: 28, bottom: 52, left: 56 };
const PBAC_PW = PBAC_W - PBAC_M.left - PBAC_M.right;
const PBAC_PH = PBAC_H - PBAC_M.top - PBAC_M.bottom;
const PBAC_PIN = 46;   // pinned lane for ratios past the axis maximum

// Point radius encodes contract ARR. Scaled on sqrt(ARR) so it is the dot area
// that tracks the money, which is how people read circle size — ARR spans
// roughly 15k to 4.5M here, and a linear radius would pin all but a handful of
// accounts to the floor. Accounts with no ARR draw at the minimum.
const PBAC_R_MIN = 1, PBAC_R_MAX = 3;

let pbacPinned = null, pbacHover = null, pbacRing = null;
const pbacAt = {};     // account name -> {cx, cy} while drawn, else null

const pbacX = r => PBAC_M.left + Math.min(r, PA_CFG.ratioMax) / PA_CFG.ratioMax * (PBAC_PW - PBAC_PIN);
const pbacY = s => PBAC_M.top + PBAC_PH - (Math.min(s, PA_CFG.scoreMax) / PA_CFG.scoreMax) * PBAC_PH;

// Deterministic jitter, so an account lands in the same spot every run and the
// plot is comparable week to week. The score is an integer and most accounts
// sit on a handful of values, so without this they hide each other.
function pbacSeed(str) {
  let h = 2166136261;
  for (let i = 0; i < str.length; i++) { h ^= str.charCodeAt(i); h = Math.imul(h, 16777619); }
  return ((h >>> 0) % 10000) / 10000;
}

// Band caption, one entry per line. The two red cells are named for the axis
// that is failing, matching the stored rating labels.
function pbacBandLines(key, highScore) {
  if (key === "red")   return ["RED: HIGH RISK", highScore ? "CONSUMPTION" : "ADOPTION"];
  if (key === "black") return ["BLACK: CRITICAL", "CHURN RISK"];
  return [key.toUpperCase()];
}

// Lifecycle S1 ("S1: Installation and Config.") — matched on the stage number
// or the word, so a picklist relabel does not quietly turn squares into dots.
function isInstallStage(a) {
  const st = String((a && a.lifecycle_stage) || "").trim().toLowerCase();
  return st.startsWith("s1") || st.includes("install");
}

// PBAC-only rating cell, for the mini plot on the account card.
function pbacRatingAt(score, ratio) {
  return ratingCellAt(score >= PBAC_CFG.complexityMin, ratio, PBAC_CFG);
}
// Product adoption rating cell, for the Adoption page scatter.
function paRatingAt(score, ratio) {
  return ratingCellAt(score >= PA_CFG.adoptionMin, ratio, PA_CFG);
}
function ratingCellAt(high, ratio, cfg) {
  const heavy = ratio !== null && ratio >= cfg.ratioModerate;
  const some  = ratio !== null && ratio >= cfg.ratioMinimal;
  if (high) return heavy ? "green" : (some ? "amber" : "red");
  return heavy ? "amber" : (some ? "red" : "black");
}

function svgEl(tag, attrs, text) {
  const n = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const k in attrs) n.setAttribute(k, attrs[k]);
  if (text !== undefined) n.textContent = text;
  return n;
}

// Facet + search, shared by the scatter (lit vs dimmed) and the table (shown).
function paInView(a) {
  return adoptFacet(a) && (!searchTerm || a.name.toLowerCase().includes(searchTerm));
}

function renderPbacPanel(rows) {
  const svg = document.getElementById("pbac-plot");
  svg.textContent = "";

  if (!rows.length) {
    document.getElementById("pbac-side").innerHTML =
      `<p class="ps-empty">No product adoption scores \\u2014 run the pipeline with looker = TRUE.</p>`;
    return;
  }

  // ── Background: the rating grid itself ───────────────────────────────────
  const xEdges = [0, PA_CFG.ratioMinimal, PA_CFG.ratioModerate, PA_CFG.ratioMax];
  for (let r = 0; r < 2; r++) {
    for (let c = 0; c < 3; c++) {
      const key = paRatingAt(r === 1 ? PA_CFG.scoreMax : 0, (xEdges[c] + xEdges[c + 1]) / 2);
      const x0 = pbacX(xEdges[c]);
      // The last column swallows the pinned lane, so an outlier still sits on
      // the colour its ratio earns.
      const x1 = c === 2 ? PBAC_M.left + PBAC_PW : pbacX(xEdges[c + 1]);
      const yTop = pbacY(r === 1 ? PA_CFG.scoreMax : PA_CFG.adoptionMin);
      const yBot = pbacY(r === 1 ? PA_CFG.adoptionMin : 0);
      svg.appendChild(svgEl("rect", { x: x0, y: yTop, width: x1 - x0, height: yBot - yTop, fill: PBAC_COLOURS[key].bg }));
      pbacBandLines(key, r === 1).forEach((line, i) => svg.appendChild(svgEl("text", {
        x: x0 + 7, y: yTop + 15 + i * 11, class: "pbac-band-label", fill: PBAC_COLOURS[key].ink
      }, line)));
    }
  }

  // ── Band edges, frame, axes ──────────────────────────────────────────────
  [PA_CFG.ratioMinimal, PA_CFG.ratioModerate].forEach(v => {
    svg.appendChild(svgEl("line", { x1: pbacX(v), y1: PBAC_M.top, x2: pbacX(v), y2: PBAC_M.top + PBAC_PH,
                                    stroke: "#B8B8B8", "stroke-width": 1, "stroke-dasharray": "3 3" }));
  });
  svg.appendChild(svgEl("line", { x1: PBAC_M.left, y1: pbacY(PA_CFG.adoptionMin),
                                  x2: PBAC_M.left + PBAC_PW, y2: pbacY(PA_CFG.adoptionMin),
                                  stroke: "#B8B8B8", "stroke-width": 1, "stroke-dasharray": "3 3" }));
  svg.appendChild(svgEl("line", { x1: pbacX(PA_CFG.ratioMax), y1: PBAC_M.top,
                                  x2: pbacX(PA_CFG.ratioMax), y2: PBAC_M.top + PBAC_PH,
                                  stroke: "#B8B8B8", "stroke-width": 1 }));

  for (let s = 0; s <= PA_CFG.scoreMax; s++) {
    svg.appendChild(svgEl("text", { x: PBAC_M.left - 9, y: pbacY(s) + 3.5, class: "pbac-axis-text", "text-anchor": "end" }, s));
  }
  [0, 0.25, 0.5, 0.75, 1.0, 1.25].filter(v => v <= PA_CFG.ratioMax).forEach(v => {
    svg.appendChild(svgEl("text", { x: pbacX(v), y: PBAC_M.top + PBAC_PH + 16, class: "pbac-axis-text", "text-anchor": "middle" }, v.toFixed(2)));
  });
  svg.appendChild(svgEl("text", { x: PBAC_M.left + PBAC_PW - PBAC_PIN / 2, y: PBAC_M.top + PBAC_PH + 16,
                                  class: "pbac-axis-text", "text-anchor": "middle" }, ">" + PA_CFG.ratioMax));
  svg.appendChild(svgEl("rect", { x: PBAC_M.left, y: PBAC_M.top, width: PBAC_PW, height: PBAC_PH,
                                  fill: "none", stroke: "#B8B8B8", "stroke-width": 1 }));
  svg.appendChild(svgEl("text", { x: PBAC_M.left + PBAC_PW / 2, y: PBAC_H - 14,
                                  class: "pbac-axis-title", "text-anchor": "middle" },
                        "CONSUMPTION RATIO (REGISTERED \\u00F7 LICENSED USERS)"));
  const yTitle = svgEl("text", { x: 14, y: PBAC_M.top + PBAC_PH / 2, class: "pbac-axis-title", "text-anchor": "middle" },
                       "PRODUCT ADOPTION SCORE");
  yTitle.setAttribute("transform", "rotate(-90 14 " + (PBAC_M.top + PBAC_PH / 2) + ")");
  svg.appendChild(yTitle);

  // ── Points ───────────────────────────────────────────────────────────────
  const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));

  // ARR -> radius, fitted across the accounts actually on the plot (not the
  // whole book), so the size range is used even when a facet is applied.
  const arrRoot = a => Math.sqrt(Math.max(0, parseFloat(a.arr_raw) || 0));
  const roots   = rows.map(arrRoot);
  const rLo = Math.min.apply(null, roots), rHi = Math.max.apply(null, roots);
  const radius = a => {
    const v = arrRoot(a);
    if (!isFinite(v) || rHi <= rLo) return (PBAC_R_MIN + PBAC_R_MAX) / 2;
    return PBAC_R_MIN + (v - rLo) / (rHi - rLo) * (PBAC_R_MAX - PBAC_R_MIN);
  };

  rows.forEach(a => {
    const ratio  = a.consumption_ratio_raw;
    const known  = ratio !== null && ratio !== undefined && !isNaN(ratio);
    const pinned = known && ratio > PA_CFG.ratioMax;
    const colour = ratingColour(a.product_rating) || "black";
    const col    = PBAC_COLOURS[colour];

    // Vertical jitter is a fraction of one score step (PBAC_PH / scoreMax),
    // so a point never wanders into the neighbouring integer.
    const step = PBAC_PH / PA_CFG.scoreMax;
    const jx = (pbacSeed(a.name) - 0.5) * (pinned ? PBAC_PIN * 0.7 : 16);
    const jy = (pbacSeed(a.name + "y") - 0.5) * step * 0.6;
    const cx = clamp(pinned ? (PBAC_M.left + PBAC_PW - PBAC_PIN / 2 + jx) : pbacX(known ? ratio : 0) + jx,
                     PBAC_M.left + 7, PBAC_M.left + PBAC_PW - 7);
    const cy = clamp(pbacY(a.product_adoption) + jy, PBAC_M.top + 7, PBAC_M.top + PBAC_PH - 7);

    // Out-of-facet accounts are dimmed rather than dropped: the point of the
    // plot is where a cohort sits relative to the whole book, which is lost if
    // everyone else disappears. The count under the plot says how many are lit.
    const on = paInView(a);
    pbacAt[a.name] = on ? { cx: cx, cy: cy } : null;

    // Outline scales with the dot so a 1px-radius account is not swallowed by
    // its own stroke — the fill has to stay readable, it is the rating.
    const r  = radius(a);
    const sw = Math.max(0.4, r * 0.28);

    const g = svgEl("g", { class: "pbac-pt" + (on ? "" : " dim") });
    // Square for accounts in Installation and Config. (lifecycle S1) — low
    // consumption is expected there. Side chosen for the same area as the dot.
    const side = r * 1.77;
    const c = isInstallStage(a)
      ? svgEl("rect", { x: cx - side / 2, y: cy - side / 2, width: side, height: side,
                        fill: col.dot, stroke: "#111111", "stroke-width": pinned ? sw * 2.4 : sw })
      : svgEl("circle", { cx: cx, cy: cy, r: r, fill: col.dot, stroke: "#111111",
                          "stroke-width": pinned ? sw * 2.4 : sw });
    // Unknown ratio keeps the rating fill but breaks its outline: the dot is at
    // zero as a placeholder, not because the account measured zero.
    if (!known) { c.setAttribute("stroke-dasharray", "1.8 1.8"); c.setAttribute("fill-opacity", ".45"); }
    g.appendChild(c);
    g.appendChild(svgEl("circle", { cx: cx, cy: cy, r: 11, class: "pbac-hit" }));

    if (on) {
      g.addEventListener("mouseenter", () => { pbacHover = a.name; pbacSide(); });
      g.addEventListener("mouseleave", () => { pbacHover = null;   pbacSide(); });
      g.addEventListener("click", () => { paPin(a.name, true); });
    }
    svg.appendChild(g);
  });

  // One reusable selection ring, moved rather than redrawn — rebuilding the
  // nodes under the cursor re-fires mouseenter on the replacements, which is an
  // endless redraw.
  pbacRing = svgEl("circle", { cx: 0, cy: 0, r: 9, fill: "none", stroke: "#0D0D0D",
                               "stroke-width": 2, visibility: "hidden", "pointer-events": "none" });
  svg.appendChild(pbacRing);

  const lit    = rows.filter(a => pbacAt[a.name]).length;
  const facets = [activeRegion === "all" ? null : activeRegion,
                  activeCSM    === "all" ? null : activeCSM].filter(Boolean);
  const size = " Point size is contract ARR; a square marks an account in Installation and Config. (lifecycle S1); a dashed outline means the consumption ratio is unavailable, a heavy one means it is above the axis maximum.";
  document.getElementById("pbac-facet-note").textContent = (facets.length || searchTerm
    ? `Showing ${lit} of ${rows.length} scored accounts` +
      (facets.length ? ` \\u2014 ${facets.join(", ")}` : "") +
      ". The rest stay on the plot, dimmed, for context."
    : `${rows.length} scored accounts.`) + size;

  pbacSide();
}

// Pin (or unpin) an account on the scatter. From a point click it toggles; from
// a table row it always pins and brings the plot into view.
function paPin(name, toggle) {
  pbacPinned = (toggle && pbacPinned === name) ? null : name;
  pbacSide();
  document.querySelectorAll("#adopt-table-overview tr[data-name]").forEach(tr =>
    tr.classList.toggle("pa-pinned", tr.dataset.name === pbacPinned));
  if (!toggle && name) {
    const plot = document.getElementById("pbac-plot");
    if (plot && plot.scrollIntoView) plot.scrollIntoView({ behavior: "smooth", block: "center" });
  }
}

// Table-row hover drives the same ring and side panel as a point hover.
function paHover(name) { pbacHover = name; pbacSide(); }

// "Retained (+3)" style label for an account Request stage.
function paRequestWord(a) {
  const st = covRequestStage(a);
  if (!st) return "";
  const pts = covNum(a.request_points);
  return adoptCap(st) + (pts ? ` (+${pts})` : "");
}

// The summary panel beside the plot. Hover wins while the cursor is on a point
// or a table row, so a pin never blocks reading the next account; leaving falls
// back to the pin.
function pbacSide() {
  const el   = document.getElementById("pbac-side");
  const name = pbacHover || pbacPinned;
  const a    = name ? ACCOUNTS.find(x => x.name === name) : null;
  const at   = name ? pbacAt[name] : null;

  if (pbacRing) {
    if (a && at) {
      pbacRing.setAttribute("cx", at.cx);
      pbacRing.setAttribute("cy", at.cy);
      pbacRing.setAttribute("visibility", "visible");
    } else {
      pbacRing.setAttribute("visibility", "hidden");
    }
  }

  const isPinned = pbacPinned !== null && name === pbacPinned;
  el.className = "pbac-side" + (isPinned ? " pinned" : "");

  if (!a || !at) {
    el.innerHTML = `<p class="ps-empty">Hover a point or a table row for its account summary, or click to keep it on screen while you look elsewhere.</p>`;
    return;
  }

  const ratio  = a.consumption_ratio_raw;
  const known  = ratio !== null && ratio !== undefined && !isNaN(ratio);
  const pinned = known && ratio > PA_CFG.ratioMax;
  const none   = `<span class="ov-adopt-none">None</span>`;
  const reqPts = covNum(a.request_points) || 0;
  const cmpPts = covNum(a.comply_points) || 0;
  const stage  = covRequestStage(a);
  const badge  = fmtStoplightAs(a.product_rating, pbacFullLabel(a.product_rating), a.product_rating);

  el.innerHTML =
    `<div class="ps-name"><span class="acct-link" onclick="goToAccount(${esc(JSON.stringify(a.name))})">${esc(a.name)}</span></div>` +
    `<div>${isInstallStage(a) ? badge.replace("stoplight-dot ", "stoplight-dot square ") : badge}</div>` +
    // ARR is what the dot size encodes, so the panel has to name the figure.
    `<div class="ps-kv" style="margin-top:8px"><span>Contract ARR</span><span>${esc(a.arr)}</span></div>` +

    `<div class="ps-block"><div class="ps-h">Product adoption</div>` +
      `<div class="ps-kv"><span>Score</span><span>${a.product_adoption} / ${PA_CFG.scoreMax}</span></div>` +
      `<div class="ps-kv"><span>PBAC complexity</span><span>${a.pbac_score} / ${PBAC_CFG.scoreMax}</span></div>` +
      `<div class="ps-kv"><span>Request${stage ? " (" + esc(adoptCap(stage)) + ")" : ""}</span><span>+${reqPts}</span></div>` +
      `<div class="ps-kv"><span>Comply</span><span>+${cmpPts}</span></div>` +
    `</div>` +

    `<div class="ps-block"><div class="ps-h">Consumption</div>` +
      `<div class="ps-kv"><span>Registered users</span><span>${esc(a.registered_users)}</span></div>` +
      `<div class="ps-kv"><span>Licensed users</span><span>${esc(a.licensed_users)}</span></div>` +
      `<div class="ps-kv"><span>Consumption ratio</span><span>${known ? (Math.round(ratio * 1000) / 10) + "%" : "\\u2014"}</span></div>` +
      (known
        ? (pinned ? `<p class="ps-note">Above the axis maximum of ${PA_CFG.ratioMax} \\u2014 drawn in the pinned column at the right edge.</p>` : "")
        : `<p class="ps-note">No ratio available. Drawn at zero as a placeholder; the rating reads it as not yet demonstrated, not as zero usage.</p>`) +
    `</div>` +

    `<div class="ps-block"><div class="ps-h">PBAC detail</div>` +
      `<div class="ps-sub"><span class="ps-sub-k">Platforms</span>${(a.platform_html && a.platform_html !== "\\u2014") ? a.platform_html : none}</div>` +
      `<div class="ps-sub"><span class="ps-sub-k">Feature adoption</span>${adoptBlank(a.pbac_features) ? none : esc(a.pbac_features)}</div>` +
    `</div>` +

    `<div class="ps-hint">${isPinned
      ? `Pinned. <button type="button" onclick="paPin(null, false)">Clear</button>`
      : "Click the point or row to keep this on screen."}</div>`;
}

// ── Product adoption table ─────────────────────────────────────────────────
// Every adoption and consumption field for the accounts in view, sortable on
// any column. Hovering a row rings its point on the scatter; clicking pins it.
let paSort = { col: "product_adoption", dir: -1 };

function sortPa(col) {
  // Numbers and ratings read best high-to-low first; text reads A-Z first.
  const numericFirst = ["product_adoption", "product_rating", "arr_raw", "months_as_customer", "consumption_ratio_raw",
                        "pbac_score", "request_points", "comply_adoption", "agentic_governance_adoption"];
  paSort = paSort.col === col ? { col: col, dir: -paSort.dir }
                              : { col: col, dir: numericFirst.includes(col) ? -1 : 1 };
  renderProductTable();
}

function renderProductTable() {
  const statsEl = document.getElementById("adopt-stats-overview");
  const tableEl = document.getElementById("adopt-table-overview");
  const dash    = `<span class="em-dash">\\u2014</span>`;
  const txt     = v => adoptBlank(v) ? dash : esc(v);

  const rows = ACCOUNTS.filter(paInView);
  if (!rows.length) {
    statsEl.innerHTML = "";
    tableEl.innerHTML = `<p class="empty-msg" style="padding:40px">No accounts match the current region, CSM and search filters.</p>`;
    return;
  }

  // Stats strip: rating counts across the accounts in view.
  const counts = { green: 0, amber: 0, red: 0, black: 0 };
  rows.forEach(a => { const c = ratingColour(a.product_rating); if (c in counts) counts[c]++; });
  const scored = rows.filter(hasProduct);
  const avg = scored.length
    ? Math.round(scored.reduce((s, a) => s + covNum(a.product_adoption), 0) / scored.length * 10) / 10 : null;
  const stat = (l, v) => `<div class="stat-item"><div class="stat-label">${l}</div><div class="stat-value">${v}</div></div>`;
  statsEl.innerHTML =
    stat("Accounts", rows.length) +
    stat("Avg Product Adoption", avg === null ? "\\u2014" : avg) +
    ["green", "amber", "red", "black"].map(c => stat(adoptCap(c), counts[c])).join("");

  // Request rating source: Looker tier where present, else the SFDC picklist.
  const reqSrc = a => adoptBlank(a.request_adoption_derived) ? a.request_adoption : a.request_adoption_derived;
  const ratingRank = f => a => COLOUR_RANK[ratingColour(a[f])];

  // [sort key, header, cell renderer, sort value (optional; default is text)]
  const cols = [
    ["name", "Account", a =>
      `<span class="acct-link" onclick="event.stopPropagation(); goToAccount(${esc(JSON.stringify(a.name))})">${esc(a.name)}</span>`],
    ["csm",    "CSM",    a => txt(a.csm)],
    ["region", "Region", a => txt(a.region)],
    ["arr_raw", "ARR", a => `<span class="pa-num">${txt(a.arr)}</span>`, a => covNum(a.arr_raw)],
    ["months_as_customer", "Months as Customer", a => covNum(a.months_as_customer) === null ? dash
        : `<span class="pa-num" title="Customer since ${esc(a.customer_since)}">${a.months_as_customer}</span>`,
      a => covNum(a.months_as_customer)],
    ["product_adoption", "Product Adoption", a => hasProduct(a)
        ? `<span class="pa-num pa-score">${a.product_adoption}</span><span class="pa-of"> / ${PA_CFG.scoreMax}</span>` : dash,
      a => covNum(a.product_adoption)],
    ["product_rating", "Rating", a => adoptBlank(a.product_rating) ? dash
        : fmtStoplightAs(a.product_rating, pbacFullLabel(a.product_rating), pbacFullLabel(a.product_rating)),
      ratingRank("product_rating")],
    ["consumption_ratio_raw", "Consumption", a => {
        const r = covNum(a.consumption_ratio_raw);
        return r === null ? dash
          : `<span class="pa-num" title="${esc(a.registered_users)} registered / ${esc(a.licensed_users)} licensed">${Math.round(r * 1000) / 10}%</span>`;
      }, a => covNum(a.consumption_ratio_raw)],
    ["pbac_score", "PBAC Complexity", a => covNum(a.pbac_score) === null ? dash
        : `<span class="pa-num">${a.pbac_score}</span>`, a => covNum(a.pbac_score)],
    ["request_points", "Request", a => {
        const w = paRequestWord(a);
        return w ? fmtStoplightAs(reqSrc(a), w, reqSrc(a)) : dash;
      }, a => (covNum(a.request_points) || 0) * 10 + COLOUR_RANK[ratingColour(reqSrc(a))]],
    ["comply_adoption", "Comply", a => fmtStoplight(a.comply_adoption), ratingRank("comply_adoption")],
    ["agentic_governance_adoption", "Agentic", a => fmtStoplight(a.agentic_governance_adoption),
      ratingRank("agentic_governance_adoption")],
    ["platform_html", "Platforms", a => (a.platform_html && a.platform_html !== "\\u2014") ? a.platform_html : dash],
    ["iam",           "IAM",            a => txt(a.iam)],
    ["ext_catalog",   "Ext. Catalog",   a => txt(a.ext_catalog)],
    ["pbac_features", "PBAC Features",  a => txt(a.pbac_features)],
    ["lifecycle_stage", "Lifecycle Stage", a => txt(a.lifecycle_stage)]
  ];

  const col = cols.find(c => c[0] === paSort.col) || cols.find(c => c[0] === "product_adoption");
  const key = col[3] || (a => {
    const v = a[col[0]];
    return adoptBlank(v) ? "" : String(v).replace(/<[^>]*>/g, "").toLowerCase();
  });
  const sorted = rows.slice().sort((x, y) => {
    const xv = key(x), yv = key(y);
    // Blanks sink to the bottom whichever way the column is sorted.
    const xb = xv === null || xv === "" || xv === undefined, yb = yv === null || yv === "" || yv === undefined;
    if (xb !== yb) return xb ? 1 : -1;
    const d = (typeof xv === "number" && typeof yv === "number") ? xv - yv : String(xv).localeCompare(String(yv));
    return d * paSort.dir || x.name.localeCompare(y.name);
  });

  const hdrs = cols.map(c => {
    const arrow = paSort.col === c[0] ? (paSort.dir === 1 ? " \\u2191" : " \\u2193") : "";
    return `<th onclick="sortPa(${esc(JSON.stringify(c[0]))})">${esc(c[1])}${arrow}</th>`;
  }).join("");

  const body = sorted.map(a => {
    const nm  = esc(JSON.stringify(a.name));
    const cls = [a.name === pbacPinned ? "pa-pinned" : "", isInstallStage(a) ? "pa-onboarding" : ""].join(" ").trim();
    return `<tr data-name="${esc(a.name)}" class="${cls}" onmouseenter="paHover(${nm})" onmouseleave="paHover(null)" onclick="paPin(${nm}, false)">` +
      cols.map(c => `<td>${c[2](a)}</td>`).join("") + `</tr>`;
  }).join("");

  tableEl.innerHTML = `<table class="flat-table pa-table"><thead><tr>${hdrs}</tr></thead><tbody>${body}</tbody></table>`;
}

// How the score and rating are derived — the second Adoption sub-tab. Static;
// the thresholds and weights are read from PA_CFG / PBAC_CFG so the prose
// cannot fall out of step with the shading.
function productWriteupHtml() {
  const cell = (k, label) =>
    `<td style="background:${PBAC_COLOURS[k].bg};color:${PBAC_COLOURS[k].ink}">${label}</td>`;
  const row = (name, desc, w) =>
    `<tr><td><b>${name}</b><br><span style="color:var(--text-3);font-size:11.5px">${desc}</span></td><td class="w">${w}</td></tr>`;
  const pbacWeights = [
    ["Multi-platform", "Data sources registered on more than one platform in Production.", 3],
    ["Data &amp; subscription policies", "Both kinds of global policy active \\u2014 the account governs who gets in and what they see.", 1],
    ["Sensitive Data Discovery", "An SDD run within the last 90 days.", 1],
    ["Domains", "On the Domains dashboard retained-accounts tile \\u2014 governance is delegated, not centrally hand-run.", 1],
    ["Guardrails", "At least one active guardrail policy.", 1]
  ].map(w => row(w[0], w[1], w[2])).join("");
  const requestWeights = [
    ["Retained", "On the Request dashboard retained tile \\u2014 in use for 3 of the last 4 weeks.", PA_CFG.requestRetained],
    ["Adopted", "A Request use case in production, not yet retained.", PA_CFG.requestAdopted],
    ["Tried / none", "POC or planning only, or no Request workflow.", 0]
  ].map(w => row(w[0], w[1], w[2])).join("");

  return `
    <h3>How the product adoption score is derived</h3>
    <p>Seat usage on its own is a poor read on adoption. An account can log in constantly and still run one trivial policy on one platform; another can have modest login numbers while governing three platforms with guardrails and discovery in place, and running access requests through Immuta. The rating scores each account on two independent axes and crosses the result \\u2014 <b>product adoption</b> (how much the account asks of the product) against <b>consumption</b> (how much of what they bought they use).</p>

    <h3>Product adoption score</h3>
    <p>The score runs 0\\u2013${PA_CFG.scoreMax} and is the sum of three parts: the PBAC complexity score (0\\u2013${PBAC_CFG.scoreMax}), Request adoption points (0\\u2013${PA_CFG.requestRetained}) and Comply points (0\\u2013${PA_CFG.complyMax}).</p>

    <h3>1. PBAC complexity (0\\u2013${PBAC_CFG.scoreMax})</h3>
    <p>Five yes/no signals from the Looker consumption looks and the Domains dashboard. Multi-platform is worth three because running policy across two or more platforms is the qualitative jump; the rest are worth one each. Policy signals count Production instances only \\u2014 a guardrail on a dev instance is not production complexity.</p>
    <table class="pbac-weights"><tbody>${pbacWeights}</tbody></table>

    <h3>2. Request adoption (0\\u2013${PA_CFG.requestRetained})</h3>
    <p>Points for the account Request stage. The stage comes from the Looker Request metrics dashboard (1009); an account on none of its stage tiles is None. Retained carries the same weight as multi-platform: a request workflow people use week after week is as strong a signal as governing a second platform.</p>
    <table class="pbac-weights"><tbody>${requestWeights}</tbody></table>

    <h3>3. Comply (0\\u2013${PA_CFG.complyMax})</h3>
    <p>One point each for <code>access_summary_export</code> and <code>comply_enabled</code> in the account Features List (Looker look 1814, Production instances). <code>comply_enabled</code> rates Comply Amber; <code>access_summary_export</code> alone rates it Red; neither is Black.</p>

    <p>The threshold for <b>high adoption</b> is ${PA_CFG.adoptionMin}. Routes past it include multi-platform plus any one other PBAC signal or an adopted Request workflow (3 + 1), a retained Request workflow plus any one PBAC signal (3 + 1), or all four single-weight PBAC signals. Multi-platform alone, or a retained Request workflow alone, scores 3 and does not clear it.</p>

    <h3>Crossing the axes</h3>
    <table class="pbac-matrix"><tbody>
      <tr><th></th><th>Ratio &lt; ${PA_CFG.ratioMinimal}</th><th>${PA_CFG.ratioMinimal} \\u2013 ${PA_CFG.ratioModerate}</th><th>Ratio &ge; ${PA_CFG.ratioModerate}</th></tr>
      <tr><th class="rowh">score &ge; ${PA_CFG.adoptionMin}</th>${cell("red", "Red: High Risk - Consumption")}${cell("amber", "Amber")}${cell("green", "Green")}</tr>
      <tr><th class="rowh">score &lt; ${PA_CFG.adoptionMin}</th>${cell("black", "Black: Critical Churn Risk")}${cell("red", "Red: High Risk - Adoption")}${cell("amber", "Amber")}</tr>
    </tbody></table>
    <p>Note the asymmetry: adoption alone caps at <b>red</b>, consumption alone caps at <b>amber</b>. Neither axis reaches green by itself \\u2014 green needs the deployment to be doing demanding work <i>and</i> people to be there doing it.</p>

    <h3>Missing data</h3>
    <ul>
      <li><b>A missing PBAC signal scores zero.</b> If an account is absent from the SDD look, no SDD has run \\u2014 a real negative, not a gap.</li>
      <li><b>An unknown Request stage scores zero.</b> No Looker stage and no Salesforce picklist value means no Request points.</li>
      <li><b>A missing consumption ratio stays unknown.</b> It is not forced to zero, because an unknown ratio is not the same as no usage. The rating reads it as not yet demonstrated, so such an account can reach red on adoption alone but no further.</li>
      <li><b>Zero licensed users is an ELA, not missing data.</b> Unlimited seats means there is no denominator to divide by, so the ratio is 1 \\u2014 those accounts are consuming everything they are entitled to. An ELA with nobody registered is still 0.</li>
    </ul>
    <p>The ratio is deliberately kept out of the score. It measures seat uptake, not deployment sophistication, and folding it in would collapse the two axes back into the single number this rating exists to avoid.</p>

    <h3>Where this differs from the PBAC rating</h3>
    <p>The PBAC rating on each account card (the first square of the four-square glyph, and the mini plot on the card Overview tab) is unchanged: it crosses PBAC complexity alone against consumption, with the same thresholds. The product adoption rating on this page adds Request on top, so an account can sit higher here than on its card \\u2014 never lower.</p>

    <h3>Reading a point</h3>
    <p>The colour gives severity; the pair behind it gives the play. <b>Red: High Risk - Consumption</b> (high score, thin ratio) is an enablement problem \\u2014 the platform work is done, the users are not there. <b>Red: High Risk - Adoption</b> (low score, some usage) is a depth problem \\u2014 people are there, nothing hard is being asked of the product. Same colour, opposite conversations. <b>Black: Critical Churn Risk</b> should move fastest: neither axis has cleared its floor, and there is no adoption story to tell at renewal.</p>`;
}

// ── Escalations ────────────────────────────────────────────────────────────
// op_list$escalations, one row per open CUSTX Jira issue. Severity, status and
// customer are facets of their own; region and CSM reuse the global pills.
let escSev = "all", escStatus = "all", escCustomer = "all";

function setEscSev(v)      { escSev      = v; renderEscalations(); }
function setEscStatus(v)   { escStatus   = v; renderEscalations(); }
function setEscCustomer(v) { escCustomer = v; renderEscalations(); }

// Pill row from the distinct values present, built from the rows the OTHER
// facets already allow — so a status pill never offers a combination that
// yields nothing.
function escPills(containerId, values, active, fn, allLabel) {
  const el = document.getElementById(containerId);
  if (!el) return;
  el.innerHTML = [`<button class="flat-pill${active === "all" ? " active" : ""}" onclick="${fn}(\'all\')">${allLabel}</button>`]
    .concat(values.map(v =>
      `<button class="flat-pill${active === v ? " active" : ""}" onclick="${fn}(\'${esc(String(v).replace(/\\\\/g, "\\\\\\\\").replace(/\'/g, "\\\\\'"))}\')">${esc(v)}</button>`
    )).join("");
}

function renderEscalations() {
  const statsEl = document.getElementById("esc-stats");
  const listEl  = document.getElementById("esc-list");
  const all     = (ESCALATIONS_DATA && ESCALATIONS_DATA.rows) || [];

  // Region and CSM come from the global pills; severity, status and customer
  // are local to this tab.
  const inFacet = r => (activeRegion === "all" || r.Region === activeRegion) &&
                       (activeCSM    === "all" || r.csm    === activeCSM);

  const base = all.filter(inFacet);
  setCount("sub-count-escalations", base.length);

  // Pill options come from `base`, so they narrow with the global facets.
  const uniq = (arr, k) => [...new Set(arr.map(r => r[k]).filter(Boolean))];
  escPills("esc-sev-filters",
           uniq(base, "severity").sort((a, b) => escRank(a, base) - escRank(b, base)),
           escSev, "setEscSev", "All severities");
  escPills("esc-status-filters", uniq(base, "status").sort(), escStatus, "setEscStatus", "All statuses");
  buildFlatPills("esc-region-filters", "esc");
  buildFlatCSMPills("esc-csm-filters");

  let rows = base;
  if (escSev      !== "all") rows = rows.filter(r => r.severity === escSev);
  if (escStatus   !== "all") rows = rows.filter(r => r.status   === escStatus);
  if (escCustomer !== "all") rows = rows.filter(r => r["Account Name"] === escCustomer);
  if (searchTerm) {
    rows = rows.filter(r =>
      String(r["Account Name"] || "").toLowerCase().includes(searchTerm) ||
      String(r.summary || "").toLowerCase().includes(searchTerm) ||
      String(r.key || "").toLowerCase().includes(searchTerm));
  }

  if (!rows.length) {
    statsEl.innerHTML = "";
    listEl.innerHTML = `<p class="empty-msg" style="padding:40px">${
      all.length
        ? "No open escalations match the current filters."
        : "No escalation data \\u2014 set JIRA_EMAIL and JIRA_TOKEN in .Renviron and re-run the pipeline."}</p>`;
    return;
  }

  const bySev = {};
  rows.forEach(r => { bySev[r.severity] = (bySev[r.severity] || 0) + 1; });
  const sevOrder = Object.keys(bySev).sort((a, b) => escRank(a, rows) - escRank(b, rows));
  const customers = new Set(rows.map(r => r["Account Name"]).filter(Boolean));
  const oldest = Math.max.apply(null, rows.map(r => r.age_days || 0));

  statsEl.innerHTML =
    `<div class="stat-item"><div class="stat-label">Open</div><div class="stat-value">${rows.length}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Customers</div><div class="stat-value">${customers.size}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Oldest</div><div class="stat-value">${oldest}d</div></div>` +
    sevOrder.map(s =>
      `<div class="stat-item"><div class="stat-label">${esc(s)}</div><div class="stat-value">${bySev[s]}</div></div>`
    ).join("");

  // Already sorted by severity then age in op_list_escalations(); re-sorting
  // here keeps that true after filtering.
  const sorted = rows.slice().sort((a, b) =>
    (a.severity_rank - b.severity_rank) || ((b.age_days || 0) - (a.age_days || 0)));

  listEl.innerHTML = sorted.map(r => {
    const rank = Math.min(Math.max(r.severity_rank || 4, 1), 4);
    const acct = r["Account Name"];
    return `<div class="esc-row sev-${rank}">
      <span class="esc-sev esc-sev-${rank}">${esc(r.severity || "\\u2014")}</span>
      <div class="esc-summary">${esc(r.summary || "\\u2014")}</div>
      <div class="esc-age${(r.age_days || 0) >= 90 ? " stale" : ""}">${r.age_days != null ? r.age_days + "d open" : ""}</div>
      <div class="esc-meta">
        <a class="esc-key" href="${esc(r.url)}" target="_blank" rel="noopener">${esc(r.key)}</a>
        <span>${acct
          ? `<b><span class="acct-link" onclick="goToAccount(\'${esc(acct)}\')">${esc(acct)}</span></b>`
          : `<span class="esc-unmatched">no account matched</span>`}</span>
        <span>${esc(r.status || "\\u2014")}</span>
        <span>${esc(r.Region || "\\u2014")}</span>
        <span>${esc(r.csm || "\\u2014")}</span>
        <span>${esc(r.assignee || "unassigned")}</span>
        <span>opened ${esc(r.created || "\\u2014")}</span>
      </div>
    </div>`;
  }).join("");
}

// Severity sort position, read off the rank the R side already computed rather
// than re-deriving it from the label — the two must not disagree.
function escRank(sev, rows) {
  const hit = rows.find(r => r.severity === sev);
  return hit ? (hit.severity_rank || 9) : 9;
}

// ── SaaS migration board ───────────────────────────────────────────────────
// op_list$saas_pipeline, one column per estimated fiscal quarter. The report is
// already scoped to in-flight migrations, so everything in it is "ongoing" —
// there is no completed stage to filter out.

// Stage order is weakest to strongest. The picklist is mid-rename: two values
// carry the new numeric prefix and two are legacy text, which is why this is an
// explicit list rather than a sort on the string. Anything unrecognised sorts
// last and renders grey, so a new picklist value shows up rather than vanishing.
const SAAS_STAGES = [
  { match: "1 - pipeline",            key: "pipeline",  label: "Pipeline" },
  { match: "having conversations",    key: "talking",   label: "Having conversations" },
  { match: "verbal commitment",       key: "committed", label: "Verbal commitment" },
  { match: "2 - migration in progress", key: "running", label: "Migration in progress" }
];

function saasStage(v) {
  const s = String(v || "").trim().toLowerCase();
  const i = SAAS_STAGES.findIndex(st => st.match === s);
  if (i === -1) return { rank: SAAS_STAGES.length, key: "pipeline", label: v || "—" };
  return { rank: i, key: SAAS_STAGES[i].key, label: SAAS_STAGES[i].label };
}

// "Q3-2027" -> 20273, so quarters sort chronologically rather than as strings
// (where Q1-2028 would land before Q3-2027).
function saasQuarterKey(q) {
  const m = String(q || "").match(/^Q([1-4])-(\\d{4})$/);
  return m ? parseInt(m[2], 10) * 10 + parseInt(m[1], 10) : Infinity;
}

// The Immuta fiscal year starts in February, so Q1 is Feb-Apr of the year the
// label counts back from — the same convention as date_to_fiscal_quarter().
function saasQuarterMonths(q) {
  const m = String(q || "").match(/^Q([1-4])-(\\d{4})$/);
  if (!m) return "";
  const qn = parseInt(m[1], 10), fyStart = parseInt(m[2], 10) - 1;
  const names = ["Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan"];
  // Index 11 is January, which falls in the next calendar year — so Q4 reads
  // "Nov 26 - Jan 27" rather than putting both months in the same year.
  const yr = i => String(i === 11 ? fyStart + 1 : fyStart).slice(2);
  const s = (qn - 1) * 3, e = s + 2;
  return yr(s) === yr(e)
    ? names[s] + "\\u2013" + names[e] + " " + yr(e)
    : names[s] + " " + yr(s) + " \\u2013 " + names[e] + " " + yr(e);
}

function toggleSaaSCard(el) { el.classList.toggle("open"); }

function renderSaaSMigration() {
  buildFlatPills("saas-region-filters", "saas");
  buildFlatCSMPills("saas-csm-filters");

  const statsEl = document.getElementById("saas-stats");
  const boardEl = document.getElementById("saas-board");
  const all     = (SAAS_DATA && SAAS_DATA.rows) || [];

  setCount("sub-count-saas_migration", all.filter(adoptFacet2).length);

  let rows = all.filter(adoptFacet2);
  if (searchTerm) {
    rows = rows.filter(r => String(r["Account Name"] || "").toLowerCase().includes(searchTerm));
  }

  if (!rows.length) {
    statsEl.innerHTML = "";
    boardEl.innerHTML = `<p class="empty-msg" style="padding:40px">${
      all.length ? "No migrations match the current region, CSM and search filters."
                 : "No SaaS migration pipeline data \\u2014 check the Salesforce report in main.R."}</p>`;
    return;
  }

  // Stats: one per stage, in pipeline order, plus the in-flight total.
  const byStage = {};
  rows.forEach(r => { const k = saasStage(r["SaaS Migration Pipeline Stage"]).label;
                      byStage[k] = (byStage[k] || 0) + 1; });
  const ordered = SAAS_STAGES.map(s => s.label).filter(l => byStage[l]);
  Object.keys(byStage).forEach(l => { if (!ordered.includes(l)) ordered.push(l); });

  // ── TGT SaaS % ──────────────────────────────────────────────────────────
  // For each quarter: (SaaS customers today + migrations scheduled in that
  // quarter or earlier) / customer count. Pipeline rows whose notes say
  // "assumed churn" are never counted as migrated, and drop out of the
  // customer count from the account\'s churn quarter on (its next renewal\'s
  // Churn Quarter; failing that its migration period; failing both, every
  // quarter). Computed on the region/CSM facet but NOT the search box —
  // a search narrows the cards, not the book the percentage is of.
  const book      = ACCOUNTS.filter(a => (activeRegion === "all" || a.region === activeRegion) &&
                                         (activeCSM    === "all" || a.csm    === activeCSM));
  const saasNow   = book.filter(a => a.is_saas).length;
  const facetRows = all.filter(adoptFacet2);
  const tgtFor = q => {
    const k = saasQuarterKey(q);
    let migrated = 0, churned = 0;
    facetRows.forEach(r => {
      const rq = String(r["Estimated SaaS Migration Period"] || "").trim();
      const rk = saasQuarterKey(rq);                 // Infinity when unscheduled
      if (saasWillChurn(r)) {
        const ck = saasQuarterKey(saasChurnQuarter(r));
        if (ck === Infinity || ck <= k) churned++;
      }
      else if (rk <= k) migrated++;
    });
    const denom = book.length - churned;
    return { saas: saasNow + migrated, denom: denom,
             pct: denom > 0 ? (saasNow + migrated) / denom : null };
  };
  const pctTxt = v => v === null ? "\\u2014" : (Math.round(v * 1000) / 10) + "%";

  statsEl.innerHTML =
    `<div class="stat-item"><div class="stat-label">SaaS today</div><div class="stat-value">${
      pctTxt(book.length ? saasNow / book.length : null)}</div></div>` +
    `<div class="stat-item"><div class="stat-label">In flight</div><div class="stat-value">${rows.length}</div></div>` +
    ordered.map(l =>
      `<div class="stat-item"><div class="stat-label">${esc(l)}</div><div class="stat-value">${byStage[l]}</div></div>`
    ).join("");

  // Group by estimated quarter. Anything without one gets its own column at the
  // end rather than being dropped — an unscheduled migration is the one most
  // worth seeing.
  const groups = {};
  rows.forEach(r => {
    const q = String(r["Estimated SaaS Migration Period"] || "").trim() || "Unscheduled";
    (groups[q] = groups[q] || []).push(r);
  });

  const quarters = Object.keys(groups).sort((a, b) => saasQuarterKey(a) - saasQuarterKey(b));
  const nowKey   = saasQuarterKey(currentFiscalQuarter());
  const today    = new Date().toISOString().slice(0, 10);

  boardEl.innerHTML = quarters.map(q => {
    const cards = groups[q].slice().sort((a, b) => {
      const d = saasStage(b["SaaS Migration Pipeline Stage"]).rank -
                saasStage(a["SaaS Migration Pipeline Stage"]).rank;
      return d !== 0 ? d
        : String(a["Account Name"] || "").localeCompare(String(b["Account Name"] || ""));
    }).map(r => {
      const st   = saasStage(r["SaaS Migration Pipeline Stage"]);
      const due  = r["Est. Date to Move Out of Stage (SM)"];
      const late = due && due < today;
      const note = r["SaaS Migration Pipeline Notes"];
      return `<div class="saas-card st-${st.key}" onclick="toggleSaaSCard(this)">
        <div class="saas-card-name">${esc(r["Account Name"])}${
          saasWillChurn(r) ? `<span class="saas-churn-tag">Assumed churn${
            saasChurnQuarter(r) ? " \\u00b7 " + esc(saasChurnQuarter(r)) : ""}</span>` : ""}</div>
        <div class="saas-card-meta">
          <span>${esc(r.Region || "\\u2014")}</span>
          <span>${esc(r.csm || "\\u2014")}</span>
        </div>
        <div class="saas-card-meta">
          <span>Out of stage: ${due
            ? `<span class="${late ? "saas-overdue" : ""}">${esc(due)}${late ? " (past)" : ""}</span>`
            : "\\u2014"}</span>
        </div>
        <span class="saas-stage saas-s-${st.key}">${esc(st.label)}</span>
        ${note ? `<div class="saas-note">${esc(note)}</div>
                  <div class="saas-note-hint">Click for notes</div>` : ""}
      </div>`;
    }).join("");

    const isCurrent = saasQuarterKey(q) === nowKey;
    const months    = saasQuarterMonths(q);
    return `<div class="saas-col${isCurrent ? " current" : ""}">
      <div class="saas-col-head">
        <div>
          <div class="saas-q">${esc(q)}${isCurrent ? " \\u00b7 now" : ""}</div>
          ${months ? `<div class="saas-q-sub">${months}</div>` : ""}
        </div>
        <span class="saas-q-n">${groups[q].length}</span>
      </div>
      ${saasQuarterKey(q) === Infinity ? "" : (() => {
        const t = tgtFor(q);
        return `<div class="saas-tgt" title="SaaS customers today plus migrations due by ${esc(q)}, over customers net of expected churn">
          <span><span class="saas-tgt-k">TGT SaaS %</span><br><span class="saas-tgt-v">${pctTxt(t.pct)}</span></span>
          <span class="saas-tgt-sub">${t.saas} / ${t.denom}</span>
        </div>`;
      })()}
      <div class="saas-col-body">${cards}</div>
    </div>`;
  }).join("");
}

// A pipeline row whose notes mark the customer as "assumed churn" rather than
// a migration. ("will churn" is the older wording, still honoured.)
function saasWillChurn(r) {
  return /(assumed|will)\\s+churn/i.test(String(r["SaaS Migration Pipeline Notes"] || ""));
}

// The quarter an assumed-churn customer leaves the book: the Churn Quarter on
// its next renewal, else its Estimated SaaS Migration Period, else "" (which
// removes it from every quarter).
function saasChurnQuarter(r) {
  const name = String(r["Account Name"] || "").trim();
  const a = ACCOUNTS.find(x => x.name === name);
  const nr = a ? nextRenewal(a) : null;
  const cq = nr ? String(nr.churn_quarter || "").trim() : "";
  if (/^Q[1-4]-\\d{4}$/.test(cq)) return cq;
  const mq = String(r["Estimated SaaS Migration Period"] || "").trim();
  return /^Q[1-4]-\\d{4}$/.test(mq) ? mq : "";
}

// The facet, against a saas_pipeline row rather than an account record — same
// rule as adoptFacet(), different field names.
function adoptFacet2(r) {
  return (activeRegion === "all" || r.Region === activeRegion) &&
         (activeCSM    === "all" || r.csm    === activeCSM);
}

// Today as a fiscal quarter label, matching date_to_fiscal_quarter(): the
// fiscal year starts in February and is labelled with the calendar year it
// ends in, so February 2026 is Q1-2027.
function currentFiscalQuarter() {
  const d = new Date(), m = d.getMonth(), y = d.getFullYear();   // m: 0 = Jan
  const fyEnd = m >= 1 ? y + 1 : y;                              // Feb onwards -> next year
  const qn = Math.floor(((m + 11) % 12) / 3) + 1;                // Feb-Apr -> 1
  return "Q" + qn + "-" + fyEnd;
}

// ── Self-Managed ─────────────────────────────────────────────────────────────
function smVersionStatus(r) {
  const v = r["Customer on Unsupported Version"];
  if (v === null || v === undefined || v === "" || v === "NA") return "Unknown";
  return (v === true || v === "TRUE" || v === "Yes" || v === "yes") ? "Unsupported" : "Supported";
}

function smSupportBadge(status) {
  const cls = status === "Supported" ? "arch-yes" : status === "Unsupported" ? "arch-warn" : "arch-no";
  return `<span class="arch-badge ${cls}">${esc(status)}</span>`;
}

function setSMSupport(val) {
  activeSMSupport = val;
  document.querySelectorAll(\'.flat-pill[data-sm-support]\').forEach(b =>
    b.classList.toggle("active", b.dataset.smSupport === val));
  renderSelfManaged();
}

function setSMLifecycle(val) {
  activeSMLifecycle = val;
  document.querySelectorAll(\'.flat-pill[data-sm-lifecycle]\').forEach(b =>
    b.classList.toggle("active", b.dataset.smLifecycle === val));
  renderSelfManaged();
}

function sortSM(col) {
  smSort.dir = smSort.col === col ? -smSort.dir : 1;
  smSort.col = col;
  renderSelfManaged();
}

function renderSelfManaged() {
  const allRows = (CONSUMPTION_DATA && CONSUMPTION_DATA.rows) || [];
  const selfManagedRows = allRows.filter(r =>
    !(r["Is SaaS Customer"] === true || r["Is SaaS Customer"] === "TRUE")
  );

  setCount("tab-count-performance", selfManagedRows.length);
  setCount("sub-count-self_managed", selfManagedRows.length);

  let rows = selfManagedRows;
  if (activeSMSupport !== "all") rows = rows.filter(r => smVersionStatus(r) === activeSMSupport);
  if (activeSMLifecycle !== "all") rows = rows.filter(r => (r["Lifecycle Stage"] || "") === activeSMLifecycle);

  const statsEl = document.getElementById("sm-stats");
  const tableEl = document.getElementById("sm-table");

  if (!selfManagedRows.length) {
    statsEl.innerHTML = "";
    tableEl.innerHTML = \'<p class="empty-msg" style="padding:40px">No self-managed accounts found, or consumption data hasn\\\'t been refreshed yet.</p>\';
    return;
  }

  const unsupportedCount = selfManagedRows.filter(r => smVersionStatus(r) === "Unsupported").length;
  const supportedCount   = selfManagedRows.filter(r => smVersionStatus(r) === "Supported").length;
  const lowConCount      = selfManagedRows.filter(r => r["Low Consumption"] === true).length;
  statsEl.innerHTML =
    `<div class="stat-item"><div class="stat-label">Self-Managed</div><div class="stat-value">${selfManagedRows.length}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Supported</div><div class="stat-value">${supportedCount}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Unsupported</div><div class="stat-value">${unsupportedCount}</div></div>` +
    `<div class="stat-item"><div class="stat-label">Low Consumption</div><div class="stat-value">${lowConCount}</div></div>`;

  if (!rows.length) {
    tableEl.innerHTML = \'<p class="empty-msg" style="padding:40px">No self-managed accounts match the current filters.</p>\';
    return;
  }

  const cols = SM_COLS;
  const sorted = smSort.col ? rows.slice().sort((a, b) => {
    let av, bv;
    if (smSort.col === "Support Status") { av = smVersionStatus(a); bv = smVersionStatus(b); }
    else { av = a[smSort.col]; bv = b[smSort.col]; }
    const anum = parseFloat(av), bnum = parseFloat(bv);
    if (!isNaN(anum) && !isNaN(bnum) && String(av).trim() !== "" && String(bv).trim() !== "") {
      return (anum - bnum) * smSort.dir;
    }
    return String(av || "").localeCompare(String(bv || "")) * smSort.dir;
  }) : rows;

  const hdrs = cols.map(c => {
    const arrow = smSort.col === c ? (smSort.dir === 1 ? " \\u2191" : " \\u2193") : "";
    return `<th onclick="sortSM(\'${c}\')" style="cursor:pointer">${esc(c)}${arrow}</th>`;
  }).join("");

  const brs = sorted.map(r => {
    const rowCls = r["Low Consumption"] === true ? \' class="low-con"\' : "";
    return `<tr${rowCls}>` + cols.map(c => {
      if (c === "Support Status") return `<td>${smSupportBadge(smVersionStatus(r))}</td>`;
      const v = r[c];
      if (c === "Consumption Ratio") {
        const pct = parseFloat(v);
        if (v === null || v === undefined || v === "NA" || isNaN(pct)) return `<td><span class="em-dash">—</span></td>`;
        return `<td class="${pctClass(pct)}">${Math.round(pct * 1000) / 10}%</td>`;
      }
      if (v === null || v === undefined || v === "" || v === "NA") return `<td><span class="em-dash">—</span></td>`;
      const n = parseFloat(v);
      if (!isNaN(n) && typeof v !== "boolean" && String(v).trim() !== "" && c !== "Immuta Version")
        return `<td style="text-align:right">${n.toLocaleString("en-US", {maximumFractionDigits: 2})}</td>`;
      return `<td>${esc(String(v))}</td>`;
    }).join("") + "</tr>";
  }).join("");

  tableEl.innerHTML = `<table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${brs}</tbody></table>`;
}

// ── Accounts render ────────────────────────────────────────────────────────
function renderAccounts() {
  const q      = searchTerm.toLowerCase();
  const region = activeRegion;
  const visible = ACCOUNTS.filter(a =>
    (region === "all" || a.region === region) &&
    (activeCSM === "all" || a.csm === activeCSM) &&
    (!q || a.name.toLowerCase().includes(q))
  );

  let html = "";
  if (accountSort === "end_date") {
    // One group per churn quarter of the account\'s NEXT renewal (earliest end
    // date), in quarter order; accounts are by end date inside each group.
    const groups = groupByChurnQuarter(visible.map(a => {
      const nr = nextRenewal(a);
      return {
        a: a,
        end: nr ? nr.end_date_raw : "",
        quarter: nr ? nr.churn_quarter : null,
        amount: nr ? (+nr.amount_raw || 0) : 0
      };
    }), x => x.quarter, x => x.end, 1);

    groups.forEach(g => {
      const arr = g.items.reduce((s, x) => s + x.amount, 0);
      const n   = g.items.length;
      html += `<div class="letter-group" id="${g.anchor}">` +
        `<div class="letter-heading cq-heading">` +
          `<span class="cq-label">${esc(g.label)}</span>` +
          (g.months ? `<span class="cq-months">${esc(g.months)}</span>` : "") +
          `<span class="cq-meta">${n} account${n !== 1 ? "s" : ""}` +
            (arr > 0 ? ` &middot; ${fmtUsd(arr)} renewing` : "") + `</span>` +
        `</div>`;
      g.items.forEach(x => { html += buildCard(x.a); });
      html += "</div>";
    });
    buildQuarterJump(groups);
  } else {
    const groups = {};
    visible.forEach(a => {
      const letter = (a.name[0] || "#").toUpperCase();
      if (!groups[letter]) groups[letter] = [];
      groups[letter].push(a);
    });
    const letters = Object.keys(groups).sort();
    letters.forEach(letter => {
      html += `<div class="letter-group" id="letter-${letter}"><div class="letter-heading">${letter}</div>`;
      groups[letter].forEach(a => { html += buildCard(a); });
      html += "</div>";
    });
    buildAlphaJump(letters);
  }

  document.getElementById("account-list").innerHTML = html;
  document.getElementById("no-results").style.display = visible.length === 0 ? "block" : "none";
  document.getElementById("count-badge").textContent = visible.length + " account" + (visible.length !== 1 ? "s" : "");
  // The page badge tracks the Accounts sub-tab: it is the account count, and
  // the other three panels are all views onto the same set.
  setCount("tab-count-engagement", visible.length);
  setCount("sub-count-accounts",   visible.length);
}

function setAccountSort(mode) {
  accountSort = mode;
  document.querySelectorAll("[data-acctsort]").forEach(b =>
    b.classList.toggle("active", b.dataset.acctsort === mode));
  renderAccounts();
}

// ── Churn-quarter grouping (Accounts, Renewals, Expansions) ────────────────
// The account\'s next renewal: earliest end date, ignoring undated ones unless
// that is all there is.
function nextRenewal(a) {
  const rs = (a.renewals || []).slice();
  if (rs.length === 0) return null;
  rs.sort((x, y) => {
    const xe = x.end_date_raw || "9999", ye = y.end_date_raw || "9999";
    return xe < ye ? -1 : xe > ye ? 1 : 0;
  });
  return rs[0];
}

// Buckets items by fiscal quarter label, in chronological order (reversed when
// dir is -1), with items inside each bucket ordered by sortKey. A missing
// quarter (null) means "no open renewal"; a blank/em-dash one means the
// renewal has no churn quarter set. Both sort after every real quarter.
function groupByChurnQuarter(items, quarterOf, sortKey, dir) {
  const NONE = "__none__", UNSET = "__unset__";
  const buckets = {};
  items.forEach(x => {
    const raw = quarterOf(x);
    const k = raw === null || raw === undefined ? NONE
            : (/^Q[1-4]-\\d{4}$/.test(String(raw).trim()) ? String(raw).trim() : UNSET);
    (buckets[k] = buckets[k] || []).push(x);
  });
  const rank = k => k === NONE ? Infinity : k === UNSET ? 1e9 : saasQuarterKey(k);
  const keys = Object.keys(buckets).sort((x, y) => {
    const special = (k) => k === NONE || k === UNSET;
    // Unset / none stay at the bottom whichever way the dates run.
    if (special(x) || special(y)) return rank(x) - rank(y);
    return (rank(x) - rank(y)) * dir;
  });
  return keys.map(k => ({
    key: k,
    label: k === NONE ? "No open renewal" : k === UNSET ? "No churn quarter" : k,
    months: (k === NONE || k === UNSET) ? "" : saasQuarterMonths(k),
    anchor: "cq-" + k.replace(/[^A-Za-z0-9]/g, "_"),
    items: buckets[k].slice().sort((x, y) => {
      const xa = sortKey(x) || "9999", ya = sortKey(y) || "9999";
      return (xa < ya ? -1 : xa > ya ? 1 : 0) * dir;
    })
  }));
}

// Group heading row for the flat Renewals / Expansions tables.
function cqGroupRow(g, colspan, noun, amountOf) {
  const n   = g.items.length;
  const tot = g.items.reduce((s, x) => s + (+amountOf(x) || 0), 0);
  return `<tr class="cq-group-row"><td colspan="${colspan}">${esc(g.label)}` +
    (g.months ? `<span class="cq-months">${esc(g.months)}</span>` : "") +
    `<span class="cq-meta">${n} ${noun}${n !== 1 ? "s" : ""}` +
    (tot > 0 ? ` &middot; ${fmtUsd(tot)}` : "") + `</span></td></tr>`;
}

// Fiscal quarter label for a "YYYY-MM-DD" date — the JS twin of
// date_to_fiscal_quarter(): FY starts in February and is labelled by the
// calendar year it ends in, so March 2026 is "Q1-2027".
function dateToFiscalQuarter(ymd) {
  const m = String(ymd || "").match(/^(\\d{4})-(\\d{2})/);
  if (!m) return "";
  const y = parseInt(m[1], 10), mo = parseInt(m[2], 10);
  const q = Math.floor(((mo - 2 + 12) % 12) / 3) + 1;
  return "Q" + q + "-" + (mo >= 2 ? y + 1 : y);
}

function expansionQuarter(e) {
  const fp = String(e.fiscal_period || "").trim();
  return /^Q[1-4]-\\d{4}$/.test(fp) ? fp : dateToFiscalQuarter(e.close_date_raw);
}

// Sidebar jump links for the End Date view — one per churn-quarter group.
function buildQuarterJump(groups) {
  const label = document.getElementById("jump-label");
  if (label) label.textContent = "Jump to Quarter";
  document.getElementById("alpha-jump").innerHTML = groups.map(g =>
    `<a href="#${g.anchor}" class="has-accounts cq-jump">${esc(g.key.indexOf("__") === 0 ? (g.key === "__none__" ? "None" : "Unset") : g.label)}</a>`
  ).join("");
}

// ── Renewals render ────────────────────────────────────────────────────────
function renderRenewals() {
  const q = searchTerm.toLowerCase();
  const region = activeRegion;

  let rows = ALL_RENEWALS.filter(r =>
    (region === "all" || r.region === region) &&
    (activeCSM === "all" || r.csm === activeCSM) &&
    (!q || r.account.toLowerCase().includes(q))
  );

  // Sort
  rows = rows.slice().sort((a, b) => {
    let av = a[renewalSort.col] || "", bv = b[renewalSort.col] || "";
    if (renewalSort.col === "amount_raw") { av = +av || 0; bv = +bv || 0; }
    return renewalSort.dir * (av < bv ? -1 : av > bv ? 1 : 0);
  });

  // Stats
  const totalArr = rows.reduce((s, r) => s + (+r.amount_raw || 0), 0);
  const acctSet  = new Set(rows.map(r => r.account));
  document.getElementById("renewal-stats").innerHTML = `
    <div class="stat-item"><div class="stat-label">Open Renewals</div><div class="stat-value">${rows.length}</div></div>
    <div class="stat-item"><div class="stat-label">Total ARR</div><div class="stat-value">${fmtUsd(totalArr)}</div></div>
    <div class="stat-item"><div class="stat-label">Accounts</div><div class="stat-value">${acctSet.size}</div></div>`;
  setCount("sub-count-renewals", rows.length);
  document.getElementById("count-badge").textContent = rows.length + " renewal" + (rows.length !== 1 ? "s" : "");

  // Region + CSM pills
  buildFlatPills("renewal-filters", "renewal");
  buildFlatCSMPills("renewal-csm-filters");

  // Table
  const cols = [
    { key: "account",       label: "Account" },
    { key: "name",          label: "Opportunity" },
    { key: "amount_raw",    label: "Amount" },
    { key: "end_date_raw",  label: "End Date" },
    { key: "churn_quarter", label: "Churn Quarter" },
    { key: "risk",          label: "Risk" },
    { key: "next_steps",    label: "Next Steps" },
    { key: "csm",           label: "CSM" },
    { key: "region",        label: "Region" }
  ];
  const headers = cols.map(c => {
    const cls = renewalSort.col === c.key ? (renewalSort.dir === 1 ? "sort-asc" : "sort-desc") : "";
    return `<th class="${cls}" onclick="sortRenewals(\'${c.key}\')">${c.label}</th>`;
  }).join("");

  const renewalRow = r => {
    const riskCls = r.risk.toLowerCase() === "high" ? "risk-high" :
                    r.risk.toLowerCase() === "moderate" ? "risk-moderate" :
                    r.risk.toLowerCase() === "low" ? "risk-low" : "";
    const cq = r.churn_quarter && r.churn_quarter !== "—"
      ? `<span class="churn-q">${esc(r.churn_quarter)}</span>` : esc(r.churn_quarter);
    return `<tr>
      <td><span class="acct-link" onclick="goToAccount(\'${esc(r.account)}\')">${esc(r.account)}</span></td>
      <td>${r.url ? \'<a class="opp-link" href="\' + esc(r.url) + \'" target="_blank" rel="noopener">\' + esc(r.name) + \'</a>\' : esc(r.name)}</td>
      <td>${esc(r.amount)}</td>
      <td>${esc(r.end_date)}</td>
      <td>${cq}</td>
      <td class="${riskCls}">${esc(r.risk)}</td>
      <td>${esc(r.next_steps)}</td>
      <td>${esc(r.csm)}</td>
      <td>${esc(r.region)}</td>
    </tr>`;
  };
  // Sorted by End Date: group under churn-quarter heading rows, each with a
  // count and ARR subtotal. Any other sort stays a flat list.
  const bodyRows = renewalSort.col === "end_date_raw"
    ? groupByChurnQuarter(rows, r => r.churn_quarter, r => r.end_date_raw, renewalSort.dir)
        .map(g => cqGroupRow(g, cols.length, "renewal", r => r.amount_raw) +
                  g.items.map(renewalRow).join("")).join("")
    : rows.map(renewalRow).join("");

  document.getElementById("renewal-table").innerHTML = rows.length === 0
    ? \'<p class="empty-msg" style="padding:24px">No renewals match the current filter.</p>\'
    : `<table class="flat-table"><thead><tr>${headers}</tr></thead><tbody>${bodyRows}</tbody></table>`;
}

// ── Expansions render ──────────────────────────────────────────────────────
function renderExpansions() {
  const q = searchTerm.toLowerCase();
  const region = activeRegion;

  let rows = ALL_EXPANSIONS.filter(e =>
    (region === "all" || e.region === region) &&
    (activeCSM === "all" || e.csm === activeCSM) &&
    (!q || e.account.toLowerCase().includes(q))
  );

  rows = rows.slice().sort((a, b) => {
    let av = a[expansionSort.col] || "", bv = b[expansionSort.col] || "";
    if (expansionSort.col === "amount_raw") { av = +av || 0; bv = +bv || 0; }
    return expansionSort.dir * (av < bv ? -1 : av > bv ? 1 : 0);
  });

  const totalArr = rows.reduce((s, e) => s + (+e.amount_raw || 0), 0);
  const acctSet  = new Set(rows.map(e => e.account));
  document.getElementById("expansion-stats").innerHTML = `
    <div class="stat-item"><div class="stat-label">Open Expansions</div><div class="stat-value">${rows.length}</div></div>
    <div class="stat-item"><div class="stat-label">Pipeline Value</div><div class="stat-value">${fmtUsd(totalArr)}</div></div>
    <div class="stat-item"><div class="stat-label">Accounts</div><div class="stat-value">${acctSet.size}</div></div>`;
  setCount("sub-count-expansions", rows.length);
  document.getElementById("count-badge").textContent = rows.length + " expansion" + (rows.length !== 1 ? "s" : "");

  buildFlatPills("expansion-filters", "expansion");
  buildFlatCSMPills("expansion-csm-filters");

  const cols = [
    { key: "account",        label: "Account" },
    { key: "name",           label: "Opportunity" },
    { key: "owner",          label: "Owner" },
    { key: "amount_raw",     label: "Amount" },
    { key: "fiscal_period",  label: "Fiscal Period" },
    { key: "close_date_raw", label: "Close Date" },
    { key: "next_steps",     label: "Next Steps" },
    { key: "region",         label: "Region" }
  ];
  const headers = cols.map(c => {
    const cls = expansionSort.col === c.key ? (expansionSort.dir === 1 ? "sort-asc" : "sort-desc") : "";
    return `<th class="${cls}" onclick="sortExpansions(\'${c.key}\')">${c.label}</th>`;
  }).join("");

  const expansionRow = e => `<tr>
    <td><span class="acct-link" onclick="goToAccount(\'${esc(e.account)}\')">${esc(e.account)}</span></td>
    <td>${e.url ? \'<a class="opp-link" href="\' + esc(e.url) + \'" target="_blank" rel="noopener">\' + esc(e.name) + \'</a>\' : esc(e.name)}</td>
    <td>${esc(e.owner)}</td>
    <td>${esc(e.amount)}</td>
    <td>${esc(e.fiscal_period)}</td>
    <td>${esc(e.close_date)}</td>
    <td>${esc(e.next_steps)}</td>
    <td>${esc(e.region)}</td>
  </tr>`;
  // Sorted by Close Date: group by the quarter the expansion closes in — its
  // Salesforce Fiscal Period, or the fiscal quarter of the close date when
  // that is blank. Same heading rows as Renewals.
  const bodyRows = expansionSort.col === "close_date_raw"
    ? groupByChurnQuarter(rows, e => expansionQuarter(e), e => e.close_date_raw, expansionSort.dir)
        .map(g => cqGroupRow(g, cols.length, "expansion", e => e.amount_raw) +
                  g.items.map(expansionRow).join("")).join("")
    : rows.map(expansionRow).join("");

  document.getElementById("expansion-table").innerHTML = rows.length === 0
    ? \'<p class="empty-msg" style="padding:24px">No expansions match the current filter.</p>\'
    : `<table class="flat-table"><thead><tr>${headers}</tr></thead><tbody>${bodyRows}</tbody></table>`;
}

// ── New logos render ───────────────────────────────────────────────────────
// Rows come from NEW_LOGOS_DATA (op_list$new_logos), not ACCOUNTS: these are
// prospects, so there is no account card to jump to and no CSM to filter on.
// Region and the search box apply; the CSM pills are not shown here.
function renderNewLogos() {
  const q = searchTerm.toLowerCase();
  const region = activeRegion;

  let rows = ALL_NEW_LOGOS.filter(n =>
    (region === "all" || n.region === region) &&
    (!q || String(n.account || "").toLowerCase().includes(q) ||
           String(n.name || "").toLowerCase().includes(q))
  );

  rows = rows.slice().sort((a, b) => {
    let av = a[newLogoSort.col] || "", bv = b[newLogoSort.col] || "";
    if (newLogoSort.col === "amount_raw") { av = +av || 0; bv = +bv || 0; }
    return newLogoSort.dir * (av < bv ? -1 : av > bv ? 1 : 0);
  });

  const total   = rows.reduce((s, n) => s + (+n.amount_raw || 0), 0);
  const acctSet = new Set(rows.map(n => n.account));
  document.getElementById("new-logo-stats").innerHTML = `
    <div class="stat-item"><div class="stat-label">Open New Logos</div><div class="stat-value">${rows.length}</div></div>
    <div class="stat-item"><div class="stat-label">Pipeline Value</div><div class="stat-value">${fmtUsd(total)}</div></div>
    <div class="stat-item"><div class="stat-label">Prospects</div><div class="stat-value">${acctSet.size}</div></div>`;
  setCount("sub-count-new_logos", rows.length);
  document.getElementById("count-badge").textContent = rows.length + " new logo" + (rows.length !== 1 ? "s" : "");

  buildFlatPills("new-logo-filters", "new_logo");

  const cols = [
    { key: "account",        label: "Account" },
    { key: "name",           label: "Opportunity" },
    { key: "owner",          label: "Owner" },
    { key: "amount_raw",     label: "Amount" },
    { key: "stage",          label: "Stage" },
    { key: "probability",    label: "Probability" },
    { key: "fiscal_period",  label: "Fiscal Period" },
    { key: "close_date_raw", label: "Close Date" },
    { key: "next_steps",     label: "Next Steps" },
    { key: "region",         label: "Region" }
  ];
  const headers = cols.map(c => {
    const cls = newLogoSort.col === c.key ? (newLogoSort.dir === 1 ? "sort-asc" : "sort-desc") : "";
    return `<th class="${cls}" onclick="sortNewLogos(\'${c.key}\')">${c.label}</th>`;
  }).join("");

  const newLogoRow = n => `<tr>
    <td>${esc(n.account)}</td>
    <td>${n.url && n.url !== "—" ? `<a class="opp-link" href="${esc(n.url)}" target="_blank" rel="noopener">${esc(n.name)}</a>` : esc(n.name)}</td>
    <td>${esc(n.owner)}</td>
    <td>${esc(n.amount)}</td>
    <td>${esc(n.stage)}</td>
    <td>${esc(n.probability)}</td>
    <td>${esc(n.fiscal_period)}</td>
    <td>${esc(n.close_date)}</td>
    <td>${esc(n.next_steps)}</td>
    <td>${esc(n.region)}</td>
  </tr>`;
  // Sorted by Close Date: grouped by fiscal quarter, same as Expansions.
  const bodyRows = newLogoSort.col === "close_date_raw"
    ? groupByChurnQuarter(rows, n => expansionQuarter(n), n => n.close_date_raw, newLogoSort.dir)
        .map(g => cqGroupRow(g, cols.length, "new logo", n => n.amount_raw) +
                  g.items.map(newLogoRow).join("")).join("")
    : rows.map(newLogoRow).join("");

  document.getElementById("new-logo-table").innerHTML = rows.length === 0
    ? (NEW_LOGOS_DATA && NEW_LOGOS_DATA.has_data
        ? \'<p class="empty-msg" style="padding:24px">No new logo opportunities match the current filter.</p>\'
        : \'<p class="empty-msg" style="padding:24px">No new logo data in this snapshot \\u2014 re-run the pipeline after adding New Business opportunities to the Salesforce report.</p>\')
    : `<table class="flat-table"><thead><tr>${headers}</tr></thead><tbody>${bodyRows}</tbody></table>`;
}

// ── Sort handlers ──────────────────────────────────────────────────────────
function sortNewLogos(col) {
  newLogoSort.dir = newLogoSort.col === col ? -newLogoSort.dir : 1;
  newLogoSort.col = col;
  renderNewLogos();
}
function sortRenewals(col) {
  renewalSort.dir = renewalSort.col === col ? -renewalSort.dir : 1;
  renewalSort.col = col;
  renderRenewals();
}
function sortExpansions(col) {
  expansionSort.dir = expansionSort.col === col ? -expansionSort.dir : 1;
  expansionSort.col = col;
  renderExpansions();
}

// ── Navigate from table row to account card ────────────────────────────────
function goToAccount(name) {
  // Accounts is a sub-tab now, so set both: the page, then the sub-tab within
  // it. Order matters — switchSub() writes into whichever page is active.
  PAGES.engagement.active = "accounts";
  switchPage("engagement");
  searchTerm = name;
  document.getElementById("search").value = name;
  render();
  setTimeout(() => {
    // Scoped to the Accounts list: the Coverage and Agentic Engagement Overview tabs keep
    // their own .card elements in the DOM.
    const cards = document.querySelectorAll("#account-list .card");
    if (cards.length === 1) { cards[0].classList.add("open"); cards[0].scrollIntoView({ behavior: "smooth", block: "center" }); }
  }, 50);
}

// ── Region pills for flat pages ────────────────────────────────────────────
function buildFlatPills(containerId, pageKey) {
  const allRegions = [...new Set(ACCOUNTS.map(a => a.region).filter(Boolean))].sort();
  const container  = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = [
    `<button class="flat-pill${activeRegion === "all" ? " active" : ""}" onclick="setRegion(\'all\')">All Regions</button>`
  ].concat(allRegions.map(r =>
    `<button class="flat-pill${activeRegion === r ? " active" : ""}" onclick="setRegion(\'${esc(r)}\')">${esc(r)}</button>`
  )).join("");
}

function buildFlatCSMPills(containerId) {
  const allCSMs = [...new Set(ACCOUNTS.map(a => a.csm).filter(Boolean))].sort();
  const container = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = [
    `<button class="flat-pill${activeCSM === "all" ? " active" : ""}" onclick="setCSM(\'all\')">All CSMs</button>`
  ].concat(allCSMs.map(c =>
    `<button class="flat-pill${activeCSM === c ? " active" : ""}" onclick="setCSM(\'${esc(c)}\')">${esc(c)}</button>`
  )).join("");
}

function setRegion(r) {
  activeRegion = r;
  document.querySelectorAll("[data-region]").forEach(b => b.classList.toggle("active", b.dataset.region === r));
  render();
}

function setCSM(c) {
  activeCSM = c;
  document.querySelectorAll("[data-csm]").forEach(b => b.classList.toggle("active", b.dataset.csm === c));
  render();
}

// ── Alpha jump bar ─────────────────────────────────────────────────────────
function buildAlphaJump(visibleLetters) {
  const label = document.getElementById("jump-label");
  if (label) label.textContent = "Jump to Letter";
  const container = document.getElementById("alpha-jump");
  const all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".split("");
  container.innerHTML = all.map(l =>
    visibleLetters.includes(l)
      ? `<a href="#letter-${l}" class="has-accounts">${l}</a>`
      : `<a class="disabled">${l}</a>`
  ).join("");
}

// ── Card toggle ────────────────────────────────────────────────────────────
function toggleCard(header) { header.parentElement.classList.toggle("open"); }

// ── Card tab switch ────────────────────────────────────────────────────────
function switchTab(btn, paneId) {
  btn.closest(".tabs").querySelectorAll(".tab-btn").forEach(b => b.classList.remove("active"));
  btn.classList.add("active");
  btn.closest(".card-body").querySelectorAll(".tab-pane").forEach(p => p.classList.remove("active"));
  document.getElementById(paneId).classList.add("active");
}

// ── USD formatter ──────────────────────────────────────────────────────────
function fmtUsd(n) {
  if (!n || isNaN(n)) return "—";
  return "$" + Number(n).toLocaleString("en-US", { maximumFractionDigits: 0 });
}

// ── Sidebar pill listeners ─────────────────────────────────────────────────
document.querySelectorAll("[data-region]").forEach(btn => {
  btn.addEventListener("click", () => { setRegion(btn.dataset.region); });
});
document.querySelectorAll("[data-acctsort]").forEach(btn => {
  btn.addEventListener("click", () => { setAccountSort(btn.dataset.acctsort); });
});
document.querySelectorAll("[data-csm]").forEach(btn => {
  btn.addEventListener("click", () => { setCSM(btn.dataset.csm); });
});

document.getElementById("search").addEventListener("input", e => {
  searchTerm = e.target.value;
  render();
});

// ── Changes rendering ──────────────────────────────────────────────────────

function renderChanges() {
  const hdr  = document.getElementById("changes-header");
  const body = document.getElementById("changes-content");

  if (!CHANGES_DATA || !CHANGES_DATA.has_data) {
    hdr.innerHTML  = "";
    body.innerHTML = `<p class="empty-msg" style="padding:40px">No changes data — run enrich_list_object() to generate a diff.</p>`;
    return;
  }

  const sum = CHANGES_DATA.summary || [];
  hdr.innerHTML = `
    <div>
      <div class="changes-date-label">Comparing to</div>
      <div class="changes-date-val">${esc(CHANGES_DATA.diff_date)}</div>
    </div>
    <div style="width:1px;height:36px;background:var(--border);flex-shrink:0"></div>
    ${sum.map(r => `
      <div class="stat-item">
        <div class="stat-label">${esc(r.Component || "")}</div>
        <div class="stat-value" style="font-size:16px">${r.Total_Moves || 0}</div>
      </div>`).join("")}`;

  body.innerHTML = [
    buildChangeSection("accounts",      "Accounts",        CHANGES_DATA.accounts,      renderAccountChanges),
    buildChangeSection("renewals",      "Renewals",        CHANGES_DATA.renewals,      renderRenewalChanges),
    buildChangeSection("expansions",    "Expansions",      CHANGES_DATA.expansions,    renderExpansionChanges),
    buildChangeSection("new_logos",     "New Logos",       CHANGES_DATA.new_logos,     d => renderExpansionChanges(d, false)),
    buildChangeSection("forecast",      "Churn Forecast",  CHANGES_DATA.forecast,      renderForecastChanges),
    buildChangeSection("csm_table",     "CSM Assignments", CHANGES_DATA.csm_table,     renderCSMChanges),
    buildChangeSection("saas_pipeline", "SaaS Pipeline",   CHANGES_DATA.saas_pipeline, renderSaaSChanges),
    buildChangeSection("escalations",   "Escalations",     CHANGES_DATA.escalations,   renderEscalationChanges),
    // Same table as the Agentic Engagement Overview\'s "What changed" card (agxChangesHtml()).
    buildChangeSection("agentic",       "Agentic Engagement", CHANGES_DATA.agentic,    d => agxChangesHtml(d))
  ].join("");
}

function buildChangeSection(key, label, data, tableFn) {
  if (!data) return "";
  const nAdded   = (data.added   || []).length;
  const nRemoved = (data.removed || []).length;
  const nChanged = (data.field_changes || data.reassignments || []).length;
  const total    = nAdded + nRemoved + nChanged;

  const badges = [
    nAdded   > 0 ? `<span class="chg-badge chg-added">+${nAdded}</span>`       : "",
    nRemoved > 0 ? `<span class="chg-badge chg-removed">&#8722;${nRemoved}</span>` : "",
    nChanged > 0 ? `<span class="chg-badge chg-changed">&#126;${nChanged}</span>`  : ""
  ].join(" ");

  const bodyContent = total === 0
    ? `<p class="empty-msg" style="padding:14px 18px">No changes detected.</p>`
    : tableFn(data);

  return `
    <div class="change-section">
      <div class="change-section-hdr" onclick="toggleChgSection(\'${key}\')">
        <span class="change-section-title">${esc(label)}</span>
        ${badges}
        <span class="change-section-chevron" id="chsec-chev-${key}">&#9660;</span>
      </div>
      <div class="change-section-body" id="chsec-body-${key}">${bodyContent}</div>
    </div>`;
}

function toggleChgSection(key) {
  const body = document.getElementById("chsec-body-" + key);
  const chev = document.getElementById("chsec-chev-" + key);
  const isOpen = body.style.display !== "none";
  body.style.display = isOpen ? "none" : "";
  chev.innerHTML     = isOpen ? "&#9654;" : "&#9660;";
}

// chgArrow: renders old → new or just the value if unchanged
function chgArrow(oldVal, newVal) {
  const o = oldVal == null ? "" : String(oldVal);
  const n = newVal == null ? "" : String(newVal);
  if (!o && !n) return `<span class="chg-null">—</span>`;
  if (o === n)  return esc(o);
  return `<span class="chg-old">${esc(o) || "<em>empty</em>"}</span> &#8594; <span class="chg-new">${esc(n) || "<em>empty</em>"}</span>`;
}

// Generic simple table for added / removed rows
function buildSimpleTable(groupLabel, rows, cols, badgeCls) {
  if (!rows || rows.length === 0) return "";
  const hdrs = cols.map(c => `<th>${c.label}</th>`).join("");
  const body = rows.map(r =>
    `<tr>${cols.map(c => `<td>${esc(String(r[c.key] || ""))}</td>`).join("")}</tr>`
  ).join("");
  return `
    <div class="chg-group">
      <div class="chg-group-title"><span class="chg-badge ${badgeCls}">${groupLabel} (${rows.length})</span></div>
      <table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${body}</tbody></table>
    </div>`;
}

function renderAccountChanges(data) {
  const cols = [
    { key: "Account Name",             label: "Account" },
    { key: "Region",                   label: "Region" },
    { key: "Contract ARR Rollup",      label: "ARR" },
    { key: "Customer Success Manager", label: "CSM" }
  ];
  let html = "";
  html += buildSimpleTable("+ Added",         data.added,   cols, "chg-added");
  html += buildSimpleTable("&#8722; Removed", data.removed, cols, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const watchedPairs = [
      ["Contract ARR Rollup",      "ARR"],
      ["Customer Success Manager", "CSM"],
      ["Account Owner",            "Owner"],
      ["Field CTO",                "Field CTO"],
      ["Region",                   "Region"],
      ["Adoption Phase",           "Adoption"],
      ["request_adoption_looker_stage", "Request stage"],
      ["comply_adoption_looker_stage",  "Comply stage"]
    ].filter(([f]) => fc.some(r => {
      const n = r[f + ".new"], o = r[f + ".old"];
      return (n || o) && n !== o;
    }));

    const hdrs = `<th>Account</th>${watchedPairs.map(([, l]) => `<th>${l}</th>`).join("")}`;
    const rows = fc.map(r => {
      const nm = r["Account Name"] || "";
      const cells = watchedPairs.map(([f]) =>
        `<td>${chgArrow(r[f + ".old"], r[f + ".new"])}</td>`
      ).join("");
      return `<tr>
        <td><span class="acct-link" onclick="goToAccount(\'${esc(nm)}\')">${esc(nm)}</span></td>
        ${cells}
      </tr>`;
    }).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Changed (${fc.length})</span></div>
        <table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${rows}</tbody></table>
      </div>`;
  }
  return html;
}

function renderRenewalChanges(data) {
  const addCols = [
    { key: "Opportunity Name", label: "Opportunity" },
    { key: "Account Name",     label: "Account" },
    { key: "Amount",           label: "Amount" },
    { key: "Close Date",       label: "Close Date" },
    { key: "Account Risk",     label: "Risk" }
  ];
  let html = "";
  html += buildSimpleTable("+ Added",         data.added,   addCols, "chg-added");
  html += buildSimpleTable("&#8722; Removed", data.removed, addCols, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const na = `<td class="chg-null">—</td>`;
    const rows = fc.map(r => {
      const acct = r["Account Name"] || "";
      return `<tr>
        <td>${esc(r["Opportunity Name"] || "")}</td>
        <td><span class="acct-link" onclick="goToAccount(\'${esc(acct)}\')">${esc(acct)}</span></td>
        ${r.risk_changed     ? `<td>${chgArrow(r["Account Risk.old"],   r["Account Risk.new"])}</td>`   : na}
        ${r.forecast_changed ? `<td>${chgArrow(r["Churn Forecast.old"], r["Churn Forecast.new"])}</td>` : na}
        ${r.amount_changed   ? `<td>${chgArrow(r["Amount.old"],         r["Amount.new"])}</td>`         : na}
        ${r.date_changed     ? `<td>${chgArrow(r["Close Date.old"],     r["Close Date.new"])}</td>`     : na}
      </tr>`;
    }).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Changed (${fc.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>Opportunity</th><th>Account</th><th>Risk</th><th>Forecast</th><th>Amount</th><th>Date</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

// linkAccounts = false for new logos: their accounts are prospects with no card
// on this page, so the name is shown as plain text rather than a dead link.
function renderExpansionChanges(data, linkAccounts = true) {
  const addCols = [
    { key: "Opportunity Name",  label: "Opportunity" },
    { key: "Account Name",      label: "Account" },
    { key: "Amount",            label: "Amount" },
    { key: "Fiscal Period",     label: "Period" },
    { key: "Opportunity Owner", label: "Owner" }
  ];
  let html = "";
  html += buildSimpleTable("+ Added",         data.added,   addCols, "chg-added");
  html += buildSimpleTable("&#8722; Removed", data.removed, addCols, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const na = `<td class="chg-null">—</td>`;
    const rows = fc.map(r => {
      const acct = r["Account Name"] || "";
      return `<tr>
        <td>${esc(r["Opportunity Name"] || "")}</td>
        <td>${linkAccounts ? `<span class="acct-link" onclick="goToAccount(\'${esc(acct)}\')">${esc(acct)}</span>` : esc(acct)}</td>
        ${r.amount_changed ? `<td>${chgArrow(r["Amount.old"],        r["Amount.new"])}</td>`        : na}
        ${r.period_changed ? `<td>${chgArrow(r["Fiscal Period.old"], r["Fiscal Period.new"])}</td>` : na}
      </tr>`;
    }).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Changed (${fc.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>Opportunity</th><th>Account</th><th>Amount</th><th>Fiscal Period</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

// Renewal churn forecast (op_list$forecast / changes_detail$forecast).
// Keyed on Opportunity ID, so "added"/"removed" mean an opportunity entered or
// left the forecast report between the two snapshots — usually a churn quarter
// moving in or out of the report filter, not a deal being created or deleted.
function renderForecastChanges(data) {
  const addCols = [
    { key: "Opportunity Name", label: "Opportunity" },
    { key: "Account Name",     label: "Account" },
    { key: "Churn Quarter",    label: "Churn Qtr" },
    { key: "Churn Forecast",   label: "Forecast" },
    { key: "Amount",           label: "Amount" },
    { key: "Account Risk",     label: "Risk" },
    { key: "Stage",            label: "Stage" }
  ];
  let html = "";
  html += buildSimpleTable("+ Added",         data.added,   addCols, "chg-added");
  html += buildSimpleTable("&#8722; Removed", data.removed, addCols, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    // 0 is a real forecast, not a missing one, so don\'t reuse fmtUsd() here.
    const usd = v => {
      const n = parseFloat(v);
      return isNaN(n) ? "" : "$" + n.toLocaleString("en-US", { maximumFractionDigits: 0 });
    };

    // Net movement in forecast churn dollars across every changed row.
    const net = fc.reduce((s, r) => s + (parseFloat(r.forecast_delta) || 0), 0);
    const netCls  = net > 0 ? "pct-low" : net < 0 ? "pct-high" : "pct-none";
    const netTxt  = (net > 0 ? "+" : net < 0 ? "&#8722;" : "") +
                    "$" + Math.abs(Math.round(net)).toLocaleString("en-US");

    const na = `<td class="chg-null">—</td>`;
    const rows = fc.map(r => {
      const acct  = r["Account Name"] || "";
      const delta = parseFloat(r.forecast_delta);
      const dTxt  = isNaN(delta) || delta === 0 ? "" :
        ` <span class="${delta > 0 ? "pct-low" : "pct-high"}">(${delta > 0 ? "+" : "&#8722;"}${usd(Math.abs(delta))})</span>`;
      const acctCell = acct
        ? `<span class="acct-link" onclick="goToAccount(\'${esc(acct)}\')">${esc(acct)}</span>`
        : `<span class="chg-null">—</span>`;
      return `<tr>
        <td>${esc(r["Opportunity Name"] || "")}</td>
        <td>${acctCell}</td>
        ${r.quarter_changed  ? `<td>${chgArrow(r["Churn Quarter.old"], r["Churn Quarter.new"])}</td>` : na}
        ${r.forecast_changed ? `<td>${chgArrow(usd(r["Churn Forecast.old"]), usd(r["Churn Forecast.new"]))}${dTxt}</td>` : na}
        ${r.risk_changed     ? `<td>${chgArrow(r["Account Risk.old"], r["Account Risk.new"])}</td>` : na}
        ${r.amount_changed   ? `<td>${chgArrow(usd(r["Amount.old"]), usd(r["Amount.new"]))}</td>` : na}
        ${r.stage_changed    ? `<td>${chgArrow(r["Stage.old"], r["Stage.new"])}</td>` : na}
        ${r.date_changed     ? `<td>${chgArrow(r["Close Date.old"], r["Close Date.new"])}</td>` : na}
      </tr>`;
    }).join("");

    html += `
      <div class="chg-group">
        <div class="chg-group-title">
          <span class="chg-badge chg-changed">&#126; Changed (${fc.length})</span>
          <span class="${netCls}" style="margin-left:10px;font-weight:600">Net forecast churn ${netTxt}</span>
        </div>
        <table class="flat-table">
          <thead><tr><th>Opportunity</th><th>Account</th><th>Churn Qtr</th><th>Forecast</th><th>Risk</th><th>Amount</th><th>Stage</th><th>Close Date</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

function renderCSMChanges(data) {
  let html = "";

  function dynTable(label, rows, badgeCls) {
    if (!rows || rows.length === 0) return "";
    const keys = Object.keys(rows[0] || {});
    if (!keys.length) return "";
    const hdrs = keys.map(k => `<th>${esc(k)}</th>`).join("");
    const body = rows.map(r =>
      `<tr>${keys.map(k => `<td>${esc(String(r[k] || ""))}</td>`).join("")}</tr>`
    ).join("");
    return `<div class="chg-group"><div class="chg-group-title"><span class="chg-badge ${badgeCls}">${label} (${rows.length})</span></div><table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${body}</tbody></table></div>`;
  }

  html += dynTable("+ Added",         data.added,   "chg-added");
  html += dynTable("&#8722; Removed", data.removed, "chg-removed");

  const rs = data.reassignments || [];
  if (rs.length > 0) {
    const keys   = Object.keys(rs[0] || {});
    const keyCol = keys[0] || "";
    const newCol = keys.find(k => k.endsWith(".new")) || "";
    const oldCol = keys.find(k => k.endsWith(".old")) || "";
    const fieldLabel = newCol.replace(".new", "") || "CSM";
    const rows = rs.map(r =>
      `<tr><td>${esc(r[keyCol] || "")}</td><td>${chgArrow(r[oldCol], r[newCol])}</td></tr>`
    ).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Reassignments (${rs.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>${esc(keyCol)}</th><th>${esc(fieldLabel)}</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

function renderSaaSChanges(data) {
  let html = "";

  function dynTable(label, rows, badgeCls) {
    if (!rows || rows.length === 0) return "";
    const keys = Object.keys(rows[0] || {});
    if (!keys.length) return "";
    const hdrs = keys.map(k => `<th>${esc(k)}</th>`).join("");
    const body = rows.map(r =>
      `<tr>${keys.map(k => `<td>${esc(String(r[k] || ""))}</td>`).join("")}</tr>`
    ).join("");
    return `<div class="chg-group"><div class="chg-group-title"><span class="chg-badge ${badgeCls}">${label} (${rows.length})</span></div><table class="flat-table"><thead><tr>${hdrs}</tr></thead><tbody>${body}</tbody></table></div>`;
  }

  html += dynTable("+ Added",         data.added,   "chg-added");
  html += dynTable("&#8722; Removed", data.removed, "chg-removed");

  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const keys   = Object.keys(fc[0] || {});
    const newCol = keys.find(k => k.endsWith(".new")) || "";
    const oldCol = keys.find(k => k.endsWith(".old")) || "";
    const keyCol = keys.find(k => !k.endsWith(".new") && !k.endsWith(".old")) || keys[0] || "";
    if (newCol && oldCol) {
      const fieldLabel = newCol.replace(".new", "");
      const rows = fc.map(r =>
        `<tr><td>${esc(r[keyCol] || "")}</td><td>${chgArrow(r[oldCol], r[newCol])}</td></tr>`
      ).join("");
      html += `
        <div class="chg-group">
          <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Stage Changes (${fc.length})</span></div>
          <table class="flat-table">
            <thead><tr><th>${esc(keyCol)}</th><th>${esc(fieldLabel)}</th></tr></thead>
            <tbody>${rows}</tbody>
          </table>
        </div>`;
    } else {
      html += dynTable("&#126; Changed", fc, "chg-changed");
    }
  }
  return html;
}

// Escalation changes (changes_detail$escalations), keyed on the Jira issue key.
// "Added" is an escalation raised since the last snapshot, "removed" is one that
// closed — a close is good news and is worth reading as prominently as a raise,
// so both get the issue key as a link out rather than bare text.
function renderEscalationChanges(data) {
  let html = "";

  const escTable = (label, rows, badgeCls) => {
    if (!rows || rows.length === 0) return "";
    const body = rows.map(r => `
      <tr>
        <td><a href="${esc(r.url || "#")}" target="_blank" rel="noopener">${esc(r.key || "")}</a></td>
        <td>${esc(r.severity || "\\u2014")}</td>
        <td>${r["Account Name"]
              ? `<span class="acct-link" onclick="goToAccount(\'${esc(r["Account Name"])}\')">${esc(r["Account Name"])}</span>`
              : `<span class="esc-unmatched">no account matched</span>`}</td>
        <td>${esc(r.status || "\\u2014")}</td>
        <td>${r.age_days != null ? r.age_days + "d" : "\\u2014"}</td>
        <td>${esc(r.summary || "")}</td>
      </tr>`).join("");
    return `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge ${badgeCls}">${label} (${rows.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>Issue</th><th>Severity</th><th>Account</th><th>Status</th><th>Age</th><th>Summary</th></tr></thead>
          <tbody>${body}</tbody>
        </table>
      </div>`;
  };

  html += escTable("+ Raised",  data.added,   "chg-added");
  html += escTable("&#8722; Closed", data.removed, "chg-removed");

  // Severity and status moves on issues open in both runs. Both columns render
  // even when only one moved — chgArrow() collapses an unchanged pair to the
  // plain value, so an empty cell would be ambiguous where this is not.
  const fc = data.field_changes || [];
  if (fc.length > 0) {
    const rows = fc.map(r => `
      <tr>
        <td>${esc(r.key || "")}</td>
        <td>${r["Account Name"] ? esc(r["Account Name"]) : `<span class="esc-unmatched">\\u2014</span>`}</td>
        <td>${chgArrow(r["severity.old"], r["severity.new"])}</td>
        <td>${chgArrow(r["status.old"],   r["status.new"])}</td>
      </tr>`).join("");
    html += `
      <div class="chg-group">
        <div class="chg-group-title"><span class="chg-badge chg-changed">&#126; Severity / Status (${fc.length})</span></div>
        <table class="flat-table">
          <thead><tr><th>Issue</th><th>Account</th><th>Severity</th><th>Status</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>`;
  }
  return html;
}

// ── Diff-date picker ───────────────────────────────────────────────────────
function populateSnapshots(dates) {
  const sel = document.getElementById("diff-date");
  if (!dates || dates.length === 0) {
    sel.innerHTML = \'<option value="">No snapshots found</option>\';
    return;
  }
  sel.innerHTML = dates.map((d, i) =>
    `<option value="${d}"${i === 0 ? " selected" : ""}>${d}</option>`
  ).join("");
}

async function initSnapshots() {
  try {
    const res = await fetch("http://localhost:8765/snapshots", { signal: AbortSignal.timeout(2000) });
    const data = await res.json();
    if (data.dates && data.dates.length > 0) populateSnapshots(data.dates);
    else populateSnapshots(SNAP_DATES);
  } catch(e) {
    populateSnapshots(SNAP_DATES);   // server not running — use dates baked into the HTML
  }
}
initSnapshots();

// ── Refresh ────────────────────────────────────────────────────────────────
async function triggerRefresh() {
  const btn      = document.getElementById("refresh-btn");
  const diffDate = document.getElementById("diff-date").value;
  const orig     = btn.innerHTML;
  btn.innerHTML  = "Refreshing…";
  btn.disabled   = true;
  try {
    await fetch("http://localhost:8765/status", { signal: AbortSignal.timeout(3000) });
    const url = "http://localhost:8765/refresh" + (diffDate ? "?diff_date=" + encodeURIComponent(diffDate) : "");
    const res  = await fetch(url, { signal: AbortSignal.timeout(300000) });
    const data = await res.json();
    if (data.status === "error") throw new Error(data.message);
    btn.innerHTML = "✓ Done — reloading…";
    setTimeout(() => location.reload(), 1800);
  } catch(e) {
    btn.disabled  = false;
    btn.innerHTML = orig;
    document.getElementById("refresh-modal").style.display = "flex";
  }
}

function closeRefreshModal() {
  document.getElementById("refresh-modal").style.display = "none";
}

function copyModalCmd(el) {
  navigator.clipboard.writeText(el.textContent.trim()).then(() => {
    const orig = el.textContent;
    el.textContent = "Copied!";
    setTimeout(() => { el.textContent = orig; }, 1500);
  });
}

// ── Init ───────────────────────────────────────────────────────────────────
render();
</script>
</body>
</html>'

  # Agentic Engagement Overview: shared with report_agentic_owner_html()
  # (R/agentic_overview_assets.R). Literal fill, before the data placeholders.
  tmpl <- .ao_fill(tmpl, "{{AGX_CSS}}",   .AGX_CSS)
  tmpl <- .ao_fill(tmpl, "{{AGX_PANEL}}", .AGX_PANEL)
  tmpl <- .ao_fill(tmpl, "{{AGX_JS}}",    .AGX_JS)
  tmpl <- gsub("{{FAVICON}}",     .rah_favicon_tag(favicon), tmpl, fixed = TRUE)
  tmpl <- gsub("{{UPDATED}}",     html_escape(updated),  tmpl, fixed = TRUE)
  tmpl <- gsub("{{REGION_PILLS}}", region_pills,          tmpl, fixed = TRUE)
  tmpl <- gsub("{{CSM_PILLS}}",   csm_pills,             tmpl, fixed = TRUE)
  tmpl <- gsub("{{SM_LIFECYCLE_PILLS}}", sm_lifecycle_pills, tmpl, fixed = TRUE)
  tmpl <- gsub("{{SNAP_DATES}}",  .to_json(snap_dates),  tmpl, fixed = TRUE)
  tmpl <- gsub("{{CHANGES}}",      changes_json,          tmpl, fixed = TRUE)
  tmpl <- gsub("{{CONSUMPTION}}",  consumption_json,      tmpl, fixed = TRUE)
  tmpl <- gsub("{{SAAS}}",         saas_json,             tmpl, fixed = TRUE)
  tmpl <- gsub("{{ESCALATIONS}}",  escalations_json,      tmpl, fixed = TRUE)
  tmpl <- gsub("{{NEW_LOGOS}}",    new_logos_json,        tmpl, fixed = TRUE)
  tmpl <- gsub("{{AGENTIC}}",      agentic_json,          tmpl, fixed = TRUE)
  tmpl <- gsub("{{GUIDE_B64}}",    guide_b64,             tmpl, fixed = TRUE)
  tmpl <- gsub("{{DOCTRINE_B64}}", doctrine_b64,          tmpl, fixed = TRUE)
  tmpl <- gsub("{{REPORTING_B64}}", reporting_b64,        tmpl, fixed = TRUE)
  tmpl <- gsub("{{JSON_DATA}}",    json_data,             tmpl, fixed = TRUE)
  tmpl
}


# ---- Internal: favicon ------------------------------------------------------

# A local copy wins over the website's, since an embedded icon also shows when
# the page is opened offline or from a downloaded copy.
.rah_default_favicon <- function() {
  local <- file.path(c("inst/assets", "assets", "output/assets"),
                     rep(c("immuta_favicon.svg", "immuta_favicon.png",
                           "immuta_favicon.ico"), each = 3))
  hit <- local[file.exists(local)]
  if (length(hit)) hit[1] else "https://www.immuta.com/favicon.ico"
}

# <link rel="icon"> for a local file (inlined as a data: URI) or a URL.
.rah_favicon_tag <- function(favicon) {
  if (is.null(favicon) || length(favicon) == 0 || is.na(favicon[1]) ||
      !nzchar(favicon[1])) return("")
  favicon <- favicon[1]

  if (grepl("^(https?:)?//|^data:", favicon)) {
    return(paste0('<link rel="icon" href="', html_escape(favicon), '">'))
  }

  if (!file.exists(favicon)) {
    warning("Favicon file not found: ", favicon, " - page has no favicon.",
            call. = FALSE)
    return("")
  }

  ext  <- tolower(sub(".*\\.([^.]+)$", "\\1", favicon))
  mime <- switch(ext,
    ico  = "image/x-icon",
    png  = "image/png",
    svg  = "image/svg+xml",
    gif  = "image/gif",
    jpg  = , jpeg = "image/jpeg",
    webp = "image/webp",
    "application/octet-stream")
  raw <- readBin(favicon, "raw", file.info(favicon)$size)
  paste0('<link rel="icon" type="', mime, '" href="data:', mime, ';base64,',
         jsonlite::base64_enc(raw), '">')
}
