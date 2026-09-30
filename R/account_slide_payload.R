#' Build the slide payload for a single account.
#'
#' Collects everything a regional-review slide needs — the header metadata that
#' \code{\link{generate_slide_header}} renders, plus the Adoption, Performance,
#' and Engagement blocks that \code{\link{generate_slide_tile}} renders — into a
#' plain nested list.
#'
#' This is the single source of truth for "what goes on an account slide". Both
#' \code{\link{output_slides_regional_review}} and any future JSON export (a
#' Dust push, say) consume it, so the two can never drift apart. The return
#' value is deliberately made of plain lists and character scalars, so
#' \code{jsonlite::toJSON(auto_unbox = TRUE)} produces clean output.
#'
#' Every value is pre-formatted for display: missing data is an em-dash, not
#' \code{NA}, matching the convention in \code{R/utils_internal.R}.
#'
#' @param account_name Character. Exact Salesforce account name.
#' @param op_list Named list returned by \code{\link{enrich_opportunities}}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{\code{account}}{name, region, arr, ae, csm, fcto, next_renewal,
#'     account_url, customer_central_url.}
#'   \item{\code{adoption}}{modules (a list of label/status/colour/source
#'     entries — PBAC and Request rated from Looker, Comply and Agentic from
#'     the Salesforce picklists, with \code{source} naming which), platforms,
#'     platforms_dev, features (always an array), features_text, registered,
#'     licensed, ratio, lifecycle_stage.}
#'   \item{\code{performance}}{cases_90d, cases_360d, cases_total,
#'     escalations_90d, escalations_360d, escalations_open, deployment
#'     (the SaaS / SM: Supported / SM: Unsupported badge text),
#'     deployment_color, version.}
#'   \item{\code{engagement}}{contacts (Executive Buyer then Champion, each
#'     label/name/relationship/influence plus a dot colour for the two
#'     ratings), last_qbr, next_qbr, qbr_note.}
#' }
#'
#' @examples
#' \dontrun{
#'   p <- account_slide_payload("Acorns", op_list)
#'   jsonlite::toJSON(p, auto_unbox = TRUE, pretty = TRUE)
#' }
#' @export
account_slide_payload <- function(account_name, op_list) {

  a_data <- dplyr::filter(op_list$accounts, `Account Name` == account_name)
  if (nrow(a_data) == 0) {
    stop("Account '", account_name, "' not found in op_list$accounts.",
         call. = FALSE)
  }
  a_data <- a_data[1, ]

  c_data <- .tile_row(op_list$consumption$low_consumption, account_name)
  s_data <- .tile_row(op_list$support, account_name)

  # ── Header ─────────────────────────────────────────────────────────────────
  arr_raw <- suppressWarnings(as.numeric(a_data[["Contract ARR Rollup"]]))
  arr_fmt <- if (!is.na(arr_raw) && arr_raw >= 1e6) {
    scales::dollar(arr_raw, scale = 1e-6, suffix = "M", accuracy = 0.01)
  } else if (!is.na(arr_raw)) {
    scales::dollar(arr_raw, accuracy = 1000)
  } else "—"

  next_ren <- op_list$renewals |>
    dplyr::filter(`Account Name` == account_name) |>
    dplyr::arrange(`Close Date`) |>
    dplyr::slice(1)

  next_renewal <- if (nrow(next_ren) > 0) {
    amt <- suppressWarnings(as.numeric(next_ren[["Amount"]][1]))
    amt_fmt <- if (is.na(amt)) "—" else scales::dollar(amt, accuracy = 1000)

    q <- .coalesce_chr(next_ren[["churn_forecast_period"]][1], NA_character_)
    if (is.na(q) || identical(q, "—")) {
      cd <- suppressWarnings(as.Date(next_ren[["Close Date"]][1]))
      q  <- if (!is.na(cd)) date_to_fiscal_quarter(cd + 1) else "—"
    }
    paste0(amt_fmt, ", ", q)
  } else "No open renewal"

  # ── Adoption ───────────────────────────────────────────────────────────────
  # PBAC and Request are rated from Looker, Comply and Agentic from the
  # Salesforce picklists — adoption_rows() is shared with
  # generate_slide_tile() so the PNG and this feed always agree, including on
  # the fallback when a run had no Looker data.
  #
  # `status` keeps its contract as the text that goes beside the dot; for the
  # picklists that is still the colour word, for the derived rows it is what
  # the value says ("Retained", "Moderate adoption"). `source` is new, so a
  # consumer can tell which of the two a row came from.
  p_data <- pbac_row(op_list, account_name)

  modules <- lapply(adoption_rows(a_data, p_data), function(m) {
    list(label  = m$label,
         status = m$word,
         colour = .tile_stoplight_colour(m$value),
         source = m$source)
  })

  # Evidence behind the PBAC rating. `features` is always an array so a
  # consumer can iterate it; `features_text` is the display string, where
  # "None" means Looker measured nothing in use and an em-dash means nothing
  # was measured.
  pbac_feats <- pbac_feature_labels(p_data)

  features_text <- if (is.null(p_data)) {
    "—"
  } else if (length(pbac_feats) == 0) {
    "None"
  } else {
    paste(pbac_feats, collapse = " · ")
  }

  reg_raw <- .tile_num(c_data, "Registered Users")
  lic_raw <- .tile_num(c_data, "Licensed Users")
  rat_raw <- .tile_num(c_data, "Consumption Ratio")
  # ELA rule lives in consumption_ratio(): Licensed Users of 0 is unlimited
  # seats, so the ratio is 1 rather than NA.
  if (is.na(rat_raw)) rat_raw <- consumption_ratio(reg_raw, lic_raw)

  # ── Performance ────────────────────────────────────────────────────────────

  is_saas <- isTRUE(a_data[["Is SaaS Customer"]] %in%
                      c(TRUE, "Yes", "TRUE", "true", "yes"))
  unsup   <- isTRUE(a_data[["Customer on Unsupported Version"]] %in%
                      c(TRUE, "Yes", "TRUE", "true", "yes"))

  # Same badge wording/colours as the dashboard tags and the PNG tile.
  dep_tag <- .tile_deploy_tag(is_saas, unsup)

  ids <- .slide_ids(.slide_uid(account_name))

  list(
    # Deterministic Google Slides object IDs. Emitted in the payload so any
    # consumer — the R generator, a Dust agent, anything else — mutates the
    # same shapes rather than inventing its own. Everything under `comment` is
    # CSM-owned: create it once, never write to it again.
    slide_ids = list(
      slide           = ids$slide,
      title           = ids$title,
      arr             = ids$arr,
      meta            = ids$meta,
      header          = as.list(ids$header),
      metrics         = as.list(ids$metrics),
      comment         = as.list(ids$comment),
      writable        = as.list(c(ids$title, ids$arr, ids$meta,
                                  ids$header, ids$metrics)),
      never_overwrite = as.list(ids$comment)
    ),
    account = list(
      name                 = as.character(account_name),
      region               = .coalesce_chr(a_data[["Region"]], "—"),
      arr                  = arr_fmt,
      ae                   = .coalesce_chr(a_data[["Account Owner"]], "—"),
      csm                  = .coalesce_chr(a_data[["Customer Success Manager"]], "—"),
      fcto                 = .coalesce_chr(a_data[["Field CTO"]], "—"),
      next_renewal         = next_renewal,
      account_url          = .coalesce_chr(a_data[["account_url"]], ""),
      customer_central_url = .coalesce_chr(a_data[["customer_central_url"]], "")
    ),
    adoption = list(
      modules         = modules,
      platforms       = .coalesce_chr(
        if (!is.null(c_data)) c_data[["platform_prod"]] else NA, "—"),
      platforms_dev   = .coalesce_chr(
        if (!is.null(c_data)) c_data[["platform_dev"]] else NA, "—"),
      features        = as.list(pbac_feats),
      features_text   = features_text,
      registered      = .tile_int(reg_raw),
      licensed        = .tile_int(lic_raw),
      ratio           = if (is.na(rat_raw)) "—" else paste0(round(rat_raw * 1000) / 10, "%"),
      lifecycle_stage = .coalesce_chr(a_data[["Lifecycle Stage"]], "—")
    ),
    performance = list(
      cases_90d        = .tile_int(.tile_num(s_data, "cases_90d")),
      cases_360d       = .tile_int(.tile_num(s_data, "cases_360d")),
      cases_total      = .tile_int(.tile_num(s_data, "cases_total")),
      escalations_90d  = .tile_int(.tile_num(s_data, "escalations_90d")),
      escalations_360d = .tile_int(.tile_num(s_data, "escalations_360d")),
      escalations_open = .tile_int(.tile_num(s_data, "escalations_open")),
      deployment       = dep_tag$label,
      deployment_color = dep_tag$colour,
      version          = .coalesce_chr(
        if (!is.null(c_data)) c_data[["Immuta Version"]] else NA, "—")
    ),
    engagement = list(
      # Same rows, same order as the Engagement block on the PNG tile.
      contacts = lapply(contact_role_rows(a_data), function(r) {
        list(label        = r$label,
             name         = r$name,
             relationship = r$relationship,
             relationship_colour = .strength_colour(r$relationship),
             influence    = r$influence,
             influence_colour    = .strength_colour(r$influence))
      }),
      last_qbr = fmt_date(a_data[["Date Last QBR"]]),
      next_qbr = fmt_date(a_data[["Date Next QBR"]]),
      qbr_note = .coalesce_chr(a_data[["QBR Note"]], "—")
    )
  )
}


#' Build slide payloads for every account in a region.
#'
#' Convenience wrapper over \code{\link{account_slide_payload}}.
#'
#' @param op_list Named list returned by \code{\link{enrich_opportunities}}.
#' @param region Character or \code{NULL}. Region to filter
#'   \code{op_list$accounts} by (matched case-insensitively against the
#'   \code{Region} column). \code{NULL} returns every account.
#'
#' @return An unnamed list of payloads, sorted by account name. Accounts that
#'   error are skipped with a warning.
#'
#' @examples
#' \dontrun{
#'   payloads <- region_slide_payloads(op_list, "West")
#'   jsonlite::write_json(payloads, "output/west_review.json", auto_unbox = TRUE)
#' }
#' @export
region_slide_payloads <- function(op_list, region = NULL) {

  accounts <- op_list$accounts

  if (!is.null(region)) {
    keep <- !is.na(accounts$Region) &
      tolower(trimws(accounts$Region)) == tolower(trimws(region))
    accounts <- accounts[keep, , drop = FALSE]

    if (nrow(accounts) == 0) {
      stop("No accounts found in region '", region, "'. Regions available: ",
           paste(sort(unique(na.omit(op_list$accounts$Region))), collapse = ", "),
           call. = FALSE)
    }
  }

  names_sorted <- sort(unique(accounts[["Account Name"]]))

  out <- lapply(names_sorted, function(a) {
    tryCatch(account_slide_payload(a, op_list),
             error = function(e) {
               warning("Skipped '", a, "': ", conditionMessage(e), call. = FALSE)
               NULL
             })
  })

  Filter(Negate(is.null), out)
}


#' Write slide payloads to JSON files in the output directory.
#'
#' Serialises \code{\link{region_slide_payloads}} to disk: one file per region,
#' plus a combined file covering every account. This is the same data the Google
#' Slides generator renders, so the JSON is a drop-in feed for anything else
#' that wants it — a Dust push, a static site, an external deck builder.
#'
#' Files land under \code{output/}, so they ride along with
#' \code{\link{sync_output_to_drive}} to the shared Drive folder.
#'
#' Each file wraps the account list in a small envelope:
#' \preformatted{
#' {
#'   "generated_at": "2026-08-12T09:14:02Z",
#'   "region": "West",
#'   "account_count": 23,
#'   "accounts": [ { "account": {...}, "adoption": {...},
#'                   "performance": {...}, "engagement": {...} }, ... ]
#' }
#' }
#'
#' @param op_list Named list returned by \code{\link{enrich_opportunities}}.
#' @param output_dir Character. Directory to write into. Created recursively if
#'   needed. Defaults to \code{"output/slide_payloads"}.
#' @param regions Character vector or \code{NULL}. Regions to write a file for.
#'   \code{NULL} (default) discovers every distinct non-empty region in
#'   \code{op_list$accounts}.
#' @param combined Logical. Also write \code{all-slide_payloads.json} covering
#'   every account regardless of region. Defaults to \code{TRUE}.
#' @param pretty Logical. Pretty-print the JSON. Defaults to \code{TRUE} —
#'   these files are small and being able to read them in a diff is worth more
#'   than the bytes.
#'
#' @return Invisibly, a character vector of written file paths.
#'
#' @examples
#' \dontrun{
#'   output_slide_payload_json(op_list)
#'   output_slide_payload_json(op_list, regions = "West", combined = FALSE)
#' }
#' @export
output_slide_payload_json <- function(op_list,
                                      output_dir = "output/slide_payloads",
                                      regions    = NULL,
                                      combined   = TRUE,
                                      pretty     = TRUE) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  if (is.null(regions)) {
    regions <- unique(trimws(as.character(op_list$accounts$Region)))
    regions <- sort(regions[!is.na(regions) & nzchar(regions)])
  }

  stamp <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  write_one <- function(region_label, payloads) {
    envelope <- list(
      generated_at  = stamp,
      region        = region_label,
      account_count = length(payloads),
      accounts      = payloads
    )

    fname <- paste0(gsub("[^A-Za-z0-9_-]", "_", region_label),
                    "-slide_payloads.json")
    path  <- file.path(output_dir, fname)

    jsonlite::write_json(envelope, path,
                         auto_unbox = TRUE, pretty = pretty, null = "null")

    message(".. wrote ", length(payloads), " payload(s): ", path)
    path
  }

  paths <- character(0)

  for (r in regions) {
    payloads <- tryCatch(region_slide_payloads(op_list, r),
                         error = function(e) {
                           warning("Region '", r, "' skipped: ",
                                   conditionMessage(e), call. = FALSE)
                           NULL
                         })
    if (!is.null(payloads) && length(payloads) > 0) {
      paths <- c(paths, write_one(r, payloads))
    }
  }

  if (combined) {
    all_payloads <- region_slide_payloads(op_list, NULL)
    paths <- c(paths, write_one("all", all_payloads))
  }

  invisible(paths)
}
