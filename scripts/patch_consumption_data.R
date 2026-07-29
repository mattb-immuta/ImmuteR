# patch_consumption_data.R
# Wires op_list$orders into the Consumption tab.
# Run once from the project root, then devtools::load_all() + main.R.

message("Patching R/report_all_accounts_html.R for consumption data ...")

r_file <- "R/report_all_accounts_html.R"
if (!file.exists(r_file)) stop("Run from project root — ", r_file, " not found.")

src <- paste(readLines(r_file, warn = FALSE), collapse = "\n")

patch <- function(src, old, new, label) {
  if (!grepl(old, src, fixed = TRUE)) {
    message("  SKIP (not found — already applied?): ", label); return(src)
  }
  message("  OK: ", label)
  sub(old, new, src, fixed = TRUE)
}

# ── 1. Build consumption_json from op_list$orders before calling .rah_html() ──
src <- patch(src,
  '  changes_json <- .build_changes_json(op_list)\n\n  # Build HTML -----------------------------------------------------------------\n  html <- .rah_html(json_data, regions, csms, snap_dates, changes_json)',
  '  changes_json     <- .build_changes_json(op_list)
  consumption_json <- .build_consumption_json(op_list)

  # Build HTML -----------------------------------------------------------------
  html <- .rah_html(json_data, regions, csms, snap_dates, changes_json, consumption_json)',
  "call site: pass consumption_json to .rah_html()"
)

# ── 2. Add consumption_json parameter to .rah_html() signature ────────────────
src <- patch(src,
  '.rah_html <- function(json_data, regions, csms = character(0), snap_dates = character(0), changes_json = "null") {',
  '.rah_html <- function(json_data, regions, csms = character(0), snap_dates = character(0), changes_json = "null", consumption_json = "null") {',
  ".rah_html() signature"
)

# ── 3. Replace the hardcoded null injection with the real variable ─────────────
src <- patch(src,
  '  tmpl <- gsub("{{CONSUMPTION}}",  "null",                tmpl, fixed = TRUE)',
  '  tmpl <- gsub("{{CONSUMPTION}}",  consumption_json,      tmpl, fixed = TRUE)',
  "gsub: use consumption_json variable"
)

# ── 4. Add .build_consumption_json() after .build_changes_json() ─────────────
# Find the end of .build_changes_json and insert our new function after it
old_anchor <- '# ---- Internal: full HTML page template -------------------------------------'
new_fn <- r"(.build_consumption_json <- function(op_list) {
  orders <- op_list$orders
  if (is.null(orders) || nrow(orders) == 0L) return("null")

  # Helper: strip currency formatting to numeric
  .num <- function(x) suppressWarnings(as.numeric(gsub("[$,%,]", "", gsub(",", "", x))))

  orders <- orders |>
    dplyr::mutate(
      order_arr_num  = .num(`Order Product ARR Average`),
      contract_arr   = .num(`Contract ARR Rollup`),
      total_price    = .num(`Total Price`),
      contract_end   = suppressWarnings(as.Date(`Contract End Date`, "%m/%d/%Y")),
      is_saas        = as.logical(`Is SaaS Customer`)
    )

  # ── stats$by_mapping ─────────────────────────────────────────────────────────
  by_mapping <- orders |>
    dplyr::filter(!is.na(`Mo Mapping`), `Mo Mapping` != "") |>
    dplyr::group_by(Mo_Mapping = `Mo Mapping`) |>
    dplyr::summarize(
      n         = dplyr::n(),
      sum_arr   = round(sum(order_arr_num,  na.rm = TRUE), 2),
      sum_total = round(sum(total_price,    na.rm = TRUE), 2),
      .groups   = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(sum_arr))

  # ── renewals (contract-level rows from SFDC; Looker metrics will be null) ───
  renewals <- orders |>
    dplyr::distinct(`Account Name`, contract_end, contract_arr, is_saas) |>
    dplyr::rename(Customer = `Account Name`, Date = contract_end,
                  Amount = contract_arr, SaaS = is_saas) |>
    dplyr::mutate(
      Date       = as.character(Date),
      Amount_raw = Amount,
      Amount     = ifelse(!is.na(Amount),
                          paste0("$", formatC(Amount, format = "f", digits = 0, big.mark = ",")),
                          NA_character_)
    ) |>
    dplyr::arrange(Date)

  # Join op_list$consumption if it has already been computed
  con <- op_list$consumption
  if (!is.null(con) && !is.null(con$renewals)) {
    ren <- con$renewals |>
      dplyr::mutate(
        Amount_raw              = Amount,
        Data_Sources_raw        = `Data Sources`,
        Users_raw               = Users,
        License_Consumption_num = suppressWarnings(as.numeric(gsub("%", "", `License Consumption`))),
        Policies_raw            = Policies,
        q31_raw                 = `Users With Queries Last 31 Days`,
        q90_raw                 = `Users With Queries Last 90 Days`,
        Support_Cases_raw       = `Support Cases`,
        Date                    = as.character(Date)
      )
    low <- con$low_consumption |>
      dplyr::mutate(
        Amount_raw              = Amount,
        License_Consumption_num = suppressWarnings(as.numeric(gsub("%", "", `License Consumption`))),
        Date                    = as.character(Date)
      )
  } else {
    ren <- renewals
    low <- renewals |> dplyr::filter(FALSE)  # empty, same structure
  }

  .to_json(list(
    stats           = list(by_mapping = by_mapping),
    renewals        = ren,
    low_consumption = low
  ))
}

# ---- Internal: full HTML page template -------------------------------------)"

src <- patch(src, old_anchor, paste0(new_fn, "\n\n", old_anchor),
             ".build_consumption_json() function")

# ── Write ──────────────────────────────────────────────────────────────────────
writeLines(src, r_file)
message("Done. Run devtools::load_all() then main.R (or report_all_accounts_html(op_list)).")
