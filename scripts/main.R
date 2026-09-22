# =============================================================================
# ImmuteR — Execution Script
# Run this interactively from your IDE. Not part of the package namespace.
# =============================================================================

# When sourced from the plumber server the working directory may not be the
# project root, causing every relative path (data/, output/, etc.) to fail.
# IMMUTER_PROJECT_ROOT is set by serve.R; snap to it if we're not already there.
.root <- Sys.getenv("IMMUTER_PROJECT_ROOT", unset = "")
if (nzchar(.root) && !identical(normalizePath(getwd()), normalizePath(.root))) {
  message("[main] setting wd to ", .root)
  setwd(.root)
}
rm(.root)

# Load packages used directly in this script
library(salesforcer)
library(googlesheets4)
library(bigrquery)
library(rmarkdown)
library(dplyr)
library(httr2)

# Load ImmuteR package functions (dev mode — no install needed)
devtools::load_all()

options(scipen = 999)

# -----------------------------------------------------------------------------
# Config — report IDs and sheet URLs
# -----------------------------------------------------------------------------

# Use a date passed from the refresh server, or auto-detect the most recent snapshot
.env_diff_date <- Sys.getenv("IMMUTER_DIFF_DATE", unset = "")
if (nzchar(.env_diff_date)) {
  diff_date <- .env_diff_date
  message("Using diff_date from server request: ", diff_date)
} else {
  .rdata_files <- list.files("data/", pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
  if (length(.rdata_files) == 0) stop("No snapshot found in data/ — run without a diff first to create one.")
  diff_date <- format(max(as.Date(gsub("opportunities_(.+)\\.RData", "\\1", .rdata_files))), "%Y-%m-%d")
  message("Auto-detected diff_date: ", diff_date)
}
rm(.env_diff_date)
fiscal.period   <- c("Q2-2027", "Q3-2027", "Q4-2027","Q1-2028")

# Asana
# Generate your token at https://app.asana.com/0/my-apps
# Then add ASANA_TOKEN=your_token to your .Renviron (usethis::edit_r_environ())
asana_token       <- Sys.getenv("ASANA_TOKEN")
asana_project_gid <- "YOUR_PROJECT_GID"  # from your Asana project URL


# Salesforce report IDs
sf_opportunity_report    <- "00OVH000005p6A92AI"
sf_account_report        <- "00OVH0000069G8Y2AU"
sf_active_order_report   <- "00OVV000006AOnB2AW"
sf_saas_migration_report <- "00OVV000006FbWr2AK"
sf_csm_summary_report    <- "00OVV000006FbNB2A0"
sf_orders_report         <- "00OVV000005RTxV2AW"
sf_forecast_report       <- "00OVH00000706ML2AY"  # CompanyChurnReport (renewal churn forecast)

# Google Sheet URLs
account_doc_tracker        <- "https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"
looker_user_data_path      <- "https://docs.google.com/spreadsheets/d/11GQF96cDxwpidj0_OgvliNZiShug5e4lVyIqGCM6Qj0/edit?usp=sharing"
customer_ops_tracker       <- "https://docs.google.com/spreadsheets/d/1BS3rBeDZJBflauFZc5cEdw09JTAfBM5Hwbcdh1rk1Bg/edit?usp=sharing"
customer_expansion_tracker <- "https://docs.google.com/spreadsheets/d/1gI5N6FHArFMn65y1y_9pc1l9TUJGViHxYkzVXbHNk9Y/edit?gid=0#gid=0"
BU_staging                 <- "https://docs.google.com/spreadsheets/d/1WuHvaVkHrLQkoUB9MiDe7OvM4Qw6JyQW25KWaJbjJpA/edit?gid=493722263#gid=493722263"
mb_tracker                 <- "https://docs.google.com/spreadsheets/d/1BLwqFOZpEmDvBkIAH7rCYBzPIJ0ADIUJfndzDRe9WgM/edit?usp=sharing"
rsa_tracker                 <- "https://docs.google.com/spreadsheets/d/1-jb1sdsrkPqUbRBfDUfLo2H3iuOlsJjxY7PhO9ofMnw/edit?gid=701246507#gid=701246507"
# -----------------------------------------------------------------------------
# Authenticate
# -----------------------------------------------------------------------------

sf_auth()
googlesheets4::gs4_auth(email = "matt.benigni@immuta.com")

# -----------------------------------------------------------------------------
# Load and enrich data
# -----------------------------------------------------------------------------

new_op_list <- enrich_opportunities(
  sf_opportunity_report    = sf_opportunity_report,
  sf_account_report        = sf_account_report,
  sf_active_order_report   = sf_active_order_report,
  sf_saas_migration_report = sf_saas_migration_report,
  sf_csm_summary_report    = sf_csm_summary_report,
  sf_orders_report         = sf_orders_report,
  sf_forecast_report       = sf_forecast_report,
  fiscal.period            = fiscal.period,
  looker                   = TRUE
)

load(paste0("data/opportunities_", diff_date, ".RData"))

op_list <- enrich_list_object(new_list = new_op_list,
                              diff_date=diff_date)

op_list[["constants"]] <- load_constants()

# -----------------------------------------------------------------------------
# Produce outputs
# -----------------------------------------------------------------------------

report_all_accounts_html(op_list)   # -> output/immuta_accounts.html

# Same records as the HTML page, as JSON, for the Dust frame experiment.
# -> output/dust_frame/{accounts,changes,consumption}.json + schema.md
# See DUST_FRAME_AGENT.md. Cheap to write; drop this line if the experiment ends.
output_dust_payload(op_list)

# Every account on the two axes the PBAC rating is built from, with the rating
# grid shaded behind them. Needs a looker = TRUE run — skipped, with a note,
# when consumption$pbac_adoption isn't there.
if (!is.null(op_list$consumption$pbac_adoption) &&
    nrow(op_list$consumption$pbac_adoption) > 0) {
  report_pbac_scatter(op_list)      # -> output/pbac_scatter.html
} else {
  message("Skipping report_pbac_scatter(): no consumption$pbac_adoption ",
          "(SFDC-only run?)")
}


# Slide assets, written locally only (see the Drive section at the bottom —
# these are deliberately NOT uploaded).
#   output/slide_content/headers/<n>_<account_name>_header.png
#   output/slide_content/health_summaries/<n>_<account_name>_health.png
#   output/slide_content/slide_assets_manifest.csv
# One matched pair per account, numbered 1..N with no gaps. Both files share the
# same <n>_<account_name> stem, which is what the Google Slides builder pairs on
# — so generate both together rather than calling generate_all_slide_headers()
# and generate_all_slide_tiles() separately, which can leave orphaned headers.

op_list_inspector   = "https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?gid=926810859#gid=926810859"
sheet_write(
  data = op_list$accounts,
  ss   = op_list_inspector,
  sheet = "AccountList"
)

sheet_write(
  data = op_list$consumption$users,
  ss   = op_list_inspector,
  sheet = "users"
)

sheet_write(
  data = op_list$consumption$data_sources,
  ss   = op_list_inspector,
  sheet = "data_sources"
)

sheet_write(
  data = op_list$consumption$sdd_consumption,
  ss   = op_list_inspector,
  sheet = "sdd"
)

sheet_write(
  data = op_list$consumption$pb ,
  ss   = op_list_inspector,
  sheet = "pbac"
)

sheet_write(
  data = op_list$consumption$policy_metrics ,
  ss   = op_list_inspector,
  sheet = "policies"
)
# -----------------------------------------------------------------------------
# Save list object for next run's diff
# -----------------------------------------------------------------------------

op_list[["dates"]] <- paste0("Run Date: ", Sys.Date(),
                             " , Comparison Date: ", diff_date, ".RData")
message("Saving op_list to: data/opportunities_", Sys.Date(), ".RData")
save(op_list, file = paste0("data/opportunities_", Sys.Date(), ".RData"))


# -----------------------------------------------------------------------------
# Full op_list JSON dump
# -----------------------------------------------------------------------------
# output/current_state/op_list.json — everything the run produced, including
# op_list$forecast and op_list$changes_detail$forecast. Written after the
# snapshot save so the file and the .RData always describe the same run.
dir.create("output/current_state", recursive = TRUE, showWarnings = FALSE)
tryCatch(
  jsonlite::write_json(op_list, "output/current_state/op_list.json",
                       auto_unbox = TRUE, na = "null", pretty = TRUE),
  error = function(e) {
    warning("op_list.json write skipped: ", conditionMessage(e), call. = FALSE)
  }
)

# -----------------------------------------------------------------------------
# Publish the dashboard to the shared Google Drive folder
# -----------------------------------------------------------------------------
# immuta_accounts.html plus the Dust frame payload (output/dust_frame/, ~365 KB
# across four files) — the JSON that a Dust agent is handed to rebuild the
# dashboard, so it belongs next to the dashboard it is being compared against.
# push_output_to_drive() recreates local subdirectories as Drive subfolders, so
# these land in a `dust_frame` folder rather than cluttering the shared root.
#
# The slide PNGs stay local — they're regenerated every run, there are 144 of
# them, and re-uploading the set each time is slow for no benefit. Push them by
# hand when you actually need them on Drive:
#
#   push_output_to_drive()                              # everything
#   push_output_to_drive(pattern = "slide_content")     # just the PNGs
#
# Everything is matched by name and updated in place, so the dashboard's Drive
# link and file ID — and the payload files' links — stay stable across runs.
#
# Wrapped in tryCatch so a Drive failure warns rather than killing the run (or
# the plumber /refresh request) after everything else already succeeded.
tryCatch(
  push_output_to_drive(pattern = "^(immuta_accounts\\.html|dust_frame/)"),
  error = function(e) {
    warning("Drive push skipped: ", conditionMessage(e), call. = FALSE)
  }
)
