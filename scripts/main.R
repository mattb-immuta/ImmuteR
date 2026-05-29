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

# idex parquet data directory (relative to project root)
idex_path <- "data/idex/"

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
  fiscal.period            = fiscal.period,
  idex                     = FALSE,
  idex_path                = idex_path
)

load(paste0("data/opportunities_", diff_date, ".RData"))

op_list <- enrich_list_object(new_list = new_op_list,
                              diff_date=diff_date)

op_list[["constants"]] <- load_constants()

# -----------------------------------------------------------------------------
# Produce outputs
# -----------------------------------------------------------------------------

report_all_accounts_html(op_list)
# output_gsheet_mbpipeline(op_list = op_list, mb_tracker)
#output_customer_ops_tracker(op_list              = op_list,
#                            customer_ops_tracker = customer_ops_tracker,
#                            fiscal.period        = fiscal.period)

output_gsheet_BU_staging(op_list       = op_list,
                         BU_staging    = BU_staging,
                         fiscal.period = fiscal.period)



sheet_write(
  data = report_all_accounts(op_list),
  ss   = "https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?gid=926810859#gid=926810859",
  sheet = "AccountList"
)


# -----------------------------------------------------------------------------
# Save list object for next run's diff
# -----------------------------------------------------------------------------

op_list[["dates"]] <- paste0("Run Date: ", Sys.Date(),
                             " , Comparison Date: ", diff_date, ".RData")
message("Saving op_list to: data/opportunities_", Sys.Date(), ".RData")
save(op_list, file = paste0("data/opportunities_", Sys.Date(), ".RData"))
