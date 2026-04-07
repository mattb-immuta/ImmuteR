# =============================================================================
# ImmuteR — Execution Script
# Run this interactively from your IDE. Not part of the package namespace.
# =============================================================================

# During development: loads package functions without installing
devtools::load_all()

options(scipen = 999)

# -----------------------------------------------------------------------------
# Config — report IDs and sheet URLs
# -----------------------------------------------------------------------------

diff_date       <- "2026-01-12"
fiscal.period   <- c("Q1-2027", "Q2-2027", "Q3-2027", "Q4-2027")

# idex parquet data directory (relative to project root)
idex_path <- "data/idex/"

# Salesforce report IDs
sf_opportunity_report  <- "00OVH000005p6A92AI"
sf_account_report      <- "00OVH0000069G8Y2AU"
sf_provision_report    <- "00OVH000006Zryr2AC"
sf_support_case_report <- "00OVH000006dyX72AI"
sf_contract_report     <- "00OVH0000070ZRZ2A2"
sf_active_order_report <- "00OVV000006AOnB2AW"

# Google Sheet URLs
account_doc_tracker        <- "https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"
looker_user_data_path      <- "https://docs.google.com/spreadsheets/d/11GQF96cDxwpidj0_OgvliNZiShug5e4lVyIqGCM6Qj0/edit?usp=sharing"
customer_ops_tracker       <- "https://docs.google.com/spreadsheets/d/1BS3rBeDZJBflauFZc5cEdw09JTAfBM5Hwbcdh1rk1Bg/edit?usp=sharing"
customer_expansion_tracker <- "https://docs.google.com/spreadsheets/d/1gI5N6FHArFMn65y1y_9pc1l9TUJGViHxYkzVXbHNk9Y/edit?gid=0#gid=0"
BU_staging                 <- "https://docs.google.com/spreadsheets/d/1WuHvaVkHrLQkoUB9MiDe7OvM4Qw6JyQW25KWaJbjJpA/edit?gid=493722263#gid=493722263"
mb_tracker                 <- "https://docs.google.com/spreadsheets/d/1BLwqFOZpEmDvBkIAH7rCYBzPIJ0ADIUJfndzDRe9WgM/edit?usp=sharing"

# -----------------------------------------------------------------------------
# Authenticate
# -----------------------------------------------------------------------------

sf_auth()

# -----------------------------------------------------------------------------
# Load and enrich data
# -----------------------------------------------------------------------------

new_op_list <- enrich_opportunities(
  sf_opportunity_report  = sf_opportunity_report,
  sf_account_report      = sf_account_report,
  sf_provision_report    = sf_provision_report,
  sf_support_case_report = sf_support_case_report,
  sf_contract_report     = sf_contract_report,
  fiscal.period          = fiscal.period,
  idex                   = FALSE,
  idex_path              = idex_path
)

load(paste0("data/opportunities_", diff_date, ".RData"))

op_list <- enrich_list_object(new_list = new_op_list,
                              old_list = op_list)

op_list[["constants"]] <- load_constants()

# -----------------------------------------------------------------------------
# Produce outputs
# -----------------------------------------------------------------------------

# output_gsheet_mbpipeline(op_list = op_list, mb_tracker)
output_customer_ops_tracker(op_list              = op_list,
                            customer_ops_tracker = customer_ops_tracker,
                            fiscal.period        = fiscal.period)

output_gsheet_BU_staging(op_list       = op_list,
                         BU_staging    = BU_staging,
                         fiscal.period = fiscal.period)

# output_expansion_tracker(op_list = op_list, customer_expansion_tracker,
#                          fiscal.period = c("Q3-2026","Q4-2026","Q1-2027","Q2-2027"))

# -----------------------------------------------------------------------------
# Render Rmarkdown report
# -----------------------------------------------------------------------------

rmarkdown::render("output/BottomsUp.Rmd", output_format = "html_document")

# -----------------------------------------------------------------------------
# Save list object for next run's diff
# -----------------------------------------------------------------------------

op_list[["dates"]] <- paste0("Run Date: ", Sys.Date(),
                             " , Comparison Date: ", diff_date, ".RData")
message("Saving op_list to: data/opportunities_", Sys.Date(), ".RData")
save(op_list, file = paste0("data/opportunities_", Sys.Date(), ".RData"))
