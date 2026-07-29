# =============================================================================
# Generate slide headers for all accounts
#
# Run this from the ImmuteR project root in RStudio or the R console:
#   source("scripts/generate_slide_headers.R")
#
# Output: output/slide_headers/<Account_Name>_slide_header.png
# One 1280 × 200 px PNG per account, named by account name.
# =============================================================================

# Load package functions (dev mode — no install needed)
devtools::load_all()

# Ensure required rendering packages are available
for (pkg in c("ggplot2", "svglite", "rsvg")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(pkg)
  }
}

# ── Load latest op_list snapshot ─────────────────────────────────────────────
rdata_files <- list.files(
  "data/",
  pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$",
  full.names = TRUE
)

if (length(rdata_files) == 0) stop("No opportunities_*.RData snapshot found in data/")

latest <- rdata_files[which.max(as.Date(gsub(".*opportunities_(.+)\\.RData", "\\1",
                                              rdata_files)))]
message("Loading: ", latest)
load(latest)   # loads op_list into environment

# ── Generate all headers ──────────────────────────────────────────────────────
paths <- generate_all_slide_headers(
  op_list    = op_list,
  output_dir = "output/slide_headers"
)

message("\nAll done. Files written to output/slide_headers/")
