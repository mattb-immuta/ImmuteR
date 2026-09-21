# =============================================================================
# Generate slide headers for all accounts
#
# Run this from the ImmuteR project root in RStudio or the R console:
#   source("scripts/generate_slide_headers.R")
#
# Output: output/slide_content/headers/<n>_<account_name>_header.png
#         output/slide_content/health_summaries/<n>_<account_name>_health.png
#         output/slide_content/slide_assets_manifest.csv
#
# Headers and health summaries are generated together, as one matched pair per
# account numbered 1..N (e.g. 1_acorns_header.png + 1_acorns_health.png). The
# Google Slides builder pairs the two files on that <n>_<account_name> stem, so
# they must be produced in the same pass — running the header generator on its
# own can leave a header with no health summary beside it.
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

# ── Generate all slide assets ─────────────────────────────────────────────────
manifest <- generate_all_slide_assets(op_list)

message("\nAll done. ", nrow(manifest), " account(s) written to ",
        "output/slide_content/")
