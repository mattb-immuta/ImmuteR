# =============================================================================
# ImmuteR — Start the local refresh server
#
# Run this ONCE from the project root before using the Refresh button in the
# HTML dashboard.  Leave the R session running while you use the dashboard.
#
# NOTE: Salesforce and Google auth tokens must already be cached.
#   sf_auth()             — run interactively once to cache the token
#   gs4_auth(email = ...) — run interactively once to cache the token
# =============================================================================

library(plumber)
library(devtools)

# Ensure we're at the project root so relative paths in main.R resolve correctly
if (!file.exists("scripts/main.R")) {
  stop("Run serve.R from the ImmuteR project root, e.g.:\n",
       '  setwd("/path/to/ImmuteR"); source("scripts/serve.R")')
}

# Pin the absolute project root so api.R handlers can always find it,
# regardless of what working directory plumber uses when handling requests.
Sys.setenv(IMMUTER_PROJECT_ROOT = normalizePath(getwd()))

devtools::load_all()

message("Starting ImmuteR refresh server on http://localhost:8765")
message("Open the dashboard HTML, then click the Refresh button to trigger a run.\n")

plumber::pr("scripts/api.R") |>
  plumber::pr_run(port = 8765, host = "127.0.0.1")
