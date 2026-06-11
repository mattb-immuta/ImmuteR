# =============================================================================
# ImmuteR — Plumber API
# Exposes a /refresh endpoint so the HTML dashboard can trigger a data re-run.
#
# Start the server via:  source("scripts/serve.R")
# =============================================================================

# Resolve the project root from the env var set by serve.R.
# Falls back to the current working directory if somehow not set.
.project_root <- Sys.getenv("IMMUTER_PROJECT_ROOT", unset = normalizePath(getwd()))

.proj <- function(...) file.path(.project_root, ...)


#* @filter cors
#* Allows requests from file:// origins (the local HTML dashboard)
function(req, res) {
  origin <- if (!is.null(req$HTTP_ORIGIN) && nzchar(req$HTTP_ORIGIN)) req$HTTP_ORIGIN else "*"
  res$setHeader("Access-Control-Allow-Origin",  origin)
  res$setHeader("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}


#* Ping — confirms the server is alive
#* @get /status
function() {
  list(status = "running", timestamp = format(Sys.time()))
}


#* List available snapshot dates (newest first)
#* @get /snapshots
function() {
  files <- list.files(.proj("data/"), pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
  dates <- sort(gsub("opportunities_(.+)\\.RData", "\\1", files), decreasing = TRUE)
  list(dates = dates)
}


#* Re-run main.R with an optional diff_date and regenerate all outputs
#* @param diff_date Comparison date as YYYY-MM-DD. Defaults to most recent snapshot.
#* @get /refresh
function(diff_date = "") {
  message("\n[api] /refresh triggered at ", Sys.time(),
          if (nzchar(diff_date)) paste0("  diff_date=", diff_date) else "  (auto-detect)")

  if (nzchar(diff_date)) Sys.setenv(IMMUTER_DIFF_DATE = diff_date)
  on.exit(Sys.unsetenv("IMMUTER_DIFF_DATE"), add = TRUE)

  tryCatch({
    source(.proj("scripts/main.R"), local = FALSE)
    list(status = "ok", diff_date = diff_date, timestamp = format(Sys.time()))
  }, error = function(e) {
    message("[api] ERROR: ", conditionMessage(e))
    list(status = "error", message = conditionMessage(e), timestamp = format(Sys.time()))
  })
}
