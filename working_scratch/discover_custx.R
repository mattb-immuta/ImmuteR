# Discovery: find the per-escalation table behind custx_open
# =============================================================================
# Scratch, not part of the package. Run it, paste the output back, and the real
# Escalations pull gets written against actual column names instead of guesses.
#
#   source("working_scratch/discover_custx.R")
#
# Read-only: it lists tables, reads schemas, and previews a handful of rows.
# Nothing is written anywhere.

library(bigrquery)
bq_auth()   # cached after the first run, same as the rollup query

PROJECT  <- "retool-457517"
DATASETS <- c("reports")   # add more here if the escalations live elsewhere

# Tables whose name hints at escalation/case/ticket detail rather than a rollup.
INTERESTING <- "custx|escalat|case|ticket|conversation|front|support"

cat("\n================ DATASETS IN", PROJECT, "================\n")
all_ds <- tryCatch(bq_project_datasets(PROJECT), error = function(e) {
  cat("!! could not list datasets:", conditionMessage(e), "\n"); list()
})
for (d in all_ds) cat("  ", d$dataset, "\n")

# If the dataset list came back, look at every one of them rather than just the
# guess above — the escalation detail may not sit beside the rollup.
if (length(all_ds) > 0) {
  DATASETS <- unique(c(DATASETS, vapply(all_ds, function(d) d$dataset, character(1))))
}

hits <- list()

for (ds in DATASETS) {
  cat("\n================ TABLES IN", paste0(PROJECT, ".", ds), "================\n")

  tbls <- tryCatch(bq_dataset_tables(bq_dataset(PROJECT, ds)),
                   error = function(e) { cat("  !! ", conditionMessage(e), "\n"); list() })
  if (length(tbls) == 0) next

  names_vec <- vapply(tbls, function(t) t$table, character(1))
  for (nm in sort(names_vec)) {
    flag <- if (grepl(INTERESTING, nm, ignore.case = TRUE)) "  <-- candidate" else ""
    cat("  ", nm, flag, "\n", sep = "")
  }

  hits <- c(hits, lapply(names_vec[grepl(INTERESTING, names_vec, ignore.case = TRUE)],
                         function(nm) list(ds = ds, tbl = nm)))
}

# ── Schema + row count + preview for each candidate ──────────────────────────
for (h in hits) {
  fq <- paste0(PROJECT, ".", h$ds, ".", h$tbl)
  cat("\n\n================ ", fq, " ================\n")

  tb <- bq_table(PROJECT, h$ds, h$tbl)

  n <- tryCatch(bq_table_nrow(tb), error = function(e) NA)
  cat("rows: ", n, "\n\n", sep = "")

  # Column names and types — this is the part that matters most.
  flds <- tryCatch(bq_table_fields(tb), error = function(e) NULL)
  if (!is.null(flds)) {
    cat("columns:\n")
    for (f in flds) cat(sprintf("  %-38s %s\n", f$name, f$type))
  }

  # A few rows, so the shape of severity/status values is visible. Wrapped
  # because a table can be listable but not readable.
  prev <- tryCatch(bq_table_download(tb, n_max = 3), error = function(e) {
    cat("\n  !! could not preview:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(prev) && nrow(prev) > 0) {
    cat("\nfirst ", nrow(prev), " row(s), one column per line:\n", sep = "")
    for (i in seq_len(nrow(prev))) {
      cat("  --- row", i, "---\n")
      for (cn in names(prev)) {
        v <- prev[[cn]][i]
        v <- if (is.list(v)) "<nested>" else as.character(v)
        if (!is.na(v) && nchar(v) > 90) v <- paste0(substr(v, 1, 90), "...")
        cat(sprintf("    %-38s %s\n", cn, v))
      }
    }
  }

  # Distinct values for anything that looks like a severity or status field —
  # the facets in the tab are only as good as knowing what these actually hold.
  if (!is.null(flds)) {
    cat_cols <- Filter(function(f) grepl("sever|priorit|status|state|stage|type|tier|region",
                                         f$name, ignore.case = TRUE), flds)
    for (f in cat_cols) {
      sql <- sprintf("SELECT `%s` AS v, COUNT(*) n FROM `%s` GROUP BY v ORDER BY n DESC LIMIT 15",
                     f$name, fq)
      vals <- tryCatch(bq_table_download(bq_project_query(PROJECT, sql)),
                       error = function(e) NULL)
      if (!is.null(vals)) {
        cat("\ndistinct `", f$name, "`:\n", sep = "")
        for (i in seq_len(nrow(vals))) {
          cat(sprintf("    %-40s %s\n", as.character(vals$v[i]), vals$n[i]))
        }
      }
    }
  }
}

cat("\n\n================ WHAT TO PASTE BACK ================\n")
cat("For whichever table holds ONE ROW PER ESCALATION:\n")
cat("  1. the fully-qualified table name\n")
cat("  2. its column list (the `columns:` block above)\n")
cat("  3. the distinct severity and status values\n")
cat("  4. which column joins to Salesforce (an account id, or the account name)\n\n")
cat("If nothing above is per-escalation, the detail may not be in BigQuery at\n")
cat("all -- the rollup's custx_open could be computed upstream in Front or\n")
cat("Retool, in which case say so and we will find another source.\n\n")
