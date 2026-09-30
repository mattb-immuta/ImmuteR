# Check the Request adoption derivation against Looker dashboard 1009 before
# trusting it in the pipeline: which tile each stage matched, how many accounts
# each lists, and where the derived tier disagrees with the Salesforce picklist.
#
# Run from the project root:
#   source("working_scratch/inspect_dashboard_1009.R")
#
# Needs LOOKER_CLIENT_ID / LOOKER_CLIENT_SECRET in .Renviron.

devtools::load_all()

tok  <- looker_auth()
dash <- looker_dashboard(1009, tok)

# ── 1. Every tile, so a renamed stage tile is obvious ─────────────────────────
cat("\n=== tiles on dashboard 1009 ===\n")
cat(paste0("  - ", vapply(dash$dashboard_elements,
                          function(e) .nz(e$title, "<untitled>"),
                          character(1))), sep = "\n")

# ── 2. Each stage tile: what it returns and who is on it ──────────────────────
# .RA_TILE_PATTERNS maps stage -> title substring, weakest stage first.
stages <- list()

for (stage in names(.RA_TILE_PATTERNS)) {
  pattern <- .RA_TILE_PATTERNS[[stage]]
  hits <- Filter(function(e) grepl(pattern, tolower(.nz(e$title, "")), fixed = TRUE),
                 dash$dashboard_elements)

  cat("\n=== ", stage, " (pattern: '", pattern, "') ===\n", sep = "")
  if (length(hits) == 0) {
    cat("  NO TILE MATCHED - check the titles above\n")
    stages[[stage]] <- character(0)
    next
  }
  cat("  matched ", length(hits), " tile(s); using: ",
      .nz(hits[[1]]$title, "<untitled>"), "\n", sep = "")

  df <- run_looker_dashboard_tile(hits[[1]], tok)
  cat("  rows: ", nrow(df), "\n  columns:\n", sep = "")
  cat(paste0("    - ", names(df), collapse = "\n"), "\n")

  # `Show row` is the retained tile's membership test; print its split wherever
  # the column appears, and note whether this stage is being filtered on it.
  if ("Show row" %in% names(df)) {
    cat("  `Show row` present; filtered on it for this stage: ",
        stage %in% "retained", "\n", sep = "")
    print(table(df[["Show row"]], useNA = "ifany"))
  }

  stages[[stage]] <- .ra_stage_accounts(
    dash, pattern, tok,
    Sys.getenv("LOOKER_BASE_URL", "https://immuta.cloud.looker.com"),
    show_row = stage %in% "retained"
  )
  cat("  accounts counted: ", length(stages[[stage]]), "\n", sep = "")
}

# ── 3. Derived tiers vs the Salesforce picklist ───────────────────────────────
snap <- sort(list.files("data", pattern = "^opportunities_.*RData$",
                        full.names = TRUE))
snap <- snap[length(snap)]
message("Comparing against snapshot: ", snap)
load(snap)   # -> op_list

# `stages` is reused so the tiles aren't pulled a second time.
ra <- op_list_request_adoption(accounts = op_list$accounts,
                               dash     = dash,
                               token    = tok,
                               stages   = stages)

cat("\n=== derived distribution ===\n")
print(table(ra$request_adoption_looker_stage, useNA = "ifany"))

# Off-diagonal cells are the accounts where the picklist and the dashboard
# disagree — read those before switching anything over to the derived value.
cat("\n=== SFDC vs derived (colour) ===\n")
print(table(.ra_colour(op_list$accounts$`Request Adoption`),
            .ra_colour(ra$request_adoption_looker),
            dnn = c("SFDC", "Derived"), useNA = "ifany"))

cat("\n=== disagreements ===\n")
disagree <- ra |>
  dplyr::mutate(sfdc = .ra_colour(op_list$accounts$`Request Adoption`)) |>
  dplyr::filter(!is.na(request_adoption_match), !request_adoption_match) |>
  dplyr::select(`Account Name`, sfdc, request_adoption_looker_stage)

print(disagree, n = Inf, width = Inf)

# Accounts on a stage tile that aren't in op_list$accounts at all — usually a
# name mismatch between Looker and Salesforce rather than a genuinely unknown
# customer, and each one is an account silently reading Black.
cat("\n=== on a tile but not matched to an account ===\n")
print(setdiff(unlist(stages, use.names = FALSE),
              trimws(as.character(op_list$accounts$`Account Name`))))
