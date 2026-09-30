# Discovery: the CUSTX Jira project's field shape
# =============================================================================
# Scratch, not part of the package. Run it, paste the output back, and
# op_list_escalations() gets written against real field ids instead of guesses.
#
#   source("working_scratch/discover_custx_jira.R")
#
# Read-only: three GETs and one JQL search. Nothing is created or modified.
#
# ── Setup (one time) ─────────────────────────────────────────────────────────
# Create a token at https://id.atlassian.com/manage-profile/security/api-tokens
# then add to .Renviron (usethis::edit_r_environ()) and restart R:
#
#   JIRA_EMAIL=matt.benigni@immuta.com
#   JIRA_TOKEN=your_api_token
#
# The same two variables are what op_list_escalations() will read, so this
# doubles as the credential check.

library(httr2)

JIRA_BASE  <- Sys.getenv("JIRA_BASE_URL", unset = "https://immuta.atlassian.net")
JIRA_EMAIL <- Sys.getenv("JIRA_EMAIL")
JIRA_TOKEN <- Sys.getenv("JIRA_TOKEN")
SAMPLE_KEY <- "CUSTX-3386"

if (!nzchar(JIRA_EMAIL) || !nzchar(JIRA_TOKEN)) {
  stop("Set JIRA_EMAIL and JIRA_TOKEN in .Renviron first (see the header).",
       call. = FALSE)
}

jira_get <- function(path, query = list()) {
  request(paste0(JIRA_BASE, path)) |>
    req_auth_basic(JIRA_EMAIL, JIRA_TOKEN) |>
    req_headers(Accept = "application/json") |>
    req_url_query(!!!query) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
}

show_resp <- function(r, what) {
  if (resp_status(r) >= 300) {
    cat("!! ", what, " failed: HTTP ", resp_status(r), "\n", sep = "")
    cat("   ", substr(resp_body_string(r), 1, 300), "\n\n", sep = "")
    return(NULL)
  }
  resp_body_json(r)
}

# Render a field value compactly. Jira nests almost everything one level deep
# ({name:}, {value:}, {displayName:}), and rich text arrives as an ADF document
# that is not worth printing.
flat <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (is.atomic(v) && length(v) == 1) return(as.character(v))
  if (is.list(v)) {
    for (k in c("name", "value", "displayName", "key", "emailAddress")) {
      if (!is.null(v[[k]]) && is.atomic(v[[k]])) return(as.character(v[[k]]))
    }
    if (!is.null(v$type) && identical(v$type, "doc")) return("<rich text>")
    if (is.null(names(v)) && length(v) > 0) {
      return(paste(vapply(v, flat, character(1)), collapse = " | "))
    }
    return(paste0("<object: ", paste(names(v), collapse = ", "), ">"))
  }
  paste(as.character(v), collapse = " | ")
}

# ── 1. One real issue, every populated field, with human-readable names ──────
cat("\n================ ", SAMPLE_KEY, " ================\n", sep = "")

issue <- show_resp(jira_get(paste0("/rest/api/3/issue/", SAMPLE_KEY),
                            list(expand = "names")), SAMPLE_KEY)

if (!is.null(issue)) {
  nm <- issue$names %||% list()
  f  <- issue$fields

  populated <- Filter(function(k) {
    v <- f[[k]]
    !(is.null(v) || (is.list(v) && length(v) == 0) ||
      (is.atomic(v) && length(v) == 1 && is.na(v)))
  }, names(f))

  cat("url: ", JIRA_BASE, "/browse/", SAMPLE_KEY, "\n\n", sep = "")
  cat(sprintf("%-26s %-34s %s\n", "FIELD ID", "NAME", "VALUE"))
  cat(strrep("-", 110), "\n")
  for (k in sort(populated)) {
    label <- if (!is.null(nm[[k]])) nm[[k]] else ""
    val   <- flat(f[[k]])
    if (!is.na(val) && nchar(val) > 46) val <- paste0(substr(val, 1, 45), "...")
    cat(sprintf("%-26s %-34s %s\n", k, substr(label, 1, 34), val))
  }

  cat("\n-- candidates for the ACCOUNT join (anything naming a customer) --\n")
  for (k in sort(populated)) {
    label <- if (!is.null(nm[[k]])) nm[[k]] else ""
    if (grepl("customer|account|organi[sz]ation|company|client",
              paste(k, label), ignore.case = TRUE)) {
      cat(sprintf("   %-26s %-34s %s\n", k, substr(label, 1, 34), flat(f[[k]])))
    }
  }
}

# ── 2. Distinct priority and status across OPEN CUSTX issues ─────────────────
# This is what the severity and status facets get built from, so the actual
# values matter more than the field names.
cat("\n\n================ OPEN CUSTX ISSUES ================\n")

jql <- "project = CUSTX AND statusCategory != Done ORDER BY created DESC"

search <- request(paste0(JIRA_BASE, "/rest/api/3/search/jql")) |>
  req_auth_basic(JIRA_EMAIL, JIRA_TOKEN) |>
  req_headers(Accept = "application/json") |>
  req_body_json(list(jql = jql, maxResults = 200,
                     fields = list("key", "summary", "status", "priority",
                                   "created", "updated", "assignee"))) |>
  req_error(is_error = function(r) FALSE) |>
  req_perform()

res <- show_resp(search, "JQL search")

if (!is.null(res)) {
  issues <- res$issues %||% list()
  cat("open issues returned: ", length(issues),
      "  (rollup custx_open summed to 33 at the last pipeline run)\n\n", sep = "")

  pick <- function(path) {
    vapply(issues, function(i) {
      v <- i$fields
      for (p in path) v <- if (is.null(v)) NULL else v[[p]]
      if (is.null(v)) "(none)" else as.character(v)
    }, character(1))
  }

  cat("PRIORITY values:\n")
  print(table(pick(c("priority", "name")), useNA = "ifany"))

  cat("\nSTATUS values:\n")
  print(table(pick(c("status", "name")), useNA = "ifany"))

  cat("\nSTATUS CATEGORY:\n")
  print(table(pick(c("status", "statusCategory", "name")), useNA = "ifany"))

  cat("\nfirst 5, as the tab would summarise them:\n")
  for (i in head(issues, 5)) {
    cat(sprintf("  %-12s %-9s %-16s %s\n",
                i$key,
                flat(i$fields$priority),
                substr(flat(i$fields$status), 1, 16),
                substr(i$fields$summary %||% "", 1, 60)))
  }
}

# ── 3. Custom field directory, filtered to likely-relevant names ─────────────
cat("\n\n================ CUSTOM FIELDS (filtered) ================\n")
flds <- show_resp(jira_get("/rest/api/3/field"), "field list")
if (!is.null(flds)) {
  keep <- Filter(function(f) grepl("customer|account|organi|company|sever|impact|region|csm|arr",
                                   f$name, ignore.case = TRUE), flds)
  if (length(keep) == 0) cat("  (nothing matched — paste the full table above instead)\n")
  for (f in keep) cat(sprintf("  %-26s %s\n", f$id, f$name))
}

cat("\n\n================ WHAT TO PASTE BACK ================\n")
cat("1. The field table for ", SAMPLE_KEY, " (or just the account-join candidates)\n", sep = "")
cat("2. The PRIORITY and STATUS value tables\n")
cat("3. Whether the customer field holds a Salesforce id, an exact account\n")
cat("   name, or free text -- that decides how escalations join to accounts\n\n")
