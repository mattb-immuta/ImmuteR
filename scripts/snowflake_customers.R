# Snowflake customers -> op_list_inspector "Snowflake Customers" tab
#
# Runs the Looker dashboard 474 tile(s) that carry
# "[Usage Metrics] Data Sources Count: Snowflake", keeps every account with at
# least one instance (any type: Production, Dev, Test, ...) where that count is
# > 0, attaches the CSM from op_list$accounts and writes one row per customer.
#
# Run from the project root:
#   devtools::load_all(); source("scripts/snowflake_customers.R")
#
# Uses `op_list` from the session if there is one, otherwise loads the most
# recent data/opportunities_<date>.RData snapshot (for the CSM lookup only).

library(googlesheets4)

DASHBOARD_ID <- 474
SNOW_COL     <- "[Usage Metrics] Data Sources Count: Snowflake"
OP_LIST_INSPECTOR <- "https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?gid=1015395146#gid=1015395146"
SHEET_NAME   <- "Snowflake Customers"

# ── 1. op_list for the CSM lookup ────────────────────────────────────────────
if (!exists("op_list")) {
  snaps <- list.files("data", pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$")
  if (!length(snaps)) stop("No op_list in session and no data/opportunities_*.RData snapshot found.")
  latest <- max(snaps)
  message(".. loading ", latest, " for CSM lookup")
  load(file.path("data", latest))
}

# ── 2. Pull dashboard 474 ────────────────────────────────────────────────────
tok  <- looker_auth()
dash <- looker_dashboard(DASHBOARD_ID, tok)
els  <- dash$dashboard_elements

# Run tiles whose LookML fields mention Snowflake first; if none do (or none
# return the column), fall back to running every tile.
has_snow_field <- vapply(els, function(e) {
  f <- unlist(.tile_query(e)$fields)
  any(grepl("snowflake", f, ignore.case = TRUE))
}, logical(1))

run_tiles <- function(tiles) {
  out <- list()
  for (el in tiles) {
    df <- tryCatch(run_looker_dashboard_tile(el, tok), error = function(e) NULL)
    if (!is.null(df) && SNOW_COL %in% names(df)) {
      message(".. using tile '", .nz(el$title, "<untitled>"), "' (", nrow(df), " rows)")
      out[[length(out) + 1]] <- df
    }
  }
  out
}

hits <- run_tiles(els[has_snow_field])
if (!length(hits)) hits <- run_tiles(els[!has_snow_field])
if (!length(hits)) {
  stop("No tile on dashboard ", DASHBOARD_ID, " returned a '", SNOW_COL, "' column. ",
       "Inspect with looker_dashboard_elements(", DASHBOARD_ID, ").")
}
raw <- hits[[1]]   # first tile carrying the column

acct_col <- .looker_account_col(raw)
if (is.na(acct_col)) stop("Tile has no account-name column: ", paste(names(raw), collapse = ", "))
type_col <- .looker_col(raw, c("Instance Type$"))

# ── 3. Any instance with Snowflake > 0 ───────────────────────────────────────
snow <- data.frame(
  account   = trimws(as.character(raw[[acct_col]])),
  inst_type = if (is.na(type_col)) NA_character_ else as.character(raw[[type_col]]),
  snowflake = suppressWarnings(as.numeric(raw[[SNOW_COL]])),
  stringsAsFactors = FALSE
)
snow <- snow[!is.na(snow$snowflake) & snow$snowflake > 0 & nzchar(snow$account), ]
if (!nrow(snow)) stop("No instance on the tile has '", SNOW_COL, "' > 0.")

by_acct <- do.call(rbind, lapply(split(snow, snow$account), function(d) data.frame(
  `Account Name`               = d$account[1],
  `Snowflake Instances`        = nrow(d),
  `Instance Types`             = paste(sort(unique(na.omit(d$inst_type))), collapse = ", "),
  `Snowflake Data Sources`     = sum(d$snowflake),
  check.names = FALSE
)))

# ── 4. Attach CSM (exact name, then normalised name) ─────────────────────────
accts <- op_list$accounts
idx <- match(by_acct$`Account Name`, trimws(accts$`Account Name`))
miss <- is.na(idx)
idx[miss] <- match(.pb_normalize_name(by_acct$`Account Name`[miss]),
                   .pb_normalize_name(accts$`Account Name`))

out <- data.frame(
  `Account Name`             = by_acct$`Account Name`,
  CSM                        = as.character(accts$`Customer Success Manager`)[idx],
  `Snowflake Data Sources`   = by_acct$`Snowflake Data Sources`,
  `Snowflake Instances`      = by_acct$`Snowflake Instances`,
  `Instance Types`           = by_acct$`Instance Types`,
  `Matched in op_list`       = !is.na(idx),
  check.names = FALSE
)
out <- out[order(out$CSM, out$`Account Name`, na.last = TRUE), ]
rownames(out) <- NULL

message(".. ", nrow(out), " Snowflake customers (", sum(!out$`Matched in op_list`),
        " not matched to op_list$accounts, CSM blank)")

# ── 5. Write to the inspector ────────────────────────────────────────────────
sheet_write(out, ss = OP_LIST_INSPECTOR, sheet = SHEET_NAME)
message(".. wrote '", SHEET_NAME, "' to op_list_inspector")
