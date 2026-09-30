# Confirm dashboard 995's "Connections Adoption by Instance" tile before
# trusting op_list_connections() against live data.
#
# op_list_connections() resolves the tile's columns by shape rather than by
# full name, because Looker prefixes each column with the view it came from
# and that prefix differs between explores. That makes it robust to a rename
# and silent when it guesses wrong, so this script prints what it actually
# matched. Run it once; if the mapping looks right, the pipeline needs nothing
# further.
#
#   devtools::load_all()
#   source("working_scratch/discover_connections_995.R")

devtools::load_all()

tok  <- looker_auth()
dash <- looker_dashboard(995, tok)

cat("\n== Tiles on dashboard 995 ==\n")
titles <- vapply(dash$dashboard_elements,
                 function(e) ImmuteR:::.nz(e$title, "<untitled>"), character(1))
print(titles)

tile <- looker_tile_data(995, "connections adoption by instance",
                         dash = dash, token = tok)

if (is.null(tile) || nrow(tile) == 0) {
  stop("Tile matched nothing, or returned no rows. Pick the right title from ",
       "the list above and pass it as `pattern`.")
}

cat("\n== Columns on the tile ==\n")
print(names(tile))

cat("\n== What op_list_connections() will match ==\n")
print(c(
  account  = ImmuteR:::.looker_account_col(tile),
  upgraded = ImmuteR:::.looker_col(tile, c("upgraded.*data sources.*count",
                                           "upgraded.*count", "upgraded")),
  total    = ImmuteR:::.looker_col(tile, c("data sources count", "data sources"),
                                   exclude = "upgraded"),
  instance = ImmuteR:::.looker_col(tile, c("instance name", "instances name", "instance$")),
  type     = ImmuteR:::.looker_col(tile, c("instance type"))
))

cat("\n== First rows ==\n")
print(utils::head(as.data.frame(tile), 10))

# Reshape the tile we already have rather than fetching it again.
conn <- op_list_connections(tile = tile)

cat("\n== Accounts with more than one Production instance ==\n")
print(utils::head(conn[grepl(",", conn$connections), ], 10))

cat("\n== Accounts with nothing upgraded ==\n")
print(utils::head(conn[conn$connections_upgraded == 0, ], 10))

cat("\n== Summary ==\n")
cat(nrow(conn), "accounts;",
    sum(conn$connections_upgraded == 0, na.rm = TRUE), "with nothing upgraded;",
    sum(grepl(",", conn$connections)), "with multiple Production instances\n")

# Keep a copy so the reshaping can be re-run offline:
#   readr::write_csv(tile, "data/looker/dashboard_995_connections.csv")
#   op_list_connections(tile = readr::read_csv("data/looker/dashboard_995_connections.csv"))
