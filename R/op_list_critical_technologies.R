#' Per-account IAM and external catalog integrations
#'
#' Reads the locally maintained critical-technologies file and rolls it up to
#' one row per customer, for joining onto \code{op_list$accounts} by
#' \code{enrich_opportunities()}.
#'
#' The file lists customers per technology rather than technologies per
#' customer, so this inverts it:
#' \itemize{
#'   \item \code{iam} — every \code{technologies} entry whose category is
#'     "Identity and Access Management" (currently Okta), plus every group in
#'     \code{other_iams$iam_groups}. The "Customer-specific or unclassified
#'     IAM" group renders as "Other", since its identifiers aren't product
#'     names.
#'   \item \code{ext_catalog} — every \code{technologies} entry whose category
#'     is "External Data Catalog".
#' }
#' A customer in several entries gets them all, joined with " · " in file
#' order. Customer names in the file are Salesforce account names; customers
#' not in the file get \code{NA} after the join (rendered as "None").
#'
#' @param path Character. Path to the JSON file. Defaults to
#'   \code{"data/input/critical_technologies.json"} (relative to the project
#'   root).
#'
#' @return A data frame with columns \code{Account Name}, \code{iam},
#'   \code{ext_catalog}; zero rows if the file is missing or unreadable (with a
#'   warning).
#' @export
op_list_critical_technologies <- function(path = "data/input/critical_technologies.json") {
  empty <- data.frame(`Account Name` = character(0), iam = character(0),
                      ext_catalog = character(0), check.names = FALSE,
                      stringsAsFactors = FALSE)

  if (!file.exists(path)) {
    warning("Critical technologies file not found: ", path, call. = FALSE)
    return(empty)
  }
  ct <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      warning("Critical technologies file unreadable: ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )
  if (is.null(ct)) return(empty)

  # Long form: one row per (customer, field, label).
  pairs <- list()
  add <- function(field, label, customers) {
    customers <- unlist(customers)
    if (length(customers) == 0) return(invisible())
    pairs[[length(pairs) + 1]] <<- data.frame(
      `Account Name` = as.character(customers), field = field, label = label,
      check.names = FALSE, stringsAsFactors = FALSE)
  }

  category_field <- c("Identity and Access Management" = "iam",
                      "External Data Catalog"          = "ext_catalog")
  for (t in ct$technologies) {
    cat_name <- if (is.null(t$category)) NA_character_ else t$category
    if (cat_name %in% names(category_field)) {
      add(category_field[[cat_name]], t$name, t$customers)
    }
  }
  for (g in ct$other_iams$iam_groups) {
    label <- if (identical(g$name, "Customer-specific or unclassified IAM")) "Other" else g$name
    add("iam", label, g$customers)
  }
  if (length(pairs) == 0) return(empty)

  long <- dplyr::bind_rows(pairs) |> dplyr::distinct()
  roll <- function(f) {
    long |>
      dplyr::filter(field == f) |>
      dplyr::group_by(`Account Name`) |>
      dplyr::summarise(!!f := paste(unique(label), collapse = " · "),
                       .groups = "drop")
  }

  dplyr::full_join(roll("iam"), roll("ext_catalog"), by = "Account Name") |>
    as.data.frame(stringsAsFactors = FALSE)
}
