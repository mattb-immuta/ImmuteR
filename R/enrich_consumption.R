#' Aggregate consumption metrics from idex parquet exports.
#'
#' Optionally re-runs the idex CLI to refresh parquet files before reading them.
#' All parquet files are expected to live in \code{idex_path}.
#'
#' @param idex Logical. If \code{TRUE}, re-runs the idex CLI to pull fresh data
#'   before reading parquet files. Defaults to \code{TRUE}.
#' @param idex_path Character. Path to the directory containing idex parquet
#'   exports. Defaults to \code{"data/idex/"} relative to the working directory,
#'   which is correct when running from the project root.
#'
#' @return A named list with elements \code{users}, \code{queries},
#'   \code{datasources}, \code{policies}, and \code{metrics_df} (a joined
#'   summary data frame keyed on \code{Account Name}).
#' @export
enrich_consumption <- function(idex = TRUE,
                               idex_path = "data/idex/") {

  # Normalise: ensure path ends with a separator
  idex_path <- file.path(idex_path, "")

  message(".. enrich_consumption initiated")

  if (idex) {
    system(paste0("uvx idex all -p ", idex_path))
    system(paste0("uvx idex -f ", idex_path, "users.parquet metrics -m 3 users"))
    system(paste0("uvx idex -f ", idex_path, "queries.parquet metrics -m 3 queries"))
    system(paste0("uvx idex -f ", idex_path, "marketplace.parquet metrics -m 3 marketplace"))
    system(paste0("uvx idex -f ", idex_path, "datasources.parquet metrics -m 3 datasources"))
    system(paste0("uvx idex -f ", idex_path, "data-policies.parquet metrics -m 3 data-policies"))
    system(paste0("uvx idex -f ", idex_path, "subscription-policies.parquet metrics -m 3 subscription-policies"))
  }

  metrics_list <- list()

  # Users -----------------------------------------------------------------------
  metrics_list[["users"]] <- arrow::read_parquet(paste0(idex_path, "users.parquet")) %>%
    dplyr::filter(instance_type == "Production")

  users_df <- metrics_list$users %>%
    dplyr::group_by(account_name) %>%
    dplyr::summarize(
      licensed_users   = max(salesforce_contracts.sum_of_all_contracted_user_types),
      mean_reg_users   = mean(full_user_pricing_counts + policy_owner_pricing_counts),
      median_reg_users = median(full_user_pricing_counts + policy_owner_pricing_counts),
      max_reg_users    = max(full_user_pricing_counts + policy_owner_pricing_counts),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      mean_consumption = mean_reg_users / licensed_users,
      max_consumption  = max_reg_users  / licensed_users
    )

  # Queries ---------------------------------------------------------------------
  metrics_list[["queries"]] <- arrow::read_parquet(paste0(idex_path, "queries.parquet")) %>%
    dplyr::filter(instance_type == "Production")

  queries_df <- metrics_list$queries %>%
    dplyr::group_by(account_name) %>%
    dplyr::summarize(
      mean_queries   = mean(queries_success),
      median_queries = median(queries_success),
      .groups = "drop"
    )

  # Datasources -----------------------------------------------------------------
  metrics_list[["datasources"]] <- arrow::read_parquet(paste0(idex_path, "datasources.parquet")) %>%
    dplyr::filter(instance_type == "Production")

  datasources_df <- metrics_list$datasources %>%
    dplyr::group_by(account_name) %>%
    dplyr::summarize(
      mean_datasources   = mean(data_sources),
      median_datasources = median(data_sources),
      .groups = "drop"
    )

  # Policies --------------------------------------------------------------------
  metrics_list[["policies"]] <- arrow::read_parquet(paste0(idex_path, "subscription-policies.parquet")) %>%
    dplyr::filter(instance_type == "Production")

  policies_df <- metrics_list$policies %>%
    dplyr::group_by(account_name) %>%
    dplyr::summarize(
      global_policies_avail  = mean(global_policies_available_subscription),
      global_policies_staged = median(global_policies_staged_subscription),
      .groups = "drop"
    )

  # Join all metrics into a single summary frame --------------------------------
  metrics_list[["metrics_df"]] <- purrr::reduce(
    list(users_df, queries_df, datasources_df, policies_df),
    dplyr::full_join,
    by = "account_name"
  ) %>%
    dplyr::rename(`Account Name` = account_name)

  message(".. enrich_consumption complete")

  return(metrics_list)
}
