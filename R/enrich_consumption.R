#' aggregate consumption metrics.
#'
#'
#' @return a data frame which extracts summary statistics from all production instances by Account Name.
#' @examples
#' usage()
#' @export


enrich_consumption<-function(idex=TRUE){
  message(".. enrich_consumption initiated /n")
  # message(".. bug finder /n")

  if(idex){
    system("uvx idex all -p /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/")
    system("uvx idex -f /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/users.parquet metrics -m 3 users")
    system("uvx idex -f /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/queries.parquet metrics -m 3 queries")
    system("uvx idex -f /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/marketplace.parquet metrics -m 3 marketplace")
    system("uvx idex -f /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/datasources.parquet metrics -m 3 datasources")
    system("uvx idex -f /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/data-policies.parquet metrics -m 3 data-policies")
    system("uvx idex -f /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/subscription-policies.parquet metrics -m 3 subscription-policies")
  }

  metrics_list=list()
  metrics_list[["users"]] <- read_parquet("/Users/matt.benigni/Documents/projects/ImmuteR/data/idex/users.parquet") %>%
          filter(instance_type=="Production")

  users_df<-metrics_list$users %>%
              group_by(account_name) %>%
              summarize(
                licensed_users = max(salesforce_contracts.sum_of_all_contracted_user_types),
                mean_reg_users = mean(full_user_pricing_counts+policy_owner_pricing_counts),
                median_reg_users = median(full_user_pricing_counts+policy_owner_pricing_counts),
                max_reg_users = max(full_user_pricing_counts+policy_owner_pricing_counts)
            )

          users_df$mean_consumption=users_df$mean_reg_users/users_df$licensed_users
          users_df$max_consumption=users_df$max_reg_users/users_df$licensed_users


  metrics_list[["queries"]] <- read_parquet("/Users/matt.benigni/Documents/projects/ImmuteR/data/idex/queries.parquet") %>%
          filter(instance_type=="Production")

  queries_df<- metrics_list$queries %>%
            group_by(account_name) %>%
            summarize(
              mean_queries = mean(queries_success),
              median_queries = median(queries_success)
            )



  metrics_list[["datasources"]] <- read_parquet("/Users/matt.benigni/Documents/projects/ImmuteR/data/idex/datasources.parquet") %>%
          filter(instance_type=="Production")

  datasources_df<-metrics_list$datasources %>%
            group_by(account_name) %>%
            summarize(
              mean_queries = mean(data_sources),
              median_queries = median(data_sources)
            )



  metrics_list[["policies"]] <- read_parquet("/Users/matt.benigni/Documents/projects/ImmuteR/data/idex/subscription-policies.parquet") %>%
          filter(instance_type=="Production")
  policies_df<- metrics_list$policies %>%
            group_by(account_name) %>%
            summarize(
              global_policies_avail = mean(global_policies_available_subscription),
              global_policies_staged = median(global_policies_staged_subscription)
            )


  metrics_df_list=list(users_df,queries_df,datasources_df,policies_df)


  metrics_list$metrics_df=reduce(metrics_df_list,full_join,by="account_name")
  names(metrics_list$metrics_df)[1]="Account Name"

  message(".. enrich_consumption complete /n")

  return(metrics_list)}






