#' pull current accounts from salesforce and enrich them.
#'
#' @param account.table salesforce account report
#' @param renewal.table output from enrich_renewal() function
#' @param looker_user_data_path path to google sheet with updated looker consumption data
#' @return an enriched account table with x,y,z.
#' @examples
#' example_optable("00OVH000005p6A92AI")
#' @export
#'
enrich_account.table<-function(account.table,renewal.table,
                               idex){

  if(idex){system("uvx idex all -p /Users/matt.benigni/Documents/projects/ImmuteR/data/idex/")}
  renewal.table = renewal.table %>%
    group_by(`Account ID`) %>%
    slice_max(order_by=Amount, n=1, with_ties=FALSE) %>%
    ungroup()%>%
    distinct() %>%
    select(`Account ID`,Priority,`Account Risk`,`Adoption Phase`,`Vitally Days as Customer`,opportunity_url,account_url,consumption_url )



  account.table=merge(account.table,renewal.table,by= "Account ID",all.x=TRUE)


  #load idex user metrics and keep the highest return with respect to registered users
  user_metrics=read_parquet("/Users/matt.benigni/Documents/projects/ImmuteR/data/idex/users.parquet")
  user_metrics=data.table(`Account Name`=user_metrics$account_name,
                          licensed_users=user_metrics$salesforce_contracts.sum_of_all_contracted_user_types,
                          registered_users=user_metrics$full_user_pricing_counts+user_metrics$policy_owner_pricing_counts) %>%
    group_by(`Account Name`) %>%
    slice_max(order_by=registered_users, n=1, with_ties=FALSE) %>%
    ungroup()%>%
    distinct()

  account.table=merge(account.table,user_metrics,by="Account Name",all.x=TRUE)


  #load idex user metrics and keep the highest return with respect to registered users
  query_metrics=read_parquet("/Users/matt.benigni/Documents/projects/ImmuteR/data/idex/queries.parquet")
  names(query_metrics)[4]="Account Name"
  query_metrics=query_metrics %>%
    select(`Account Name`,queries_failures, queries_success,queries_native) %>%
    group_by(`Account Name`) %>%
    slice_max(order_by=queries_success, n=1, with_ties=FALSE) %>%
    ungroup() %>%
    distinct()

  account.table=merge(account.table,query_metrics,by="Account Name",all.x=TRUE)

  # quantify utilization
  account.table$utilization=account.table$registered_users/account.table$licensed_users
  account.table$utilization[is.na(account.table$utilization)]=0

  # develop engagement risk
  account.table$engagement_risk=rep("low",length(account.table$`Date of last EBR`))
  account.table$engagement_risk[((Sys.Date()-account.table$`Date of last EBR`)>180) | is.na(account.table$`Date of last EBR`)]="high"

  # develop onboarding risk
  account.table$onboarding_risk=rep("moderate",length(account.table$engagement_risk))
    green_logical=account.table$`Adoption Phase` %in% c("Use Case Pipelining","MVP Complete","90% Adoption")
    red_logical=!green_logical & account.table$`Vitally Days as Customer`> 365*2
  account.table$onboarding_risk[green_logical]="low"
  account.table$onboarding_risk[red_logical]="high"

  message(".. account data enriched /n")
  return(account.table)}
