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
                               looker_user_data_path){


  account.table=merge(account.table,renewal.table,all.x=TRUE)
  account.table=account.table[as.logical(1-duplicated(account.table$`Account ID`)),]
  account.table=account.table[as.logical(1-duplicated(account.table$`Account Name`)),]

  user_metrics=read_sheet(looker_user_data_path)
  user_metrics=data.table(`Account Name`=user_metrics$account_name,
                          licensed_users=user_metrics$salesforce_contracts.sum_of_all_contracted_user_types,
                          registered_users=user_metrics$full_user_pricing_counts+user_metrics$policy_owner_pricing_counts)

  account.table=merge(account.table,user_metrics,by="Account Name",all.x=TRUE)
  account.table=account.table[as.logical(1-duplicated(account.table$`Account Name`)),]
  account.table$utilization=account.table$registered_users/account.table$licensed_users

  account.table$utilization[is.infinite(account.table$utilization)]=NA
  account.table$engagement_risk=rep("low",length(account.table$`Date of last QBR/EBR`))
  account.table$engagement_risk[((Sys.Date()-account.table$`Date of last QBR/EBR`)>180) | is.na(account.table$`Date of last QBR/EBR`)]="high"
  account.table$onboarding_risk=rep("moderate",length(account.table$engagement_risk))
  green_logical=account.table$`Adoption Phase` %in% c("Use Case Pipelining","MVP Complete","90% Adoption")
  red_logical=!green_logical & account.table$`Vitally Days as Customer`> 365*2
  account.table$onboarding_risk[green_logical]="low"
  account.table$onboarding_risk[red_logical]="high"

  message(".. account data enriched /n")
  return(account.table)}
