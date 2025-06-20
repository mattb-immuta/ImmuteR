#' pull current accounts from salesforce and enrich them.
#'
#' @param input_data salesforce account report
#' @param renewal.table output from enrich_renewal() function
#' @param looker_user_data_path path to google sheet with updated looker consumption data
#' @return an enriched account table with x,y,z.
#' @examples
#' example_optable("00OVH000005p6A92AI")
#' @export
#'
enrich_account.table<-function(input_data,renewal.table,looker_user_data_path){


  input_data=merge(input_data,renewal.table,all.x=TRUE)
  input_data=input_data[as.logical(1-duplicated(input_data$`Account ID`)),]
  input_data=input_data[as.logical(1-duplicated(input_data$`Account Name`)),]

  user_metrics=read_sheet(looker_user_data_path)
  user_metrics=data.table(`Account Name`=user_metrics$account_name,
                          licensed_users=user_metrics$salesforce_contracts.sum_of_all_contracted_user_types,
                          registered_users=user_metrics$full_user_pricing_counts+user_metrics$policy_owner_pricing_counts)

  input_data=merge(input_data,user_metrics,by="Account Name",all.x=TRUE)
  input_data=input_data[as.logical(1-duplicated(input_data$`Account Name`)),]
  input_data$utilization=input_data$registered_users/input_data$licensed_users

  input_data$utilization[is.infinite(input_data$utilization)]=NA
  input_data$engagement_risk=rep("low",length(input_data$`Date of last QBR/EBR`))
  input_data$engagement_risk[((Sys.Date()-input_data$`Date of last QBR/EBR`)>180) | is.na(input_data$`Date of last QBR/EBR`)]="high"
  return(input_data)}
