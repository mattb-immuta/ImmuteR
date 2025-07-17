
#' compare two update objects and append renewal changes to list.
#'
#' @param new_list the updated list enriched list object.
#' @param old_list the enriched list object to compare and identify changes
#' @return appends a 'renewal_changes' data table to 'new_list'.
#' @examples
#' generate_diff_tables(new_op_list,op_list)
#' @export
generate_diff_tables<-function(new_list,old_list){
  #new_list=new_op_list
  #old_list=op_list
  new_list$renewals$`Churn Forecast`[is.na( new_list$renewals$`Churn Forecast`)]=0
  old_list$renewals$`Churn Forecast`[is.na( old_list$renewals$`Churn Forecast`)]=0

  renewal_changes_risk=new_list$renewals$`Account Risk`!=old_list$renewals$`Account Risk`
  message(paste("Renewal Changes:",sum(renewal_changes_risk)," changes to Account Risk..."))
  renewal_changes_forecast=new_list$renewals$`Churn Forecast`!=old_list$renewals$`Churn Forecast`
  message(paste(sum(renewal_changes_forecast)," changes to churn forecast..."))

  renewal_changes=renewal_changes_risk|renewal_changes_forecast
  message(paste(sum(renewal_changes)," total changes to renewals..."))

  output=data.table(`Opportunity Name`=new_list$renewals$`Opportunity Name`[renewal_changes],
                    `Fiscal Period`=new_list$renewals$`Fiscal Period`[renewal_changes],
                    `New Account Risk`=new_list$renewals$`Account Risk`[renewal_changes],
                    `Old Account Risk`=old_list$renewals$`Account Risk`[renewal_changes],
                    `New Forecast`=new_list$renewals$`Churn Forecast`[renewal_changes],
                    `Old Forecast`=old_list$renewals$`Churn Forecast`[renewal_changes])
  output$forecast_change=output$'New Forecast'-output$`Old Forecast`
  new_list[['renewal_changes']]=output

  #provision_changes_status=new_list$marketplace$`Marketplace Status`!=old_list$marketplace_changes$`Marketplace Status`
  #message(paste("Provision Changes:",sum(provision_changes_status)," changes to status..."))
  #provision_changes_timeline=new_list$marketplace$`Est. Date to Move Out of Stage (MP)`!=old_list$marketplace_changes$`Est. Date to Move Out of Stage (MP)`
  #message(paste(sum(provision_changes_timeline)," changes to timeline..."))

  #renewal_changes=renewal_changes_risk|renewal_changes_forecast
  #message(paste(sum(renewal_changes)," total changes to renewals..."))

  #output=data.table(`Opportunity Name`=new_list$renewals$`Opportunity Name`[renewal_changes],
  #                  `Fiscal Period`=new_list$renewals$`Fiscal Period`[renewal_changes],
  #                  `New Account Risk`=new_list$renewals$`Account Risk`[renewal_changes],
  #                  `Old Account Risk`=old_list$renewals$`Account Risk`[renewal_changes],
  #                  `New Forecast`=new_list$renewals$`Churn Forecast`[renewal_changes],
  #                  `Old Forecast`=old_list$renewals$`Churn Forecast`[renewal_changes])
  #output$forecast_change=output$'New Forecast'-output$`Old Forecast`
  #new_list[['renewal_changes']]=output

  return(new_list)
}
