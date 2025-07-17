
#' compare two update objects and append renewal changes to list.
#'
#' @param new_list the updated list enriched list object.
#' @param old_list the enriched list object to compare and identify changes
#' @return appends a 'renewal_changes' data table to 'new_list'.
#' @examples
#' generate_diff_tables(new_op_list,op_list)
#' @export
enrich_list_object<-function(new_list,old_list){

  # develop change report for churn forecast
  new_list$renewals$`Churn Forecast`[is.na( new_list$renewals$`Churn Forecast`)]=0
  old_list$renewals$`Churn Forecast`[is.na( old_list$renewals$`Churn Forecast`)]=0
  renewal_change_df=merge(new_list$renewals,
                          old_list$renewals[,c('Opportunity ID','Account Risk','Churn Forecast')],
                          by='Opportunity ID',all.x=TRUE)

  renewal_changes_risk=renewal_change_df$`Account Risk.x`!=renewal_change_df$`Account Risk.y`
  message(paste("Renewal Changes:",sum(renewal_changes_risk)," changes to Account Risk..."))

  renewal_changes_forecast=renewal_change_df$`Churn Forecast.x`!=renewal_change_df$`Churn Forecast.y`
  message(paste(sum(renewal_changes_forecast)," changes to churn forecast..."))

  renewal_changes=renewal_changes_risk|renewal_changes_forecast
  message(paste(sum(renewal_changes)," total changes to renewals..."))

  output=data.table(`Opportunity Name`=renewal_change_df$`Opportunity Name`[renewal_changes],
                    `Fiscal Period`=renewal_change_df$`Fiscal Period`[renewal_changes],
                    `New Account Risk`=renewal_change_df$`Account Risk.x`[renewal_changes],
                    `Old Account Risk`=renewal_change_df$`Account Risk.y`[renewal_changes],
                    `New Forecast`=renewal_change_df$`Churn Forecast.x`[renewal_changes],
                    `Old Forecast`=renewal_change_df$`Churn Forecast.y`[renewal_changes])
  output$forecast_change=output$'New Forecast'-output$`Old Forecast`
  new_list[['renewal_changes']]=output
  message("renewal changes appended to list....")

  # develop change report for marketplace

  change_df=merge(new_list$marketplace,
                          old_list$marketplace[,c('Account Name','Marketplace Status')],
                          by='Account Name',all.x=TRUE)
  change_df$`Marketplace Status.x`[is.na(change_df$`Marketplace Status.x`)]="blank"
  change_df$`Marketplace Status.y`[is.na(change_df$`Marketplace Status.y`)]="blank"

  marketplace_changes=change_df$`Marketplace Status.x`!=change_df$`Marketplace Status.y`
  message(paste("Marketplace Changes:",sum(marketplace_changes)," accounts with status changes..."))


  output=data.table(`Account Name`=change_df$`Account Name`[marketplace_changes],
                    `Old Status`=change_df$`Marketplace Status.y`[marketplace_changes],
                    `New Status`=change_df$`Marketplace Status.x`[marketplace_changes])

  new_list[['marketplace_changes']]=output
  message("marketplace changes appended to list....")

  #generate field customer operations tracker data
  temp.expansions=new_list$expansions[,c("Account ID","Amount")]
  temp.expansions=temp.expansions %>%
    group_by(`Account ID`) %>%
    summarise(total_amount = sum(Amount),active_expansions = n())

  customer_ops_tracker=merge(new_list$renewals[,c("Account ID","account_url","opportunity_url","Amount","Close Date",
                                                  "Fiscal Period","Region",
                                                  "Account Risk","Stage","registered_users","licensed_users","utilization_risk",
                                                  "consumption_url",
                                                  "Next Step","RD Comment","Date of last EBR","Date of next EBR",
                                                  "running_estimate_hyperlink")],
                             temp.expansions,by="Account ID",all.x=TRUE)

  customer_ops_tracker=customer_ops_tracker[,c("account_url","opportunity_url","Amount","Close Date","Fiscal Period","Region",

                                               #account health
                                               "Account Risk","Stage","registered_users","licensed_users","utilization_risk",
                                               "consumption_url","active_expansions","total_amount",

                                               #account team engagement
                                               "Next Step","RD Comment","Date of last EBR","Date of next EBR",
                                               "running_estimate_hyperlink")]
  new_list[["customer_ops_tracker"]]=customer_ops_tracker
  message("customer_ops_tracker appended to list....")

  return(new_list)
}
