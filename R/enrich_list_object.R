
#' compare two update objects and append renewal changes to list.
#'
#' @param new_list the updated list enriched list object.
#' @param old_list the enriched list object to compare and identify changes
#' @return appends a 'renewal_changes' data table to 'new_list'.
#' @examples
#' generate_diff_tables(new_op_list,op_list)
#' @export
enrich_list_object<-function(new_list,old_list){


  new_list$renewals$`Churn Forecast`[is.na( new_list$renewals$`Churn Forecast`)]=0
  old_list$renewals$`Churn Forecast`[is.na( old_list$renewals$`Churn Forecast`)]=0

  renewal_changes_risk=new_list$renewals$`Adoption Risk`!=old_list$renewals$`Adoption Risk`
  message(paste("Renewal Changes:",sum(renewal_changes_risk)," changes to adoption risk..."))
  renewal_changes_forecast=new_list$renewals$`Churn Forecast`!=old_list$renewals$`Churn Forecast`
  message(paste(sum(renewal_changes_forecast)," changes to churn forecast..."))

  renewal_changes=renewal_changes_risk|renewal_changes_forecast
  message(paste(sum(renewal_changes)," total changes to renewals..."))

  output=data.table(`Opportunity Name`=new_list$renewals$`Opportunity Name`[renewal_changes],
                    `Fiscal Period`=new_list$renewals$`Fiscal Period`[renewal_changes],
                    `New Adoption Risk`=new_list$renewals$`Adoption Risk`[renewal_changes],
                    `Old Adoption Risk`=old_list$renewals$`Adoption Risk`[renewal_changes],
                    `New Forecast`=new_list$renewals$`Churn Forecast`[renewal_changes],
                    `Old Forecast`=old_list$renewals$`Churn Forecast`[renewal_changes])
  output$forecast_change=output$'New Forecast'-output$`Old Forecast`
  new_list[['renewal_changes']]=output
  message("renewal changes appended to list....")


  #generate bottoms up data
  temp.expansions=new_list$expansions[,c("Account ID","Amount")]
  temp.expansions=temp.expansions %>%
    group_by(`Account ID`) %>%
    summarise(total_amount = sum(Amount),active_expansions = n())

  customer_ops_tracker=merge(new_list$renewals[,c("Account ID","account_url","opportunity_url","Amount","Close Date",
                                                  "Fiscal Period","Region",
                                                  "Adoption Risk","Stage","registered_users","licensed_users","utilization_risk",
                                                  "consumption_url",
                                                  "Next Step","RD Comment","Date of last QBR/EBR","Date of next QBR/EBR",
                                                  "re_hyperlink")],
                             temp.expansions,by="Account ID",all.x=TRUE)

  customer_ops_tracker=customer_ops_tracker[,c("account_url","opportunity_url","Amount","Close Date","Fiscal Period","Region",

                                               #account health
                                               "Adoption Risk","Stage","registered_users","licensed_users","utilization_risk",
                                               "consumption_url","active_expansions","total_amount",

                                               #account team engagement
                                               "Next Step","RD Comment","Date of last QBR/EBR","Date of next QBR/EBR",
                                               "re_hyperlink")]
  new_list[["customer_ops_tracker"]]=customer_ops_tracker
  message("customer_ops_tracker appended to list....")

  return(new_list)
}
