#' output updates to mb pipeline google sheet.
#'
#' @param op_list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op_list=op_list,mb_tracker)
#' @export
#'

output_customer_ops_tracker<-function(op_list,
                                             customer_ops_tracker,
                                             fiscal.period){



  temp.accounts=op_list$accounts %>%
    select(`Account ID`,`max_consumption`,`Vitally Days as Customer`,onboarding_risk,
           `Next Use Case Deployment Date`,`Expected User Count`)

  output=merge(op_list$customer_ops_tracker,temp.accounts,by="Account ID", all.x=TRUE)
  #message("..merged names output:")
  #message(paste(names(output),", ",sep=""))
  output=output %>%
    select( #basic renewal admin
      `Opportunity Name`,Region,`Opportunity Owner`,
      `Customer Success Manager`,`Close Date`,Amount,`Churn Forecast`,
      `Fiscal Period`,

      # Comments
      `Next Step`,
      #
      `Account Risk`,
      # Onboarding Risk
      onboarding_risk,`Adoption Phase`,`Vitally Days as Customer`,

      # consumption risk
      licensed_users,max_reg_users,
      `Next Use Case Deployment Date`,`Expected User Count` ,

      #engagement risk
      `Date of last EBR`, `Date of next EBR`)

  for(i in fiscal.period){
    temp=output %>%
      filter(`Fiscal Period`==i) %>%
      select(-`Fiscal Period`)
    sheet_delete(ss=customer_ops_tracker,sheet=i)
    sheet_write(temp,ss=customer_ops_tracker,sheet=i)
  }


}
