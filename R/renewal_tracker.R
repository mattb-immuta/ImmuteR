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
                                             fiscal.period=c("Q4-2026","Q1-2027","Q2-2027","Q3-2027","Q4-2027")){



  temp.accounts=op_list$accounts %>%
    select(`Account ID`,`max_consumption`,`Vitally Days as Customer`,onboarding_risk,
           `Next Use Case Deployment Date`,`Expected User Count`)

  output=merge(op_list$customer_ops_tracker,temp.accounts,by="Account ID", all.x=TRUE)
  #message("..merged names output:")
  #message(paste(names(output),", ",sep=""))
  output=output %>%
    arrange(Region,desc(company_churn_hazard)) %>%
    select( #basic renewal admin
      company_churn_hazard,`Opportunity Name`,Region,`Opportunity Owner`,
      `Customer Success Manager`,`Close Date`,Amount,`Churn Forecast`,
      `Fiscal Period`,

      # Comments
      `Next Step`,`RD Comment`,
      #
      `Account Risk`,
      # Onboarding Risk
      onboarding_risk,`Adoption Phase`,`Vitally Days as Customer`,

      # consumption risk
      consumption_risk, licensed_users,max_reg_users, days_until_renewal,
      `Next Use Case Deployment Date`,`Expected User Count` ,

      #engagement risk
      engagement_risk,`Date of last EBR`, `Date of next EBR`)

  for(i in fiscal.period){
    temp=output %>%
      filter(`Fiscal Period`==i) %>%
      select(-`Fiscal Period`)
    sheet_delete(ss=customer_ops_tracker,sheet=i)
    sheet_write(temp,ss=customer_ops_tracker,sheet=i)
  }


}
