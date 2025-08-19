#' output updates to mb pipeline google sheet.
#'
#' @param op.list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op.list=op_list,mb_tracker)
#' @export
#'

output_gsheet_customer_ops_tracker<-function(op_list,
                                             customer_ops_tracker,
                                             fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027")){


  temp.renewals=op_list$renewals %>%
    select(      `Account ID`,`Opportunity Name`,Region,`Opportunity Owner`,
                 `Customer Success Manager`,`Fiscal Period`,`Close Date`,Amount,`Churn Forecast`,`Priority`,

                 # Comments
                 `Next Step`,`RD Comment`,
                 # Risk
                 `Account Risk`,`Adoption Phase`,utilization_risk,licensed_users,
                 registered_users,engagement_risk,`Date of last EBR`,`Date of next EBR`,
                 `days_until_renewal`,expected_churn,company_churn_hazard)
  #message("..renewal names temp:")
  #message(paste(names(temp),", ",sep=""))
  temp.accounts=op_list$accounts %>%
    select(`Account ID`,`utilization`,`Vitally Days as Customer`,onboarding_risk,
           `Next Use Case Deployment Date`,`Expected User Count`)

  output=merge(temp.renewals,temp.accounts,by="Account ID", all.x=TRUE)
  #message("..merged names output:")
  #message(paste(names(output),", ",sep=""))
  output=output %>%
    arrange(Region,desc(company_churn_hazard)) %>%
    select( #basic renewal admin
      `Opportunity Name`,Region,`Opportunity Owner`,
      `Customer Success Manager`,`Close Date`,Amount,`Churn Forecast`,
      `Fiscal Period`,

      # Comments
      `Next Step`,`RD Comment`,
      #
      `Account Risk`,
      # Onboarding Risk
      onboarding_risk,`Adoption Phase`,`Vitally Days as Customer`,

      # utilization risk
      utilization_risk, licensed_users,registered_users, days_until_renewal,
      `Next Use Case Deployment Date`,`Expected User Count` ,

      #engagement risk
      engagement_risk,`Date of last EBR`, `Date of next EBR`)

  for(i in fiscal.period){
    temp=output %>%
      filter(`Fiscal Period`==i) %>%
      select(-`Fiscal Period`)
    sheet_write(temp,ss=customer_ops_tracker,sheet=i)
  }


}
