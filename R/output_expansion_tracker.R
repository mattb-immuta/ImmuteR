#' output updates to mb pipeline google sheet.
#'
#' @param op.list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op.list=op_list,mb_tracker)
#' @export
#'

output_expansion_tracker<-function(op_list,
                                             customer_expansion_tracker="https://docs.google.com/spreadsheets/d/1gI5N6FHArFMn65y1y_9pc1l9TUJGViHxYkzVXbHNk9Y/edit?gid=0#gid=0",
                                             fiscal.period=fiscal.period){


  temp.expansions=op_list$expansions %>%
    select(      `Account ID`,`Opportunity Name`,Region,`Opportunity Owner`,
                 `Fiscal Period`,`Close Date`,Amount,`Probability (%)`,`Stage`,
                 `Days in Current Stage`,

                 # Comments
                 `Next Step`,`RD Comment`,
                 # Risk
                 `Account Risk`,`Adoption Phase`,consumption_risk,licensed_users,
                 max_reg_users,engagement_risk,`Date of last EBR`,`Date of next EBR`)


  temp.accounts=op_list$accounts %>%
    select(`Account ID`,`max_consumption`,`Vitally Days as Customer`,onboarding_risk,
           `Next Use Case Deployment Date`,`Expected User Count`)

  output=merge(temp.expansions,temp.accounts,by="Account ID", all.x=TRUE)


  output=output %>%
    arrange(Region,desc(Amount)) %>%
    select( #basic renewal admin
      `Opportunity Name`,Region,`Opportunity Owner`,
      `Fiscal Period`,`Close Date`,Amount,`Probability (%)`,`Stage`,
      `Days in Current Stage`,

      # Comments
      `Next Step`,`RD Comment`,
      #
      `Account Risk`,
      # Onboarding Risk
      onboarding_risk,`Adoption Phase`,`Vitally Days as Customer`,

      # consumption risk
      consumption_risk, licensed_users,max_reg_users,

      #engagement risk
      engagement_risk,`Date of last EBR`, `Date of next EBR`)

  for(i in fiscal.period){
    temp=output %>%
      filter(`Fiscal Period`==i) %>%
      select(-`Fiscal Period`)
    sheet_write(temp,ss=customer_expansion_tracker,sheet=i)
  }


}
