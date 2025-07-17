#' output updates to mb pipeline google sheet.
#'
#' @param op.list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op.list=op_list,mb_tracker)
#' @export
#'

output_gsheet_mbpipeline<-function(op_list,mb_tracker){
  output=op_list$renewals %>%
    select( #basic renewal admin
      opportunity_url,`Opportunity Name`,Region,`Opportunity Owner`,
      `Customer Success Manager`,`Fiscal Period`,`Close Date`,Amount,`Churn Forecast`,`Priority`,

      # Comments
      `Next Step`,`RD Comment`,
      # Risk
      `Account Risk`,`Adoption Phase`,utilization_risk,licensed_users,
      registered_users,engagement_risk,`Date of last EBR`,`Date of next EBR`,
      `days_until_renewal`,expected_churn,company_churn_hazard) %>%
    arrange(desc(expected_churn))

  sheet_write(output,ss=mb_tracker,sheet='renewal_sheet')

  output=op_list$expansions %>%
    select( #basic renewal admin
      opportunity_url,`Opportunity Name`,Region,`Opportunity Owner`,
      `Fiscal Period`,`Close Date`,Amount,`Probability (%)`,

      # Comments
      `Next Step`,`RD Comment`,
      # Risk
      `Account Risk`,`Adoption Phase`,utilization_risk,licensed_users,
      registered_users,engagement_risk,`Date of last EBR`,`Date of next EBR`) %>%
    arrange(desc(Amount))

  sheet_write(output,ss=mb_tracker,sheet='expansion_sheet')


  temp=op_list$renewals %>%
    select(`Account ID`,utilization_risk,days_until_renewal)

  output=merge(op_list$accounts,temp,all.x=TRUE)

  output=output %>%
    select( #basic renewal admin
      `Account ID`,`Account Name`,Region,`Account Owner`,`Customer Success Manager`,
      Priority,`Account Risk`,


      # technical attributes
      `Is SaaS Customer`,`Customer on Unsupported Version`,Databricks,
      Snowflake,`Tech Integrations`,

      # Onboarding Risk
      `Adoption Phase`,`Vitally Days as Customer`,

      # utilization risk
      utilization_risk, licensed_users,registered_users, days_until_renewal,
      `Next Use Case Deployment Date`,`Expected User Count` ,

      #engagement risk
      engagement_risk,`Date of last EBR`, `Date of next EBR`) %>%
    arrange(`Account Name`)

  sheet_write(output,ss=mb_tracker,sheet='account_sheet')


  sheet_write(op_list$renewal_changes,ss=mb_tracker,sheet="renewal_changes")
  sheet_write(op_list$marketplace,ss=mb_tracker,sheet="marketplace_updates")

  output=op_list$renewals %>%
    arrange(desc(company_churn_hazard)) %>%
    filter(`Account Risk` != 'Closed Lost') %>%
    slice_head(n = 25)%>%
    select( #basic renewal admin
      `Opportunity Name`,Region,`Opportunity Owner`,
      `Fiscal Period`,`Close Date`,Amount,

      # Comments
      `Next Step`,`RD Comment`,
      # Account Risk
      `Account Risk`,`Adoption Phase`,`Vitally Days as Customer`,

      #utilization risk
      utilization_risk,licensed_users,
      registered_users,`Next Use Case Deployment Date`,`Expected User Count`,

      #engagement risk
      engagement_risk,`Date of last EBR`,`Date of next EBR`)

  sheet_write(output,ss=mb_tracker,sheet='critical_churn_risk')
}
