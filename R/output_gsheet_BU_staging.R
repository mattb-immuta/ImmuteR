#' output updates to mb pipeline google sheet.
#'
#' @param op_list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op_list=op_list,mb_tracker)
#' @export
#'

output_gsheet_BU_staging<-function(op_list,
                                             BU_staging,
                                             fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027")){


  output=op_list$renewals %>%
    select(      `Account ID`,`Account Name`,`Opportunity Name`,Region,`Opportunity Owner`,
                 `Customer Success Manager`,`Close Date`,`Current Contract End Date`,`Account Risk`,
                 churn_forecast_period, Amount,`Churn Forecast`,

                 # Comments
                 `Next Step`,`RD Comment`,expected_churn,company_churn_hazard)


  output=output %>%
    arrange(Region,desc(company_churn_hazard)) %>%
    select( #basic renewal admin
      `Account Name`,`Close Date`,`Current Contract End Date`,`Customer Success Manager`,`Opportunity Owner`,`Account Risk`,Amount,`Churn Forecast`,`Next Step`,
      churn_forecast_period)

  for(i in fiscal.period){
    temp=output %>%
      filter(churn_forecast_period==i) %>%
    select(`Account Name`,`Close Date`,`Current Contract End Date`,`Customer Success Manager`,`Opportunity Owner`,`Account Risk`,Amount,`Churn Forecast`,`Next Step`)
    sheet_delete(ss=BU_staging,sheet=i)
    sheet_write(temp,ss=BU_staging,sheet=i)
  }

  sheet_write(op_list$renewal_changes,ss=BU_staging,sheet="change log")
}
