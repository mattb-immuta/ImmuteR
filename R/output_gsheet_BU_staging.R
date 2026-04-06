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
                                             fiscal.period=c("Q4-2026","Q1-2027","Q2-2027","Q3-2027","Q4-2027")){


  output=op_list$renewals %>%
    select(      `Account ID`,`Account Name`,`Opportunity Name`,Region,`Opportunity Owner`,
                 `Customer Success Manager`,`Close Date`,`Current Contract End Date`,`Account Risk`,
                 `Churn Quarter`, Amount,`Churn Forecast`,

                 # Comments
                 `Next Step`)


  output=output %>%
    arrange(`Account Name`) %>%
    select( #basic renewal admin
      `Account Name`,`Close Date`,`Current Contract End Date`,`Customer Success Manager`,`Opportunity Owner`,`Account Risk`,Amount,`Churn Forecast`,`Next Step`,
      `Churn Quarter`)

  for(i in fiscal.period){
    temp=output %>%
      filter(`Churn Quarter`==i) %>%
    select(`Account Name`,`Close Date`,`Current Contract End Date`,`Customer Success Manager`,`Opportunity Owner`,`Account Risk`,Amount,`Churn Forecast`,`Next Step`)
    sheet_delete(ss=BU_staging,sheet=i)
    sheet_write(temp,ss=BU_staging,sheet=i)
  }

  sheet_write(op_list$renewal_changes,ss=BU_staging,sheet="change log")
}
