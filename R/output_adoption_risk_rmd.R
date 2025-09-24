#' develop an html table for BU renewals.
#'
#' @param op.list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op.list=op_list,mb_tracker)
#' @export
#'

output_adoption_risk_rmd<-function(op_list){
  df=op_list$renewals %>%
    mutate(links = paste0('<a href="', consumption_url, '" target="_blank">',"utilization db",
                          '</a> \n <a href="',running_estimate_url,'" target="_blank">',"success plan",
                          '</a> \n <a href="',"broken-replace",'" target="_blank">',"support summary", '</a>')) %>%
    select(`Account Name`,Region, Amount,`Churn Forecast`, `Close Date`,`Account Risk`,consumption_risk,onboarding_risk,links)


  output=list()
  output[["plot"]]<-reactable(df,filterable = TRUE, resizable = TRUE, bordered=TRUE, minRows = 10,
            columns = list(
              `Close Date`=colDef(format = colFormat(date = TRUE)),
              `Churn Forecast` = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                                        aggregate = "sum"),
              Amount = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                              aggregate = "sum"),
              links = colDef(html=TRUE))
  )


  return(output)
}
