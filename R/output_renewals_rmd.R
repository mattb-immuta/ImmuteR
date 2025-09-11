#' develop an html table for BU renewals.
#'
#' @param op.list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op.list=op_list,mb_tracker)
#' @export
#'

output_renewals_rmd<-function(op_list,churn.period){
  df=op_list$renewals %>%
    filter(churn_forecast_period ==churn.period) %>%
    mutate(account_name = paste0('<a href="', account_url, '" target="_blank">',`Account Name`, '</a>')) %>%
    mutate(links = paste0('<a href="', consumption_url, '" target="_blank">',"utilization db", '</a> \n <a href="',
                                running_estimate_url,'" target="_blank">',"success plan", '</a>')) %>%
    select(account_name,Region, Amount,`Churn Forecast`, `Close Date`,`Account Risk`,`Next Step`,links)


  output=list()
  output[["plot"]]<-reactable(df,filterable = TRUE, resizable = TRUE, bordered=TRUE, minRows = 10,
            columns = list(
              account_name = colDef(html=TRUE),
              `Close Date`=colDef(format = colFormat(date = TRUE)),
              `Churn Forecast` = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                                        aggregate = "sum"),
              Amount = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                              aggregate = "sum"),
              `Next Step`=colDef(minWidth=250),
              links = colDef(html=TRUE))
  )


  return(output)
}
