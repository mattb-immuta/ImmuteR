#' output updates to mb pipeline google sheet.
#'
#' @param op_list an update object returned form enrich_opportunities()
#' @param mb_tracker url to the mb tracker google sheet
#' @return none
#' @examples
#' output_gsheet_mbpipeline(op_list=op_list,mb_tracker)
#' @export
#'

output_BU_summary<-function(op_list){
  df=data.frame(churn_forecast_period=unlist(op_list$constants$churn_plan$fiscal.period))
  data=op_list$renewals %>%
    filter(op_list$renewals$churn_forecast_period %in% df$churn_forecast_period)
  data$churn_forecast_period=factor(data$churn_forecast_period,levels=df$churn_forecast_period)
  forecast= data %>%
    group_by(churn_forecast_period) %>%
    summarise(forecast=sum(`Churn Forecast`))

  df=left_join(df,forecast, by="churn_forecast_period")
  df$company_plan=unlist(op_list$constants$churn_plan$company_plan)
  df$delta=df$company_plan-df$forecast

  #`Fiscal Period`= op_list$constants$churn_plan$fiscal.period

  df$delta=df$company_plan-df$forecast

  df <- df %>%
    mutate(across(c(forecast, company_plan, delta), dollar))
  return(df)
}
