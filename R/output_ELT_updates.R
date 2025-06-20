#' generate enriched outputs and briefing products
#'
#' @param op_list the updated list enriched list object.
#' @return HTML reports by quarter as well as summarized changes
#' @examples
#' output_ELT_updates(op_list)
#' @export
output_ELT_updates<-function(op_list=op_list){

  #create data objects for summary
  ELT_changes=op_list$renewal_changes
  ELT_renewals=op_list$renewals
  ELT_renewals$utilization_risk[is.na(ELT_renewals$utilization_risk)]="no data"


  ELT_renewals$changes<-ELT_renewals$`Opportunity Name`%in% ELT_changes$`Opportunity Name`

  for(i in fiscal.period){

    #i=fiscal.period[1]
    temp=ELT_renewals %>%
      filter(`Fiscal Period`==i) %>%
      select(opportunity_link,Region,`Customer Success Manager`,`Fiscal Period`,
             Amount,`Churn Forecast`,`Close Date`,
             `Adoption Risk`,utilization_risk,engagement_risk,`Date of last QBR/EBR`,`Date of next QBR/EBR`,
             licensed_users,registered_users,consumption_link,changes) %>%
      arrange(`Adoption Risk`)

    temp <- temp %>%
      mutate(
        `Adoption Risk` = factor(`Adoption Risk`, levels = c(
          "0. Will Lose", "Closed Lost", "1. High Risk",
          "2. Moderate Risk", "3. Low Risk", "Closed Won", "-"
        )),
        # utilization_risk now can have "no data", TRUE, FALSE
        utilization_risk = factor(utilization_risk, levels = c("FALSE", "TRUE", "no data"))
      )

    temp %>%

      gt() %>%
      tab_style(
        style = cell_text(weight = "bold"), # The style to apply: bold text
        locations = cells_body(rows = changes == TRUE)) %>%

      data_color(
        columns = `Adoption Risk`,
        fn = scales::col_factor(
          palette = c( "0. Will Lose" = "black", "Closed Lost" = "black",
                       "1. High Risk" = "#e74c3c",
                       "2. Moderate Risk" = "#f5b041",
                       "3. Low Risk" = "#7dcea0","Closed Won" = "#27ae60",
                       "-"= "white"),
          domain = c(
            "0. Will Lose", "Closed Lost", "1. High Risk",
            "2. Moderate Risk", "3. Low Risk", "Closed Won","-"
          ))) %>%

      data_color(
        columns = c(utilization_risk,engagement_risk),
        fn = scales::col_factor(
          palette = c(
            "FALSE" = "#f5b041", # Key for logical FALSE
            "TRUE" = "#e74c3c"# Key for logical TRUE
          ),
          # Crucial: Explicitly define the domain for logical factor levels
          domain = c("FALSE", "TRUE")
        )
      ) %>%

      fmt_markdown(columns = c(consumption_link, opportunity_link)) %>%

      summary_rows(
        groups = NULL,
        columns = `Churn Forecast`, # Specify the numeric columns to sum
        fns = list(
          Total = ~ sum(., na.rm = TRUE)),
        fmt = `Churn Forecast`~fmt_currency(.,currency = "USD",decimals=0))%>%

      fmt_currency(
        columns = c(Amount, `Churn Forecast`),
        currency = "USD",
        decimals = 0
      ) %>%




      cols_hide(columns=c(`Fiscal Period`,changes)) %>%

      tab_header(title = paste(i,"Bottoms Up - Churn",sep="")) %>%

      gtsave(filename=paste("output/",i,"_ELT_renewals.html",sep=""))
  }
}
