#Notes:  Need to update generate_forecast() to output mutliple quarters with colnames per quarter

library(plotly)

f=generate_forecast(op_list,
                    churn.quarters=c("Q1-2027","Q2-2027","Q3-2027","Q4-2027"),
                    p=c(.5,.1))



company_plan=c(778948,902220,81000,1133956)
p=c(sum(company_plan[1]>=f$`Q1-2027`),
    sum(company_plan[2]>=f$`Q2-2027`),
    sum(company_plan[3]>=f$`Q3-2027`),
    sum(company_plan[4]>=f$`Q4-2027`))/1000





fig <- plot_ly(y =f$`Q1-2027`, type = "box", quartilemethod="exclusive",name="FY27Q1",
               showlegend = FALSE) %>%
  add_trace(y=f$`Q2-2027`,name="FY27Q2") %>%
  add_trace(y=f$`Q3-2027`,name="FY27Q3") %>%
  add_trace(y=f$`Q4-2027`,name="FY27Q4") %>%

  add_trace(
    x = "FY27Q1",
    y = company_plan[1],
    type = "scatter",
    mode = "markers+text",
    text= p[1],
    textposition= "right",

   marker = list(
     size = 12,
     color = "black",
     symbol = "diamond"),
   name = "Company Plan") %>%

  add_trace(
    x = "FY27Q2",
    y = company_plan[2],
    type = "scatter",
    mode = "markers+text",
    text= p[2],
    textposition= "right",

    marker = list(
      size = 12,
      color = "black",
      symbol = "diamond"),
    name = "Company Plan",
    showlegend = FALSE) %>%

  add_trace(
    x = "FY27Q3",
    y = company_plan[3],
    type = "scatter",
    mode = "markers+text",
    text= p[3],
    textposition= "right",

    marker = list(
      size = 12,
      color = "black",
      symbol = "diamond"),
    name = "Company Plan",
    showlegend = FALSE) %>%

  add_trace(
    x = "FY27Q4",
    y = company_plan[4],
    type = "scatter",
    mode = "markers+text",
    text= p[4],
    textposition= "right",

    marker = list(
      size = 12,
      color = "black",
      symbol = "diamond"),
    name = "Company Plan",
    showlegend = FALSE) %>%

  layout(title = paste0("FY27 Forecast as of ",Sys.Date()),
         yaxis=list(tickformat="$,.2s"))

fig










