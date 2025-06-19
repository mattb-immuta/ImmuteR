
temp=op_list$renewals %>%
  arrange(`Opportunity Name`) %>%
  filter(Amount>=250000) %>%
  select(account_link,Region,`Customer Success Manager`,
         `Adoption Risk`,utilization_risk,engagement_risk,consumption_link) %>%
  distinct(account_link, .keep_all = TRUE)

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

  fmt_markdown(columns = c(account_link,consumption_link)) %>%






  #cols_hide(columns=c(`Fiscal Period`,changes)) %>%

  tab_header(title = "DRAFT: Account Health Tracker") %>%

  gtsave(filename="output/health_tracker.html")
