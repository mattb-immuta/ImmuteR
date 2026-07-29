# install.packages(c("readxl", "dplyr", "tidyr", "stringr", "gt"))
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(gt)
library(googlesheets4)

regional_summary="https://docs.google.com/spreadsheets/d/1Kk9eYpM0KCcdl5K6InBKZ8TPQn4GfZGw63hvFyFCqUc/edit?usp=sharing"

# 1) Read spreadsheet (Excel)
df <- op_list$accounts

df_wide <- df %>%
  select(c("Region","Account Name","Contract ARR Rollup","Customer Success Manager","Is SaaS Customer",
           "Customer on Unsupported Version","Snowflake","Tech Integrations","Account Risk","Adoption Phase","licensed_users" )) %>%
  arrange(Region, `Is SaaS Customer`)
for(i in unique(df_wide$Region)){
  temp=df_wide %>%
    filter(Region==i) %>%
    arrange(`Is SaaS Customer`,`Account Name`) %>%
    select(c('Account Name',"Is SaaS Customer"))
  sheet_write(temp,ss=regional_summary,sheet=i)
}
