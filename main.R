library(googlesheets4)
library(data.table)
library(salesforcer)
library(lookr)
library(dplyr)
library(stringr)
library(DT)
library(htmltools)
library(roxygen2)
library(gt)
library(scales)

options(scipen = 999)


rm(list=ls())
#paths and identifiers for external data calls and writes
opportunity_report_id <- "00OVH000005p6A92AI" # Salesforce MB_export Report
account_report <- "00OVH0000069G8Y2AU" # MB_marketplace_export
provision_report<-"00OVH000006Zryr2AC"
mb_tracker<-"https://docs.google.com/spreadsheets/d/1TU2yzkFbUUjeXK3Ir_Bu9S8ixp_-JaraSkQGpke9yYg/edit?usp=sharing"
rd_export<-"https://docs.google.com/spreadsheets/d/1fuLLtwZGcZhUYthzPTZwVDLu6-PxQTrBAlRKYQ5l7WE/edit?gid=1687535972#gid=1687535972"
re_tracker<-"https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"

#authenticate

diff_date="2025-06-12"
fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027")

sf_auth()

source('R/functions.R')

new_op_list=enrich_opportunities()
load(paste("data/opportunities_",diff_date,".RData",sep=""))

# compare differences in new_op_list and the comparison op_list
op_list=generate_diff_tables(new_op_list,op_list)
op_list=generate_bu_tables(op_list)

customer_operations_tracker=list()

sheet_write(op_list$renewal_changes,ss="https://docs.google.com/spreadsheets/d/1fuLLtwZGcZhUYthzPTZwVDLu6-PxQTrBAlRKYQ5l7WE/edit?gid=1417207907#gid=1417207907",
            sheet="renewal_changes")

output=op_list$renewals %>%
  select( #basic renewal admin
          opportunity_link,`Opportunity Name`,Region,`Opportunity Owner`,
         `Customer Success Manager`,`Fiscal Period`,`Close Date`,Amount,`Churn Forecast`,

         # Comments
         `Next Step`,`RD Comment`,
         # Risk
         `Adoption Risk`,`Adoption Phase`,utilization_risk,licensed_users,
         registered_users,engagement_risk,`Date of last QBR/EBR`,`Date of next QBR/EBR`,
         `days_until_renewal`,expected_churn) %>%
  arrange(desc(expected_churn))

sheet_write(output,ss=mb_tracker,sheet='renewal_sheet')

output=op_list$expansions %>%
  select( #basic renewal admin
    opportunity_link,`Opportunity Name`,Region,`Opportunity Owner`,
    `Fiscal Period`,`Close Date`,Amount,`Probability (%)`,

    # Comments
    `Next Step`,`RD Comment`,
    # Risk
    `Adoption Risk`,`Adoption Phase`,utilization_risk,licensed_users,
    registered_users,engagement_risk,`Date of last QBR/EBR`,`Date of next QBR/EBR`) %>%
  arrange(desc(Amount))

sheet_write(output,ss=mb_tracker,sheet='expansion_sheet')
sheet_write(op_list$renewal_changes,ss=mb_tracker,
            sheet="renewal_changes")

save(op_list,file=paste("data/opportunities_",Sys.Date(),".RData",sep=""))
output_ELT_updates(op_list)
