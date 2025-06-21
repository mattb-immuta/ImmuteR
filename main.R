library(googlesheets4)
library(data.table)
library(salesforcer)
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
re_tracker<-"https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"
looker_user_data_path<-"https://docs.google.com/spreadsheets/d/11GQF96cDxwpidj0_OgvliNZiShug5e4lVyIqGCM6Qj0/edit?usp=sharing"

#authenticate
diff_date="2025-06-12"
fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027")

sf_auth()

devtools::load_all("R/")

new_op_list=enrich_opportunities(looker_user_data_path=looker_user_data_path)
load(paste("data/opportunities_",diff_date,".RData",sep=""))

op_list=output_list # hacky until compared object is of same structure

op_list=enrich_list_object(new_op_list,op_list)



output_gsheet_mbpipeline(op.list=op_list,mb_tracker)



save(op_list,file=paste("data/opportunities_",Sys.Date(),".RData",sep=""))
output_ELT_updates(op_list)





