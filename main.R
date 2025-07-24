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
library(arrow)

options(scipen = 999)


rm(list=ls())
#paths and identifiers for external data calls and writes


# google sheet read/write locations
account_doc_tracker<-"https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"
looker_user_data_path<-"https://docs.google.com/spreadsheets/d/11GQF96cDxwpidj0_OgvliNZiShug5e4lVyIqGCM6Qj0/edit?usp=sharing"

customer_ops_tracker<-"https://docs.google.com/spreadsheets/d/1BS3rBeDZJBflauFZc5cEdw09JTAfBM5Hwbcdh1rk1Bg/edit?usp=sharing"
customer_expansion_tracker="https://docs.google.com/spreadsheets/d/1gI5N6FHArFMn65y1y_9pc1l9TUJGViHxYkzVXbHNk9Y/edit?gid=0#gid=0"
mb_tracker<-"https://docs.google.com/spreadsheets/d/1BLwqFOZpEmDvBkIAH7rCYBzPIJ0ADIUJfndzDRe9WgM/edit?usp=sharing"

#authenticate
diff_date="2025-07-10"
fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027","Q2-2027")

sf_auth()

devtools::load_all("R/")

#load and enrich data
new_op_list=enrich_opportunities(#input paths
  sf_opportunity_report <- "00OVH000005p6A92AI",
  sf_account_report <- "00OVH0000069G8Y2AU",
  sf_provision_report<-"00OVH000006Zryr2AC",
  sf_support_case_report="00OVH000006dyX72AI",
  fiscal.period<-fiscal.period,
  idex=TRUE)

load(paste("data/opportunities_",diff_date,".RData",sep=""))
#op_list=output_list # hacky until compared object is of same structure
op_list=enrich_list_object(new_list=new_op_list,
                           old_list=op_list)


# produce outputs

# push updates to my spreadsheet
output_gsheet_mbpipeline(op_list=op_list,mb_tracker)
output_gsheet_customer_ops_tracker(op_list=op_list,customer_ops_tracker,fiscal.period)
output_gsheet_expansion_tracker(op_list=op_list,customer_expansion_tracker,fiscal.period)
# produce confluence iframes via Rmarkdown



#output_ELT_updates(op_list)




#save list object
op_list[['dates']]=paste("Run Date: ",Sys.Date()," , Comparison Date: ",diff_date,".RData",sep="")
message(paste("saving op_list for ",op_list$dates," as: data/opportunities_",Sys.Date(),".RData",sep=""))
save(op_list,file=paste("data/opportunities_",Sys.Date(),".RData",sep=""))



