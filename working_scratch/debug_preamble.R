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
library(tidyverse)

options(scipen = 999)


rm(list=ls())
#paths and identifiers for external data calls and writes


# google sheet read/write locations
account_doc_tracker<-"https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"
looker_user_data_path<-"https://docs.google.com/spreadsheets/d/11GQF96cDxwpidj0_OgvliNZiShug5e4lVyIqGCM6Qj0/edit?usp=sharing"

customer_ops_tracker<-"https://docs.google.com/spreadsheets/d/1BS3rBeDZJBflauFZc5cEdw09JTAfBM5Hwbcdh1rk1Bg/edit?usp=sharing"
customer_expansion_tracker="https://docs.google.com/spreadsheets/d/1gI5N6FHArFMn65y1y_9pc1l9TUJGViHxYkzVXbHNk9Y/edit?gid=0#gid=0"
BU_staging="https://docs.google.com/spreadsheets/d/1WuHvaVkHrLQkoUB9MiDe7OvM4Qw6JyQW25KWaJbjJpA/edit?gid=493722263#gid=493722263"
mb_tracker<-"https://docs.google.com/spreadsheets/d/1BLwqFOZpEmDvBkIAH7rCYBzPIJ0ADIUJfndzDRe9WgM/edit?usp=sharing"

#authenticate
diff_date="2025-08-14"
fiscal.period=c("Q1-2026","Q2-2026","Q3-2026","Q4-2026","Q1-2027","Q2-2027")

sf_auth()

devtools::load_all("R/")

sf_opportunity_report <- "00OVH000005p6A92AI"
  sf_account_report <- "00OVH0000069G8Y2AU"
  sf_provision_report<-"00OVH000006Zryr2AC"
  sf_support_case_report="00OVH000006dyX72AI"
  sf_contract_report = "00OVH0000070ZRZ2A2"
  fiscal.period<-fiscal.period
  idex=TRUE
  re_tracker="https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?gid=0#gid=0"

####################################################################
#. Debug enrich_account table
  renewal.table=renewals
  consumption.list = consumption
