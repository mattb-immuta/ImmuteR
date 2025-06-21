#' pull current opportunities from salesforce and enrich them.
#'
#' @param opportunity_report_id A numeric value. The first number to be added.
#' @return a list with renewal and expansion data tables with all opportunities and enriched hyperlinks: opportunity_link,account_link,
#' and consumption_link.
#' @examples
#' enrich_optable("00OVH000005p6A92AI")
#' @export
enrich_opportunities<-function(opportunity_report_id="00OVH000005p6A92AI",
                               account_report="00OVH0000069G8Y2AU",
                               marketplace_report_2="00OVH000006Zryr2AC",
                               looker_user_data_path=looker_user_data_path,
                               re_tracker="https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"){


  c=load_constants()

  # create necessary hyperlinks
  input_data<-sf_run_report(opportunity_report_id)
  account.table<- sf_run_report(account_report)
  marketplace.table<-sf_run_report(marketplace_report_2)
  input_data$opportunity_url=paste("https://immuta.lightning.force.com/lightning/r/Opportunity/",as.character(input_data$`Opportunity ID`),"/view",sep="")
  input_data$account_url=paste("https://immuta.lightning.force.com/lightning/r/Account/",input_data$`Account ID`,"/view",sep="")
  input_data$consumption_url=paste("https://immuta.cloud.looker.com/dashboards/1052?Name=",gsub(" ","+",input_data$`Account Name`),"&Account+ID=&Report+Month=6+month&Instance+Name=&Instance+Type=Production",sep="")



  # split into expansions and renewals
  renewals<-input_data[as.logical(input_data$`Opportunity Record Type` =='Renewal'),]
  expansions<-input_data[as.logical(input_data$`Opportunity Record Type` =='Expansion'),]

  account.table<-enrich_account.table(account.table,renewals,looker_user_data_path)

  re.table=read_sheet(ss=re_tracker)
  account.table=merge(account.table,re.table[,c('Account Name','re_hyperlink')],all.x=TRUE)
  account.table$re_hyperlink=paste("[Running Estimate](",account.table$re_hyperlink,")",sep="")

  renewals=enrich_renewals(renewal.table=renewals,
                           account.table=account.table,
                           phat=c$phat_adoption,
                           fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027"))

  expansions=enrich_expansions(expansion.table=expansions,
                               account.table=account.table,
                               fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027"))




  # package enriched data tables into a list object
  output_list=list()
  output_list[["renewals"]]=renewals
  output_list[["expansions"]]=expansions
  output_list[["accounts"]]=account.table
  output_list[["marketplace"]]=marketplace.table

  return(output_list)}
