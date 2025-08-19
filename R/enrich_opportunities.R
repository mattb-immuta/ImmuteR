#' pull current opportunities from salesforce and enrich them.
#'
#' @param sf_opportunity_report A numeric value. The first number to be added.
#' @return a list with renewal and expansion data tables with all opportunities and enriched hyperlinks: opportunity_link,account_link,
#' and consumption_link.
#' @examples
#' enrich_optable("00OVH000005p6A92AI")
#' @export
enrich_opportunities<-function(sf_opportunity_report="00OVH000005p6A92AI",
                               sf_account_report="00OVH0000069G8Y2AU",
                               sf_provision_report="00OVH000006Zryr2AC",
                               sf_support_case_report="00OVH000006dyX72AI",
                               idex=FALSE,
                               fiscal.period,
                               re_tracker="https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"){

  message(".. enrich_opportunities initiated /n")
  c=load_constants()

  # create necessary hyperlinks
  opportunity.table<-sf_run_report(sf_opportunity_report)
  account.table<- sf_run_report(sf_account_report)
  marketplace.table<-sf_run_report(sf_provision_report)
  support.case.table<-sf_run_report(sf_support_case_report)


  opportunity.table$opportunity_url=paste("https://immuta.lightning.force.com/lightning/r/Opportunity/",as.character(opportunity.table$`Opportunity ID`),"/view",sep="")
  opportunity.table$account_url=paste("https://immuta.lightning.force.com/lightning/r/Account/",opportunity.table$`Account ID`,"/view",sep="")
  opportunity.table$consumption_url=paste("https://immuta.cloud.looker.com/dashboards/1052?Name=",gsub(" ","+",opportunity.table$`Account Name`),"&Account+ID=&Report+Month=6+month&Instance+Name=&Instance+Type=Production",sep="")



  # split into expansions and renewals
  renewals<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Renewal'),]
  expansions<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Expansion'),]

  message("opportunity data collected...enrich account data")

  consumption=enrich_consumption(idex=TRUE)

  account.table<-enrich_account.table(account.table,renewals,consumption.list=consumption)

  re.table=read_sheet(ss=re_tracker)
  account.table=merge(account.table,re.table[,c('Account Name','running_estimate_url')],all.x=TRUE)
  account.table$running_estimate_hyperlink=paste("[Running Estimate](",account.table$running_estimate_url,")",sep="")

  renewals=enrich_renewals(renewal.table=renewals,
                           account.table=account.table,
                           phat=c$phat_account,
                           fiscal.period=fiscal.period)

  expansions=enrich_expansions(expansion.table=expansions,
                               account.table=account.table,
                               fiscal.period=fiscal.period)




  # package enriched data tables into a list object
  output_list=list()
  output_list[["consumption"]]=consumption
  output_list[["renewals"]]=renewals
  output_list[["expansions"]]=expansions
  output_list[["accounts"]]=account.table
  output_list[["marketplace"]]=marketplace.table
  output_list[["support_cases"]]=support.case.table
  message(".. enrich_opportunities complete /n")
  return(output_list)}
