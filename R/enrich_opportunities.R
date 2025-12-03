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
                               sf_contract_report = "00OVH0000070ZRZ2A2",
                               idex=FALSE,
                               fiscal.period,
                               re_tracker="https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?gid=0#gid=0"){

  message(".. enrich_opportunities initiated /n")
  c=load_constants()
  c$fiscal.period=fiscal.period

  # create necessary hyperlinks
  opportunity.table<-sf_run_report(sf_opportunity_report)
  account.table<- sf_run_report(sf_account_report)
  marketplace.table<-sf_run_report(sf_provision_report)
  support.case.table<-sf_run_report(sf_support_case_report)
  contract.table<-sf_run_report(sf_contract_report) %>%
    filter(`Order Status`=="Active")

  opportunity.table$`Fiscal Period`<-factor(opportunity.table$`Fiscal Period`,levels=fiscal.period)
  opportunity.table$opportunity_url=paste("https://immuta.lightning.force.com/lightning/r/Opportunity/",as.character(opportunity.table$`Opportunity ID`),"/view",sep="")
  opportunity.table$account_url=paste("https://immuta.lightning.force.com/lightning/r/Account/",opportunity.table$`Account ID`,"/view",sep="")
  opportunity.table$consumption_url=paste("https://immuta.cloud.looker.com/dashboards/1052?Name=",gsub(" ","+",opportunity.table$`Account Name`),"&Account+ID=&Report+Month=6+month&Instance+Name=&Instance+Type=Production",sep="")



  # split into expansions and renewals
  renewals<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Renewal'),]
  expansions<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Expansion'),]

  message("opportunity data collected...enrich account data")

  consumption=enrich_consumption(idex=idex)

  account.table<-enrich_account.table(account.table,renewals,consumption.list=consumption)
  account.table$`Contract ARR Rollup`=extract_usd_amount(account.table$`Contract ARR Rollup`)
  account.table$retool_url=paste("https://retool-nonprod.ic-ops.net/app/customer-central/customerDetail?customerName=",gsub(" ","%2O",account.table$`Account Name`),sep="")

  re.table=read_sheet(ss=re_tracker)
  account.table=merge(account.table,re.table[,c('Account Name','running_estimate_url',
                                                'use_case_rollout_url','org_chart_url')],all.x=TRUE)
  renewals=enrich_renewals(renewal.table=renewals,
                           account.table=account.table,
                           phat=c$phat_account,
                           fiscal.period=NULL)

  expansions=enrich_expansions(expansion.table=expansions,
                               account.table=account.table,
                               fiscal.period=fiscal.period)

  account.table$churn_hazard=array(0,dim=length(account.table$`Account Name`))

  for(i in 1:length(account.table$`Account ID`)){
    account.table$churn_hazard[i]=sum(renewals$expected_churn[renewals$`Account ID`==account.table$`Account ID`[i]])
  }

  df=as.data.frame(ftable(support.case.table$`Account Name`))

  names(df)= c("Account Name","Cases in last 120d")

  account.table=merge(account.table,df,by="Account Name",all.x=TRUE)
  account.table$`Cases in last 120d`[is.na(account.table$`Cases in last 120d`)]=0

  health_metrics=merge(op_list$accounts,df,by="Account Name",all.x=TRUE)
  # package enriched data tables into a list object
  output_list=list()
  output_list[["contracts"]]=contract.table
  output_list[["consumption"]]=consumption
  output_list[["renewals"]]=renewals
  output_list[["expansions"]]=expansions
  output_list[["accounts"]]=account.table
  output_list[["marketplace"]]=marketplace.table
  output_list[["support_cases"]]=support.case.table
  message(".. enrich_opportunities complete /n")
  return(output_list)}
