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
                               sf_active_order_report = "00OVV000006AOnB2AW",
                               sf_saas_migration_report ="00OVV000006FbWr2AK" ,
                               sf_csm_summary_report = "00OVV000006FbNB2A0",
                               idex=FALSE,
                               idex_path="data/idex/",
                               fiscal.period){

  message(".. enrich_opportunities initiated")

  # create necessary hyperlinks
  opportunity.table<-sf_run_report(sf_opportunity_report)
    opportunity.table$`Fiscal Period`<-factor(opportunity.table$`Fiscal Period`,levels=fiscal.period)
    opportunity.table$opportunity_url=paste("https://immuta.lightning.force.com/lightning/r/Opportunity/",as.character(opportunity.table$`Opportunity ID`),"/view",sep="")

  account.table<- sf_run_report(sf_account_report)
    account.table$account_url=paste("https://immuta.lightning.force.com/lightning/r/Account/",account.table$`Account ID`,"/view",sep="")
    account.table$consumption_url=paste("https://immuta.cloud.looker.com/dashboards/1052?Name=",gsub(" ","+",account.table$`Account Name`),"&Account+ID=&Report+Month=6+month&Instance+Name=&Instance+Type=Production",sep="")
    account.table$`Contract ARR Rollup`=extract_usd_amount(account.table$`Contract ARR Rollup`)
    # account.table$retool_url=paste("https://retool-nonprod.ic-ops.net/app/customer-central/customerDetail?customerName=",gsub(" ","%2O",account.table$`Account Name`),sep="")


  # split into expansions and renewals
  renewals<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Renewal'),]
  expansions<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Expansion'),]

  message("opportunity data collected...enrich opportunity data")


   renewals=enrich_renewals(renewal.table=renewals,
                           account.table=account.table,
                           fiscal.period=NULL)

  expansions=enrich_expansions(expansion.table=expansions,
                               account.table=account.table,
                               fiscal.period=fiscal.period)

  # package enriched data tables into a list object
  output_list=list()
  output_list[["renewals"]]=renewals
  output_list[["expansions"]]=expansions
  output_list[["accounts"]]=account.table
  output_list[["active_orders"]]<-sf_run_report(sf_active_order_report)
  output_list[["csm_table"]]<-sf_run_report(sf_active_order_report)
  output_list[["saas_pipeline"]]<-sf_run_report(sf_saas_migration_report)
  message(".. enrich_opportunities complete")
  return(output_list)}
