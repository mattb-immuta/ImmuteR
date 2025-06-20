#' pull current accounts from salesforce and enrich them.
#'
#' @param parameterthingy A numeric value. The first number to be added.
#' @return a thingr.
#' @examples
#' example_optable("00OVH000005p6A92AI")
#' @export
#'
enrich_expansions<-function(expansion.table,account.table,fiscal.period){
  #expansion.table=optable[as.logical(optable$`Opportunity Record Type` =='Expansion'),]
  #account.table=
  #fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027")
  #sheet_write(data.table(name=expansions$`Opportunity Name`,id=expansions$`Opportunity ID`,strategic_renewal=rep(FALSE,length(renewals$`Opportunity Name`))),ss=mbexport, sheet="strategic_expansions")
  expansion.table=expansion.table %>%
    select(-`Adoption Risk`,-`Adoption Phase`)
  strat_array=read_sheet(ss=mb_tracker, sheet="strategic_expansions")
  expansion.table=merge(expansion.table,strat_array,by="Opportunity ID",all.x=TRUE)
  expansion.table=merge(expansion.table,
                        account.table[,c("Account ID",names(account.table)[as.logical(1-names(account.table) %in% names(expansion.table))])],
                        by="Account ID",all.x=TRUE)

  expansion.table =expansion.table[expansion.table$`Fiscal Period` %in% fiscal.period,]
  expansion.table=expansion.table[as.logical(1-duplicated(expansion.table$`Opportunity ID`)),]
  expansion.table$Amount=as.numeric(extract_usd_amount(expansion.table$Amount),scientific=FALSE)
  expansion.table$`Probability (%)`=as.numeric(sub("%","",expansion.table$`Probability (%)`))/100
  expansion.table$expected_revenue=expansion.table$Amount*expansion.table$`Probability (%)`
  expansion.table$expansion_type=rep('expansion',dim(expansion.table)[1])
  expansion.table$days_until_close=as.numeric(as.Date(expansion.table$`Close Date`)-rep(Sys.Date(),dim(expansion.table)[1]))
  expansion.table$utilization_risk=expansion.table$days_until_close<120 & expansion.table$utilization<.6

  #sheet_write(expansion.table,ss=mb_tracker, sheet = "expansion_sheet")
  return(expansion.table)
}
