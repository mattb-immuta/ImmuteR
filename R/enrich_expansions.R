#' pull current accounts from salesforce and enrich them.
#'
#' @param parameterthingy A numeric value. The first number to be added.
#' @return a thingr.
#' @examples
#' example_optable("00OVH000005p6A92AI")
#' @export
#'
enrich_expansions<-function(expansion.table,account.table,fiscal.period){

  expansion.table=expansion.table %>%
    select(-`Account Risk`,-`Adoption Phase`)
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
  expansion.table$consumption_risk=expansion.table$days_until_close<120 & expansion.table$consumption<.6

  message(".. expansion data enriched /n")
  return(expansion.table)
}
