
#' pull current accounts from salesforce and enrich them.
#'
#' @param parameterthingy A numeric value. The first number to be added.
#' @return a thingr.
#' @examples
#' example_optable("00OVH000005p6A92AI")
#' @export
#'
enrich_renewals<-function(renewal.table,account.table,phat,fiscal.period,
                          fiscal.period.estimate){
  #renewal.table=renewals
  #phat=c$phat_adoption
  #fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027")
  #sheet_write(data.table(name=renewals$`Opportunity Name`,id=renewals$`Opportunity ID`,strategic_renewal=rep(FALSE,length(renewals$`Opportunity Name`))),ss=mbexport, sheet="strategic_renewals")
  renewal.table=merge(renewal.table,
                      account.table[,c("Account ID",names(account.table)[as.logical(1-names(account.table) %in% names(renewal.table))])],
                      by="Account ID",all.x=TRUE)

  strat_array=read_sheet(ss=mb_tracker, sheet="strategic_renewals")
  renewal.table=merge(renewal.table,strat_array,by="Opportunity ID",all.x=TRUE)
  risk.values=c('3. Low Risk','2. Moderate Risk','1. High Risk','Planned Lost','Closed Lost','0. Will Lose','Planned Won',"Closed Won")
  renewal.table =renewal.table[renewal.table$`Fiscal Period` %in% fiscal.period,]


  renewal.table$Amount=as.numeric(extract_usd_amount(renewal.table$Amount),scientific=FALSE)
  renewal.table$`Churn Forecast`=as.numeric(extract_usd_amount(renewal.table$`Churn Forecast`),scientific=FALSE)

  temp_tier <- matrix(c('Tier III','Tier III','Tier II','Tier III','Tier II','Tier I','Tier II','Tier I','Tier I',
                        rep('plan lost',3),rep('closed lost',3),rep('0. Will Lose',3),rep('plan won',3),rep('closed won',3)),ncol=3,byrow=TRUE)
  row.names(temp_tier)=risk.values
  colnames(temp_tier)=c('small','medium','large')

  d=length(renewal.table$Amount)
  value_array=priority_array=array("No Risk Estimate",dim=d)
  worst_array=expected_array=best_array=array(NA,dim=d)
  p_sequence=1:d
  p_sequence=p_sequence[-which(renewal.table$`Adoption Risk`=="-")]

  small_indecies=which(renewal.table$Amount<150000)
  med_indecies=which(renewal.table$Amount>=150000 & renewal.table$Amount<500000)
  large_indecies=which(renewal.table$Amount>=500000)
  value_array[small_indecies]='small'
  value_array[med_indecies]='medium'
  value_array[large_indecies]='large'

  for(i in p_sequence){
    priority_array[i]<-temp_tier[which(rownames(temp_tier)==renewal.table$`Adoption Risk`[i]),which(colnames(temp_tier)==value_array[i])]
    worst_array[i]<-(1-phat$worst_case[which(phat$risk.vec==renewal.table$`Adoption Risk`[i])])*as.numeric(renewal.table$Amount[i])
    expected_array[i]<-(1-phat$expected[which(phat$risk.vec==renewal.table$`Adoption Risk`[i])])*renewal.table$Amount[i]
    best_array[i]<-(1-phat$best_case[which(phat$risk.vec==renewal.table$`Adoption Risk`[i])])*renewal.table$Amount[i]
  }

  renewal.table$priority_change=renewal.table$Priority!=priority_array #bug
  renewal.table$Priority<-priority_array
  renewal.table$best_case_churn=best_array
  renewal.table$expected_churn=expected_array
  renewal.table$worst_case_churn=worst_array
  renewal.table$days_until_renewal=as.numeric(as.Date(renewal.table$`Close Date`)-rep(Sys.Date(),dim(renewal.table)[1]))
  renewal.table$utilization_risk=rep("moderate",length(renewal.table$utilization))
  renewal.table$utilization_risk[renewal.table$days_until_renewal<120 & renewal.table$utilization<.6]="high"
  renewal.table$utilization_risk[renewal.table$utilization>.9]="low"
  renewal.table$`Churn Forecast`[is.na(renewal.table$`Churn Forecast`)]=0
  renewal.table$expected_churn[is.na(renewal.table$expected_churn)]=0
  renewal.table$company_churn_hazard=renewal.table$expected_churn/sum(renewal.table$expected_churn)

  return(renewal.table)}
