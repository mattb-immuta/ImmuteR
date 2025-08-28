
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
  renewal.table=merge(renewal.table,
                      account.table[,c("Account ID",names(account.table)[as.logical(1-names(account.table) %in% names(renewal.table))])],
                      by="Account ID",all.x=TRUE)


  risk.values=c('3. Low Risk','2. Moderate Risk','1. High Risk','Planned Lost','Closed Lost','0. Will Lose','0. Will Lose (Formally Notified)','4. Will Win',"Closed Won")
  renewal.table =renewal.table[renewal.table$`Fiscal Period` %in% fiscal.period,]


  renewal.table$Amount=as.numeric(extract_usd_amount(renewal.table$Amount),scientific=FALSE)
  renewal.table$`Churn Forecast`=as.numeric(extract_usd_amount(renewal.table$`Churn Forecast`),scientific=FALSE)

  temp_tier <- matrix(c('Tier III','Tier III','Tier II','Tier III','Tier II','Tier I','Tier II','Tier I','Tier I',
                        rep('plan lost',3),rep('Closed Lost',3),rep('0. Will Lose',3),rep('0. Will Lose (Formally Notified)',3),rep('4. Will Win',3),rep('Closed Won',3)),ncol=3,byrow=TRUE)
  row.names(temp_tier)=risk.values
  colnames(temp_tier)=c('small','medium','large')

  d=length(renewal.table$Amount)
  value_array=priority_array=phat_expected_array=array("No Risk Estimate",dim=d)
  worst_array=expected_array=best_array=array(NA,dim=d)
  p_sequence=1:d
  p_sequence=p_sequence[-which(renewal.table$`Account Risk`=="-")]

  small_indecies=which(renewal.table$Amount<150000)
  med_indecies=which(renewal.table$Amount>=150000 & renewal.table$Amount<500000)
  large_indecies=which(renewal.table$Amount>=500000)
  value_array[small_indecies]='small'
  value_array[med_indecies]='medium'
  value_array[large_indecies]='large'

  for(i in p_sequence){
    priority_array[i]<-temp_tier[which(rownames(temp_tier)==renewal.table$`Account Risk`[i]),which(colnames(temp_tier)==value_array[i])]
    phat_expected_array[i]<-1-phat$expected[which(phat$risk.vec==renewal.table$`Account Risk`[i])]
    worst_array[i]<-(1-phat$worst_case[which(phat$risk.vec==renewal.table$`Account Risk`[i])])*as.numeric(renewal.table$Amount[i])
    expected_array[i]<-(1-phat$expected[which(phat$risk.vec==renewal.table$`Account Risk`[i])])*as.numeric(renewal.table$Amount[i])
    best_array[i]<-(1-phat$best_case[which(phat$risk.vec==renewal.table$`Account Risk`[i])])*as.numeric(renewal.table$Amount[i])
  }

  renewal.table$priority_change=renewal.table$Priority!=priority_array #bug
  renewal.table$Priority<-priority_array
  renewal.table$phat_expected=phat_expected_array
  renewal.table$best_case_churn=best_array
  renewal.table$expected_churn=expected_array
  renewal.table$worst_case_churn=worst_array
  renewal.table$days_until_renewal=as.numeric(as.Date(renewal.table$`Close Date`)-rep(Sys.Date(),dim(renewal.table)[1]))
  renewal.table$consumption_risk=rep("moderate",length(renewal.table$max_consumption))
  renewal.table$max_consumption_risk[renewal.table$days_until_renewal<120 & renewal.table$max_consumption<.6]="high"
  renewal.table$max_consumption_risk[renewal.table$max_consumption>.9]="low"
  renewal.table$`Churn Forecast`[is.na(renewal.table$`Churn Forecast`)]=0
  renewal.table$expected_churn[is.na(renewal.table$expected_churn)]=0
  renewal.table$company_churn_hazard=renewal.table$expected_churn/sum(renewal.table$expected_churn)

  #generate the churn forecast period by adding 1 day to the contract end date
  renewal.table$rev_lost_day=as.Date(renewal.table$`Current Contract End Date`)+1
  renewal.table$churn_forecast_period=date_to_fiscal_quarter(renewal.table$rev_lost_day)
  message(".. renewal data enriched /n")
  return(renewal.table)}
