#' generate customer operations tracker data from renewals and expansions.
#'
#' @param op_list the updated list enriched list object.
#' @param old_list the enriched list object to compare and identify changes
#' @return a customer operations tracker data table with expansion context.
#' @examples
#' generate_bu_tables(op_list)
#' Known problems: look at Swedbank.  The Amount Column needs to be the ARR for the renewal.
#' @export
generate_forecast<-function(op.table,
                            p=c(.6,.1),
                            churn.quarters=c("Q4-2026","Q1-2027"),
                            n=1000){
  output.list=list()
 # op.table=op_list
 # i=churn.quarters[1]
  for(i in churn.quarters){
    temp.renewals= op.table$renewals %>%
      filter(`Churn Quarter`==i) %>%
      select(c(`Account Name`,`Churn Forecast`,`Amount`,`Account Risk`,`Churn Quarter`))

      static_churn=sum( (as.numeric(temp.renewals$`Account Risk`=="Closed Lost")+
        as.numeric(temp.renewals$`Account Risk`=="Verbally Confirmed"))*temp.renewals$`Churn Forecast`)

      expected_churn_vec=as.numeric(temp.renewals$`Account Risk`=="Expected to Churn") * temp.renewals$`Churn Forecast`
      expected_churn_pos=expected_churn_vec[expected_churn_vec>0]

      m=length(expected_churn_pos)
      expected_churn_sim=apply(matrix(expected_churn_pos,nrow=n,ncol=m,byrow=TRUE) * matrix(rbinom(n*m,1,p[1]),nrow=n,ncol=m),
                      1,sum)

      at_risk_vec=as.numeric(temp.renewals$`Account Risk`=="At Risk Renewal") * temp.renewals$`Churn Forecast`
      at_risk_pos=at_risk_vec[at_risk_vec>0]

      m=length(at_risk_pos)
      at_risk_sim=apply(matrix(at_risk_pos,nrow=n,ncol=m,byrow=TRUE) * matrix(rbinom(n*m,1,p[2]),nrow=n,ncol=m),
                               1,sum)

      output.list[[i]] = static_churn + expected_churn_sim + at_risk_sim


  }
  return(output.list)
}
