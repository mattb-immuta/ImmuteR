
FY28Q1=generate_forecast(op_list$renewals,p=c(.5,.1),churn.quarters="Q1-2028")

FY28Q2=generate_forecast(op_list$renewals,p=c(.5,.1),churn.quarters="Q2-2028")




# FY28Q3
ar_vec=c(1300000,267800,200000,1910256,200000,150000,353000,
         333000,1287500,150000,300000,400000)

ec_vec=c(0,480000)

FY28Q3 <- generate_forecast_manual(expected_churn = ec_vec,
                                 at_risk=ar_vec,
                                 p=c(.5,.1),
                                 static = 0)




# FY28Q4
ar_vec=c(517000,450000,550032,284677,455612,480000,
         234769,924964,250000,800000,535000,208000,960440)

ec_vec=c(0,69695)

FY28Q4 <- generate_forecast_manual(expected_churn = ec_vec,
                                   at_risk=ar_vec,
                                   p=c(.5,.1),
                                   static = 0)



