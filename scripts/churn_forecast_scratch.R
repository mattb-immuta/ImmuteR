
# FY28Q3
ar_vec=c(1300000,267800,200000,1910256,200000,150000,353000,
         333000,1287500,100000,200000,150000,300000,400000)

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

FY28Q3$summary
FY28Q4$summary


p=c(.5,.1)

scenario expected_value     mean       sd min p10    p25    p50     p75     p90     max
1 Manual scenario       955155.6 963629.1 876572.3   0   0 353000 680000 1450000 2263026 5448556
prob_above_static
1            0.8829


scenario expected_value     mean       sd min p10   p25    p50     p75     p90     max
1 Manual scenario       699896.9 690617.5 612670.1   0   0 69695 549695 1030135 1529693 4197388
prob_above_static
1            0.8712
