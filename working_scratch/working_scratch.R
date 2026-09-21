

pbac_adoption=rep("black: negligible adoption",length(op_list$consumption$pbac_complexity$`Account Name`))

pbac_adoption[op_list$consumption$pbac_complexity$consumption_ratio >= .25]= "red: minimal adoption"
pbac_adoption[op_list$consumption$pbac_complexity$consumption_ratio >= .75]= "amber: moderate adoption"
pbac_adoption[op_list$consumption$pbac_complexity$complexity_score >= 4]= "red: minimal adoption"
pbac_adoption[op_list$consumption$pbac_complexity$complexity_score >= 4 &
                op_list$consumption$pbac_complexity$consumption_ratio >= .25]= "amber: moderate adoption"

pbac_adoption[op_list$consumption$pbac_complexity$complexity_score >= 4 &
                op_list$consumption$pbac_complexity$consumption_ratio >= .75]= "green: strong adoption"


op_list$consumption$pbac_complexity$pbac_adoption=pbac_adoption
