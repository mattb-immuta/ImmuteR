#hacky thing to change 'adoption risk' to 'account risk'

names(op_list$renewals)[13]="Account Risk"
names(op_list$renewals)[33]="Account Risk"
names(op_list$accounts)[16]="Account Risk"
names(op_list$renewal_changes)[c(3,4)]=rep("Account Risk",2)
names(op_list$customer_ops_tracker)[7]="Account Risk"
save(op_list,file="data/opportunities_2025-07-10.RData")
