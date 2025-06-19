
#' Load prioritization and forecast constants.
#'
#' This function takes two numeric inputs and returns their sum.
#'
#' @return A list with a probability matrix, phat, a risk index vector, and arr index.
#' @export
#'
load_constants<-function(risk.vec=c('3. Low Risk','2. Moderate Risk','1. High Risk','Planned Lost','Closed Lost','0. Will Lose','Planned Won',"Closed Won")){
  c=list()
  c$phat_adoption <- data.table(worst_case = c(.9,.75,.5,0,0,0,1,1),
                              expected = c(.95,.85,.675,0,0,0,1,1),
                              best_case = c(1,.9,.75,0,0,0,1,1),
                              risk.vec)


  c$risk_index = risk.vec
  c$arr_index = c(0,150000,
                  500000)
  return(c)
}

#' pull current opportunities from salesforce and enrich them.
#'
#' @param opportunity_report_id A numeric value. The first number to be added.
#' @return a list with renewal and expansion data tables with all opportunities and enriched hyperlinks: opportunity_link,account_link,
#' and consumption_link.
#' @examples
#' enrich_optable("00OVH000005p6A92AI")
#' @export
enrich_opportunities<-function(opportunity_report_id="00OVH000005p6A92AI",
                               account_report="00OVH0000069G8Y2AU",
                               marketplace_report_2="00OVH000006Zryr2AC",
                               re_tracker="https://docs.google.com/spreadsheets/d/1Rxl25OyKEW1W1ny8Vbv3TDa2QCqR9VSpTN0QVPu6o9w/edit?usp=sharing"){


      c=load_constants()

      # create necessary hyperlinks
      input_data<-sf_run_report(opportunity_report_id)
      account.table<- sf_run_report(account_report)
      marketplace.table<-sf_run_report(marketplace_report_2)
      input_data$opportunity_link=paste("[",input_data$`Opportunity Name`,"](https://immuta.lightning.force.com/lightning/r/Opportunity/",as.character(input_data$`Opportunity ID`),"/view)",sep="")
      input_data$account_link=paste("[",input_data$`Account Name`,"](https://immuta.lightning.force.com/lightning/r/Account/",input_data$`Account ID`,"/view)",sep="")
      input_data$consumption_link=paste("[consumption](https://immuta.cloud.looker.com/dashboards/1052?Name=",gsub(" ","+",input_data$`Account Name`),"&Account+ID=&Report+Month=6+month&Instance+Name=&Instance+Type=Production)",sep="")

    # split into expansions and renewals
      renewals<-input_data[as.logical(input_data$`Opportunity Record Type` =='Renewal'),]
      expansions<-input_data[as.logical(input_data$`Opportunity Record Type` =='Expansion'),]

      account.table<-enrich_account.table(account.table,renewals)

      re.table=read_sheet(ss=re_tracker)
      account.table=merge(account.table,re.table[,c('Account Name','re_hyperlink')],all.x=TRUE)
      account.table$re_hyperlink=paste("[Running Estimate](",account.table$re_hyperlink,")",sep="")

      renewals=enrich_renewals(renewal.table=renewals,
                               account.table=account.table,
                               phat=c$phat_adoption,
                               fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027"))

      expansions=enrich_expansions(expansion.table=expansions,
                                   account.table=account.table,
                                   fiscal.period=c("Q2-2026","Q3-2026","Q4-2026","Q1-2027"))




      # package enriched data tables into a list object
      output_list=list()
      output_list[["renewals"]]=renewals
      output_list[["expansions"]]=expansions
      output_list[["accounts"]]=account.table
      output_list[["marketplace"]]=marketplace.table

      save(op_list=output_list,file=paste("data/opportunities_",Sys.Date(),".RData",sep=""))

      return(output_list)}



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
  renewal.table$utilization_risk=renewal.table$days_until_renewal<120 & renewal.table$utilization<.6
  renewal.table$`Churn Forecast`[is.na(renewal.table$`Churn Forecast`)]=0
  #temp=gsub("-","",renewal.table$`Next Step`)
  #temp[is.na(temp)]=Sys.Date()-100
  #renewal.table$age_nextstep=Sys.Date()-as.Date(str_extract(temp, "^(\\d{1,2})/(\\d{2})"),format = "%m/%d")

  #sheet_write(renewal.table,ss=mb_tracker, sheet = "renewal_sheet")
  return(renewal.table)}

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


extract_usd_amount <- function(vector) {
  # Remove "USD " from the beginning of the string
  for(i in 1:length(vector)){vector[i]=sub("^USD\\s*", "", gsub(",", "", vector[i]))}
  vector=as.numeric(vector)
  return(vector)
}

enrich_account.table<-function(input_data,renewal.table){
  looker_data_path<-"https://docs.google.com/spreadsheets/d/1IHUD5cvNjj3PfdPqNvgTnU_01Lbrq9bVRY374jEqExk/edit?usp=sharing"

  input_data=merge(input_data,renewal.table,all.x=TRUE)
  input_data=input_data[as.logical(1-duplicated(input_data$`Account ID`)),]
  input_data=input_data[as.logical(1-duplicated(input_data$`Account Name`)),]

  user_metrics=read_sheet(looker_data_path)
  user_metrics=data.table(`Account Name`=user_metrics$ACCOUNT_NAME,
                          saas=user_metrics$IS_SAAS_CUSTOMER,
                          licensed_users=user_metrics$salesforce_contracts.sum_of_all_contracted_user_types,
                          registered_users=user_metrics$immuta_instance_metrics_daily.count_full_user_pricing_counts+user_metrics$immuta_instance_metrics_daily.count_policy_owner_pricing_counts)

  input_data=merge(input_data,user_metrics,by="Account Name",all.x=TRUE)
  input_data=input_data[as.logical(1-duplicated(input_data$`Account Name`)),]
  input_data$utilization=input_data$registered_users/input_data$licensed_users

  input_data$utilization[is.infinite(input_data$utilization)]=NA
  input_data$engagement_risk=(Sys.Date()-input_data$`Date of last QBR/EBR`)>180 | is.na(input_data$`Date of last QBR/EBR`)
  return(input_data)}



analyze_opportunities=function(opportunity_list,comparison_date){
  output_filepath=paste("data/opportunities_",,".RData",sep="")
}


#' compare two update objects and append renewal changes to list.
#'
#' @param new_list the updated list enriched list object.
#' @param old_list the enriched list object to compare and identify changes
#' @return appends a 'renewal_changes' data table to 'new_list'.
#' @examples
#' generate_diff_tables(new_op_list,op_list)
#' @export
generate_diff_tables<-function(new_list,old_list){
  #new_list=new_op_list
  #old_list=op_list
  new_list$renewals$`Churn Forecast`[is.na( new_list$renewals$`Churn Forecast`)]=0
  old_list$renewals$`Churn Forecast`[is.na( old_list$renewals$`Churn Forecast`)]=0

  renewal_changes_risk=new_list$renewals$`Adoption Risk`!=old_list$renewals$`Adoption Risk`
  message(paste("Renewal Changes:",sum(renewal_changes_risk)," changes to adoption risk..."))
  renewal_changes_forecast=new_list$renewals$`Churn Forecast`!=old_list$renewals$`Churn Forecast`
  message(paste(sum(renewal_changes_forecast)," changes to churn forecast..."))

  renewal_changes=renewal_changes_risk|renewal_changes_forecast
  message(paste(sum(renewal_changes)," total changes to renewals..."))

  output=data.table(`Opportunity Name`=new_list$renewals$`Opportunity Name`[renewal_changes],
                    `Fiscal Period`=new_list$renewals$`Fiscal Period`[renewal_changes],
                    `New Adoption Risk`=new_list$renewals$`Adoption Risk`[renewal_changes],
                    `Old Adoption Risk`=old_list$renewals$`Adoption Risk`[renewal_changes],
                    `New Forecast`=new_list$renewals$`Churn Forecast`[renewal_changes],
                    `Old Forecast`=old_list$renewals$`Churn Forecast`[renewal_changes])
  output$forecast_change=output$'New Forecast'-output$`Old Forecast`
  new_list[['renewal_changes']]=output

  #provision_changes_status=new_list$marketplace$`Marketplace Status`!=old_list$marketplace_changes$`Marketplace Status`
  #message(paste("Provision Changes:",sum(provision_changes_status)," changes to status..."))
  #provision_changes_timeline=new_list$marketplace$`Est. Date to Move Out of Stage (MP)`!=old_list$marketplace_changes$`Est. Date to Move Out of Stage (MP)`
  #message(paste(sum(provision_changes_timeline)," changes to timeline..."))

  #renewal_changes=renewal_changes_risk|renewal_changes_forecast
  #message(paste(sum(renewal_changes)," total changes to renewals..."))

  #output=data.table(`Opportunity Name`=new_list$renewals$`Opportunity Name`[renewal_changes],
  #                  `Fiscal Period`=new_list$renewals$`Fiscal Period`[renewal_changes],
  #                  `New Adoption Risk`=new_list$renewals$`Adoption Risk`[renewal_changes],
  #                  `Old Adoption Risk`=old_list$renewals$`Adoption Risk`[renewal_changes],
  #                  `New Forecast`=new_list$renewals$`Churn Forecast`[renewal_changes],
  #                  `Old Forecast`=old_list$renewals$`Churn Forecast`[renewal_changes])
  #output$forecast_change=output$'New Forecast'-output$`Old Forecast`
  #new_list[['renewal_changes']]=output

  return(new_list)
}

#' generate customer operations tracker data from renewals and expansions.
#'
#' @param op_list the updated list enriched list object.
#' @param old_list the enriched list object to compare and identify changes
#' @return a customer operations tracker data table with expansion context.
#' @examples
#' generate_bu_tables(op_list)
#' @export
generate_bu_tables<-function(op.table){
  #op.table=op_list
  temp.expansions=op.table$expansions[,c("Account ID","Amount")]
  temp.expansions=temp.expansions %>%
    group_by(`Account ID`) %>%
    summarise(total_amount = sum(Amount),active_expansions = n())

  customer_ops_tracker=merge(op.table$renewals[,c("Account ID","account_link","opportunity_link","Amount","Close Date",
                                                  "Fiscal Period","Region",
                                                  "Adoption Risk","Stage","registered_users","licensed_users","utilization_risk",
                                                  "consumption_link",
                                                  "Next Step","RD Comment","Date of last QBR/EBR","Date of next QBR/EBR",
                                                  "re_hyperlink")],
                             temp.expansions,by="Account ID",all.x=TRUE)

  customer_ops_tracker=customer_ops_tracker[,c("account_link","opportunity_link","Amount","Close Date","Fiscal Period","Region",

                                            #account health
                                            "Adoption Risk","Stage","registered_users","licensed_users","utilization_risk",
                                            "consumption_link","active_expansions","total_amount",

                                            #account team engagement
                                            "Next Step","RD Comment","Date of last QBR/EBR","Date of next QBR/EBR",
                                            "re_hyperlink")]
   op.table[["customer_ops_tracker"]]=customer_ops_tracker
  return(op.table)
}

#' generate enriched outputs and briefing products
#'
#' @param op_list the updated list enriched list object.
#' @return HTML reports by quarter as well as summarized changes
#' @examples
#' output_ELT_updates(op_list)
#' @export
output_ELT_updates<-function(op_list=op_list){

  #create data objects for summary
  ELT_changes=op_list$renewal_changes
  ELT_renewals=op_list$renewals
  ELT_renewals$utilization_risk[is.na(ELT_renewals$utilization_risk)]="no data"


  ELT_renewals$changes<-ELT_renewals$`Opportunity Name`%in% ELT_changes$`Opportunity Name`

  for(i in fiscal.period){

    #i=fiscal.period[1]
    temp=ELT_renewals %>%
      filter(`Fiscal Period`==i) %>%
      select(opportunity_link,Region,`Customer Success Manager`,`Fiscal Period`,
             Amount,`Churn Forecast`,`Close Date`,
             `Adoption Risk`,utilization_risk,engagement_risk,`Date of last QBR/EBR`,`Date of next QBR/EBR`,
             licensed_users,registered_users,consumption_link,changes) %>%
      arrange(`Adoption Risk`)

    temp <- temp %>%
      mutate(
        `Adoption Risk` = factor(`Adoption Risk`, levels = c(
          "0. Will Lose", "Closed Lost", "1. High Risk",
          "2. Moderate Risk", "3. Low Risk", "Closed Won", "-"
        )),
        # utilization_risk now can have "no data", TRUE, FALSE
        utilization_risk = factor(utilization_risk, levels = c("FALSE", "TRUE", "no data"))
      )

    temp %>%

      gt() %>%
      tab_style(
        style = cell_text(weight = "bold"), # The style to apply: bold text
        locations = cells_body(rows = changes == TRUE)) %>%

      data_color(
        columns = `Adoption Risk`,
        fn = scales::col_factor(
          palette = c( "0. Will Lose" = "black", "Closed Lost" = "black",
                       "1. High Risk" = "#e74c3c",
                       "2. Moderate Risk" = "#f5b041",
                       "3. Low Risk" = "#7dcea0","Closed Won" = "#27ae60",
                       "-"= "white"),
          domain = c(
            "0. Will Lose", "Closed Lost", "1. High Risk",
            "2. Moderate Risk", "3. Low Risk", "Closed Won","-"
          ))) %>%

      data_color(
        columns = c(utilization_risk,engagement_risk),
        fn = scales::col_factor(
          palette = c(
            "FALSE" = "#f5b041", # Key for logical FALSE
            "TRUE" = "#e74c3c"# Key for logical TRUE
          ),
          # Crucial: Explicitly define the domain for logical factor levels
          domain = c("FALSE", "TRUE")
        )
      ) %>%

      fmt_markdown(columns = c(consumption_link, opportunity_link)) %>%

      summary_rows(
        groups = NULL,
        columns = `Churn Forecast`, # Specify the numeric columns to sum
        fns = list(
          Total = ~ sum(., na.rm = TRUE)),
        fmt = `Churn Forecast`~fmt_currency(.,currency = "USD",decimals=0))%>%

      fmt_currency(
        columns = c(Amount, `Churn Forecast`),
        currency = "USD",
        decimals = 0
      ) %>%




      cols_hide(columns=c(`Fiscal Period`,changes)) %>%

      tab_header(title = paste(i,"Bottoms Up - Churn",sep="")) %>%

      gtsave(filename=paste("output/",i,"_ELT_renewals.html",sep=""))
  }
}

