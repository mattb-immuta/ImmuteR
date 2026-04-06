#' pull current accounts from salesforce and enrich them.
#'
#' @param account.table salesforce account report
#' @param renewal.table output from enrich_renewal() function
#' @param looker_user_data_path path to google sheet with updated looker consumption data
#' @return an enriched account table with x,y,z.
#' @examples
#' example_optable("00OVH000005p6A92AI")
#' @export
#'
#summarize_accounts(op_list){
        summarize_account<-function(account_name,op_list){
         # account_name=op_list$accounts$`Account Name`[35]
          output=list()
          a_data=op_list$accounts %>%
            filter(`Account Name`==account_name)
          output[["account"]]=a_data$`Account Name`
          output[["metadata"]]=a_data %>%
            select(Region,`Contract ARR Rollup`,`Account Owner`,`Customer Success Manager`,`Field CTO`,org_chart_url,running_estimate_url) %>%
            t()
          output$metadata[2,1]=dollar(as.numeric(output$metadata[2,1]))

          output[["renewals"]]= op_list$renewals %>%
            filter(`Account Name`==account_name) %>%
            select(`Opportunity Name`,Amount,`Close Date`,`Account Risk`)
          output$renewals$Amount=dollar(output$renewals$Amount)

          output[["expansions"]]= op_list$expansions %>%
            filter(`Account Name`==account_name) %>%
            select(`Opportunity Name`,Amount,`Close Date`,Stage)
          output$expansions$Amount=dollar(output$expansions$Amount)

          output[["adoption"]]= a_data %>%
            select(`Adoption Phase`,`Vitally Days as Customer`,licensed_users,max_reg_users,mean_consumption,max_consumption,mean_queries,
                   global_policies_staged,use_case_rollout_url,consumption_url) %>%
            t()

          tmp_vec= a_data %>%
            select(`Customer on Unsupported Version`,`Tech Integrations`,`Is SaaS Customer`,`retool_url`) %>%
            t()

          names(tmp_vec)=c("Customer on Unsupported Version","Tech Integrations","Is SaaS Customer","retool_url")
          output[["Performance-Stability"]]= as.data.frame(tmp_vec)




          return(output)}



#}
