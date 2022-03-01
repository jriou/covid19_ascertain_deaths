#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_401_format_regbma <- function(x) {
  summ_samples = function(x) c(quantile(x,probs=c(0.5,0.025,0.975)),prob1=mean(x>1))
  x = x %>% 
    lapply(.,function(x) t(apply(x,2,summ_samples))) %>% 
    do.call("rbind",.) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    tidyr::separate(rowname,":",into=c("beta","group")) %>% 
    dplyr::select(-beta) %>% 
    dplyr::mutate(group=gsub("age_group","agegroup",group)) %>% 
    tidyr::separate(group,"_",into=c("type","group")) %>% 
    dplyr::mutate(group=if_else(is.na(group),"Overall",group)) %>% 
    dplyr::group_by(type,group) %>% 
    dplyr::mutate(n_beta=if_else(row_number()==1,"A","B")) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(beta_med=`50%`,beta_lb=`2.5%`,beta_ub=`97.5%`) #%>% 
    # tidyr::pivot_wider(names_from=n_beta,values_from=c(beta_med,beta_lb,beta_ub,prob1))
  return(x)
}


