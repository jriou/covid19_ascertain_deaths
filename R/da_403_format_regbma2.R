#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_403_format_regbma2 <- function(x) {
  r = x %>% 
    lapply(.,function(x) t(apply(x,2,summ_samples))) %>% 
    do.call("rbind",.) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    tidyr::separate(rowname,":",into=c("beta","group")) %>% 
    tidyr::separate(beta,"\\[",into=c("par","num")) %>% 
    dplyr::mutate(group=gsub("age_group","agegroup",group)) %>% 
    tidyr::separate(group,"_",into=c("type","group")) %>% 
    dplyr::mutate(group=if_else(is.na(group),"Overall",group),
                  group=if_else(par!="beta","Hyperparameters",group),
                  type=if_else(group=="Overall","tot",type)) %>% 
    dplyr::group_by(type,group) %>% 
    dplyr::mutate(n_beta=if_else(row_number()==1,"beta_2","beta_1")) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(beta_med=`50%`,beta_lb=`2.5%`,beta_ub=`97.5%`) %>% 
    dplyr::select(-num)
  return(r)
}


