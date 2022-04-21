#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_403_format_regbma2 <- function(x) {
  r = x %>% 
    lapply(.,function(x) t(apply(x,2,summ_samples))) %>% 
    do.call("rbind",.)
  s =
    r %>% 
    tibble::as_tibble() %>%
    dplyr::mutate(rowname=rownames(r)) %>% 
    tidyr::separate(rowname,":",into=c("beta","group")) %>% 
    tidyr::separate(beta,"\\[",into=c("par","num")) %>% 
    dplyr::mutate(group=gsub("age_group","agegroup",group)) %>% 
    dplyr::mutate(group=gsub("canton_name","canton",group)) %>% 
    tidyr::separate(group,"_",into=c("type","group")) %>%
    dplyr::mutate(type=if_else(is.na(type),lag(type,4),type),
                  type=if_else(type %in% c("exp","lab"),"overall",type),
                  group=if_else(is.na(group) & type!="overall","Hyperparameters",group),
                  group=if_else(is.na(group) & type=="overall","Overall",group)) %>% 
    dplyr::group_by(type,group) %>% 
    dplyr::mutate(n_beta=if_else(row_number()==1,"beta_2","beta_1")) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(beta_med=`50%`,beta_lb=`2.5%`,beta_ub=`97.5%`)  %>% 
    dplyr::select(-num) 
  return(s)
}


