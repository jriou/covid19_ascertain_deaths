#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: merge samples and laboratory-confirmed deaths
#:::::::::::::::::::::::::::::

da_103_merge <- function(dat_samp,dat_labd) {
  # samples from the INLA model without temperature
  samples_base = dat_samp$samples_base %>% 
    dplyr::left_join(dat_labd,by=c("week","canton","age_group","sex")) %>% 
    dplyr::mutate(labo_deaths=ifelse(is.na(labo_deaths),0,labo_deaths)) %>% 
    dplyr::left_join(phases,by="week") %>% 
    dplyr::filter(!is.na(phase))
  
  # samples from the INLA model with temperature
  samples_temp = dat_samp$samples_temp %>% 
    dplyr::left_join(dat_labd,by=c("week","canton","age_group","sex")) %>% 
    dplyr::mutate(labo_deaths=ifelse(is.na(labo_deaths),0,labo_deaths)) %>% 
    dplyr::left_join(phases,by="week") %>% 
    dplyr::filter(!is.na(phase))
  
  return(list(samples_base = samples_base,
              samples_temp = samples_temp))
}


