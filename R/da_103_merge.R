#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: merge samples and laboratory-confirmed deaths
#:::::::::::::::::::::::::::::

da_103_merge <- function(dat_samp,dat_labd) {
  # samples from the INLA model with temperature
  samples_temp = dat_samp %>% 
    dplyr::left_join(dat_labd) %>% 
    dplyr::mutate(labo_deaths=ifelse(is.na(labo_deaths),0,labo_deaths)) %>% 
    dplyr::left_join(phases,by="week") %>% 
    dplyr::filter(!is.na(phase)) %>%
    dplyr::mutate(it=paste0(it_exp,"_",it_pop))
  
  return(samples_temp)
}


