#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: clean samples
#:::::::::::::::::::::::::::::

da_101_clean_samples <- function(dat) {
  # samples from the INLA model without temperature
  samples_base = dat$samples_base %>% 
    dplyr::rename(canton_id=ID_space,
                  canton_name=NAME.POP,
                  age=age.group,
                  week_raw=EURO_LABEL) %>%
    dplyr::left_join(cantons_ids) %>% 
    dplyr::mutate(week=ISOweek::ISOweek2date(paste0(week_raw,"-1")),
                  age_group=factor(age,
                                   levels=c("less40","40-59","60-69","70-79","80plus"),
                                   labels=age_classes),
                  canton=factor(canton)) %>% 
    tidyr::pivot_longer(cols=starts_with("V")) %>% 
    dplyr::mutate(it=as.numeric(gsub("V","",name)),
                  excess=deaths-value) %>% 
    dplyr::select(year,week,
                  canton,canton_id,canton_name,
                  age_group,sex,
                  it,deaths,exp_deaths=value,excess,
                  population)
  
  # samples from the INLA model with temperature
  samples_temp = dat$samples_temp %>% 
    dplyr::rename(canton_id=ID_space,
                  canton_name=NAME.POP,
                  age=age.group,
                  week_raw=EURO_LABEL) %>%
    dplyr::left_join(cantons_ids) %>% 
    dplyr::mutate(week=ISOweek::ISOweek2date(paste0(week_raw,"-1")),
                  age_group=factor(age,
                                   levels=c("less40","40-59","60-69","70-79","80plus"),
                                   labels=age_classes),
                  canton=factor(canton)) %>% 
    tidyr::pivot_longer(cols=starts_with("V")) %>% 
    dplyr::mutate(it=as.numeric(gsub("V","",name)),
                  excess=deaths-value) %>% 
    dplyr::select(year,week,
                  canton,canton_id,canton_name,
                  age_group,sex,
                  it,deaths,exp_deaths=value,excess,
                  population)
  
  return(list(samples_base = samples_base,
              samples_temp = samples_temp))
}


