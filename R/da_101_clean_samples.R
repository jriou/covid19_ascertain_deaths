#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: clean samples
#:::::::::::::::::::::::::::::

da_101_clean_samples <- function(dat) {
  # samples from the INLA model with temperature
  samples_temp = dat %>% 
    dplyr::rename(canton_id=ID_space,
                  canton_name=NAME.POP,
                  age=age.group,
                  week_raw=EURO_LABEL) %>%
    dplyr::left_join(cantons_ids) %>% 
    dplyr::mutate(week=ISOweek::ISOweek2date(paste0(week_raw,"-1")),
                  age_group=factor(age,
                                   levels=c("less40","40-59","60-69","70-79","80plus"),
                                   labels=age_classes),
                  canton=as.character(canton),
                  canton=if_else(canton_id=="CH033","AG",canton)) %>%
    dplyr::filter(canton != "FL") %>%
    tidyr::pivot_longer(cols=starts_with("V")) %>% 
    dplyr::mutate(it=as.numeric(gsub("V","",name)),
                  excess=deaths-value) %>% 
    dplyr::select(year,week,
                  canton,canton_id,canton_name,
                  age_group,sex,
                  it,deaths,exp_deaths=value,excess,
                  population)
  
  return(list(#samples_base = samples_base,
              samples_temp = samples_temp))
}


