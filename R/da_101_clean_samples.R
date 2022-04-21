#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: clean samples
#:::::::::::::::::::::::::::::

da_101_clean_samples <- function(dat) {
  # samples from the INLA model with temperature
  dat2 = dat %>% 
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
    dplyr::ungroup()
    # extract pop and pivot
    samples_pop = dat2 %>% 
      dplyr::select(-starts_with("V")) %>% 
      tidyr::pivot_longer(starts_with("pop"),names_to="it_pop",values_to="population") %>% 
      dplyr::mutate(it_pop=as.numeric(gsub("pop_","",it_pop)))
    # extract exp and pivot
   samples_exp = dat2 %>% 
      dplyr::select(-starts_with("pop")) %>% 
      tidyr::pivot_longer(starts_with("V"),names_to="it",values_to="exp_deaths") %>% 
      dplyr::mutate(it=gsub("V","",it)) %>% 
      tidyr::separate(it,"_",into=c("it_exp","it_pop")) %>% 
      dplyr::mutate(it_pop=as.numeric(it_pop),
                    it_exp=as.numeric(it_exp))
    # merge
    samples_temp = samples_exp %>% 
      dplyr::left_join(samples_pop) %>% 
      dplyr::select(year,week,
                    canton,canton_id,canton_name,
                    age_group,sex,
                    it_exp,it_pop,deaths,exp_deaths,
                    population)

  return(samples_temp = samples_temp)
}


