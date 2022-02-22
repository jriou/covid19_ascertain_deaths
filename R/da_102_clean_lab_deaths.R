#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: clean laboratory-confirmed deaths
#:::::::::::::::::::::::::::::

da_102_clean_lab_deaths <- function(dat) {
  labo_deaths = dat %>% 
    # create week of death using isoweek
    dplyr::mutate(isoweek=ISOweek::date2ISOweek(pttoddat),
                  isoweek=paste0(substr(isoweek,1,9),"1"),
                  week=ISOweek::ISOweek2date(isoweek)) %>%
    # summarise by canton, age, sex, week
    dplyr::group_by(canton,age_group,sex,week) %>% 
    dplyr::summarise(labo_deaths=n()) %>% 
    dplyr::ungroup()
    
  return(labo_deaths)
}


