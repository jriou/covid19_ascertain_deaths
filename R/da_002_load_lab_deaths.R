#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: load laboratory-confirmed deaths
#:::::::::::::::::::::::::::::

da_002_load_lab_deaths <- function(limit) {
  falldetail_raw = bag.epi.io::read_dump_share(
      object_name = "ncov2019_falldetail_cases-",
      full_dump_directory = paste0(path_mm_root, limit+15, "_INTERN/"))
  
  labo_deaths = falldetail_raw %>% 
    # select deaths until limit
    dplyr::filter(pttod==1,!is.na(pttoddat),pttoddat<=limit) %>% 
    # rm FL
    dplyr::filter(ktn != "FL") %>%
    # filter out missing age
    dplyr::filter(!is.na(altersjahr)) %>%
    # create age group
    dplyr::mutate(age_group = cut(altersjahr,
                                  breaks = breaks_age_classes,
                                  labels = age_classes,
                                  right = FALSE)) %>%
    # create sex
    dplyr::mutate(sex = ifelse(sex == "Weiblich", "female", "male")) %>% 
    # retain only useful variables
    dplyr::select(fall_id,
                  canton = ktn, 
                  pttoddat,
                  age_group, 
                  sex
    ) 
   
  return(labo_deaths)
}


