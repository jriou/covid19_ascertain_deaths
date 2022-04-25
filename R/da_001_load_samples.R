#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: load samples
#:::::::::::::::::::::::::::::

da_001_load_samples <- function() {
  # samples of expected deaths and corresponding population from the INLA model with temperature
  samples_temp = readRDS("savepoint/pois.samples.temp.bma")
  samples_temp = samples_temp[,unique(names(samples_temp))] %>% 
    as_tibble() %>% 
    dplyr::filter(year>=2020)
  
  return(samples_temp)
}


