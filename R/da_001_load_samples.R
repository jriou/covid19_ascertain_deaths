#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: load samples
#:::::::::::::::::::::::::::::

da_001_load_samples <- function() {
  # samples from the INLA model without temperature
  samples_base = readRDS("samples/pois.samples.2022") %>% 
    as_tibble() %>% 
    dplyr::filter(year>=2020)
  
  # samples from the INLA model with temperature
  samples_temp = readRDS("samples/pois.samples.withtemperature") %>% 
    as_tibble() %>% 
    dplyr::filter(year>=2020)
  
  return(list(samples_base = samples_base,
              samples_temp = samples_temp))
}


