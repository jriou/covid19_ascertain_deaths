# Ascertainment of laboratory-confirmed COVID-19 deaths in Switzerland until 30 January 2022
# Julien Riou, Anthony Hauser, Garyfallos Konstantinoudis
# Started 2022-02-17

end_date = as.Date("2022-01-30")
source("R/da_setup.R")

if(FALSE) { # ignored upon sourcing
  
  # Block 0: load all
  samp = da_001_load_samples()
  labd = da_002_load_lab_deaths(end_date)
  
  # Block 1: data management
  samp = da_101_clean_samples(samp)
  labd = da_102_clean_lab_deaths(labd)
  samp = da_103_merge(samp,labd)
  
  # Save point
  saveRDS(samp,"savepoint/merged_samples.rds")
}

# Start from save point upon sourcing
samp = readRDS("savepoint/merged_samples.rds")

# Block 2: summarise
summ_all_base = da_201_summarise_by(samp$samples_base,by=NULL)
summ_all_temp = da_201_summarise_by(samp$samples_temp,by=NULL)
summ_week_base = da_201_summarise_by(samp$samples_base,by=c("week"))
summ_week_temp = da_201_summarise_by(samp$samples_temp,by=c("week"))
summ_phase_base = da_201_summarise_by(samp$samples_base,by=c("phase"))
summ_phase_temp = da_201_summarise_by(samp$samples_temp,by=c("phase"))
# summ_phase_age_sex_base = da_201_summarise_by(samp$samples_base,by=c("phase","age_group","sex"))
summ_phase_canton = da_201_summarise_by(samp$samples_base,by=c("phase","canton"))
# summ_phase_canton_age_sex_base = da_201_summarise_by(samp$samples_base,by=c("phase","canton","age_group","sex"))
# summ_week_age_sex_base = da_201_summarise_by(samp$samples_base,by=c("week","age_group","sex"))
summ_week_age_base = da_201_summarise_by(samp$samples_base,by=c("week","age_group"))
summ_week_canton_base = da_201_summarise_by(samp$samples_base,by=c("week","canton"))
# summ_week_canton_age_sex_base = da_201_summarise_by(samp$samples_base,by=c("week","canton","age_group","sex"))
summ_week_canton_age_base = da_201_summarise_by(samp$samples_base,by=c("week","canton","age_group"))
