# Ascertainment of laboratory-confirmed COVID-19 deaths in Switzerland until 30 January 2022
# Julien Riou, Anthony Hauser, Garyfallos Konstantinoudis
# Started 2022-02-17

end_date = as.Date("2022-01-30")
source("R/da_setup.R")

if(FALSE) { # ignored upon sourcing
  
  # Block 0: apply model to estimate excess mortality from https://www.nature.com/articles/s41467-022-28157-3
  
  # Block 1: load samples and laboratory-confirmed deaths ----
  samp = da_001_load_samples()
  labd = da_002_load_lab_deaths(end_date)
  
  # Block 2: data management ----
  samp = da_101_clean_samples(samp)
  labd = da_102_clean_lab_deaths(labd)
  samp = da_103_merge(samp,labd)
  
  # Save point
  saveRDS(samp,"savepoint/merged_samples2.rds")
  samp = readRDS("savepoint/merged_samples2.rds")
  
  # Optional: reduce samples during development
  if(FALSE) {
    samp$samples_base = dplyr::filter(samp$samples_base,it<=100)
    samp$samples_temp = dplyr::filter(samp$samples_temp,it<=100)
  }
  
  # Block 2: summarize ----
  summ_all_base = da_201_summarise_by(samp$samples_base,by=NULL)
  summ_all_temp = da_201_summarise_by(samp$samples_temp,by=NULL)
  summ_week_base = da_201_summarise_by(samp$samples_base,by=c("week"))
  summ_week_temp = da_201_summarise_by(samp$samples_temp,by=c("week"))
  summ_phase_base = da_201_summarise_by(samp$samples_base,by=c("phase"))
  summ_phase_canton = da_201_summarise_by(samp$samples_base,by=c("phase","canton"))
  summ_week_age_base = da_201_summarise_by(samp$samples_base,by=c("phase","week","age_group"))
  summ_week_canton_base = da_201_summarise_by(samp$samples_base,by=c("phase","week","canton"))
  summ_week_canton_age_base = da_201_summarise_by(samp$samples_base,by=c("phase","week","canton","age_group"))
  
  # Save point
  save(list=ls(pattern = "summ_"),file="savepoint/summ2.Rdata")
}

# Start from save point upon sourcing
load("savepoint/summ2.Rdata")


# Block 3: descriptive figures ----

# Laboratory-confirmed deaths and excess death over time
summ_week_base %>% 
  da_301_summary_plot()
summ_week_temp %>% 
  da_301_summary_plot()
summ_week_canton_base %>% 
  dplyr::filter(canton=="ZH") %>% 
  da_301_summary_plot()

# Linear model base (ignoring uncertainty for now)
summ_week_base %>% 
  ggplot() +
  geom_point(aes(x=labo_deaths,y=deaths,colour=week)) 
summary(lm(deaths ~ labo_deaths,data=summ_week_base))
summary(lm(deaths ~  exp_deaths_med + labo_deaths -1,data=summ_week_base))

# Linear model by canton
summ_week_canton_base %>% 
  ggplot() +
  geom_point(aes(x=labo_deaths,y=deaths,colour=week)) +
  facet_wrap(~ canton,scales="free")
summary(lm(deaths ~ exp_deaths_med:canton + labo_deaths:canton -1,data=summ_week_canton_base))

# Linear model by age
summ_week_age_base %>% 
  ggplot() +
  geom_point(aes(x=labo_deaths,y=deaths,colour=age_group)) +
  facet_wrap(~ age_group,scales="free")
summary(lm(deaths ~ exp_deaths_med:age_group + labo_deaths:age_group -1,data=summ_week_age_base))


# Block 4: regression and Bayesian model averaging ----

# Regression and Bayesian model averaging procedure
if(FALSE) {
  source("R/NIMBLE_BMA.R") 
  source("R/Combine_Samples.R")
}

# Load outputs from the regression and Bayesian model averaging procedure
# regbma = readRDS("savepoint/combined_samples_trun_temperature")
# 
# # Format outputs
# summ_regbma = da_401_format_regbma(regbma)
# 
# # Plot
# da_402_plot_regbma(summ_regbma)


# Block 5: multilevel regression and BMA ----

# Load outputs from the multilevel regression and Bayesian model averaging procedure
regbma2 = readRDS("savepoint/combined_samples_trun_temperature")

# Format outputs
summ_regbma2 = da_403_format_regbma2(regbma2)

# Plot
da_404_plot_regbma(summ_regbma2)

regbma2$phase
