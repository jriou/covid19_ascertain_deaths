# Ascertainment of laboratory-confirmed COVID-19 deaths in Switzerland 
# Julien Riou, Anthony Hauser, Garyfallos Konstantinoudis
# Started 2022-02-17

end_date = as.Date("2022-04-19")
source("R/da_setup.R")

if(FALSE) { # ignored upon sourcing
  
  # Block 0: compute excess mortality (from https://www.nature.com/articles/s41467-022-28157-3)
  if(FALSE) {
    da_001_compute_samples()
  }
  
  # Block 1: samples of expected deaths from historical trends ----
  samp = da_001_load_samples()
  samp = da_101_clean_samples(samp)
  samp = da_104_get_excess(samp)
  
  # Block 2: laboratory-confirmed deaths from the BAG ----
  if(FALSE) {
    labd = da_002_load_lab_deaths()
    labd = da_102_clean_lab_deaths(labd)
    writeRDS(labd,file="savepoint/labd_2022-04-19.rds")
  }
  labd = readRDS("savepoint/labd_2022-04-19.rds")
  
  # Block 3: merge
  merg = da_103_merge(samp,labd)
  
  # Save point
  saveRDS(merg,"savepoint/merged_samples4.rds")
  merg = readRDS("savepoint/merged_samples4.rds")
  
  # Optional: reduce samples during development
  if(FALSE) {
    merg = dplyr::filter(merg,id_loop<=100)
  }
  
  # Block 2: summarize ----
  summ_all_temp = da_201_summarise_by(merg,by=NULL)
  summ_all_temp_corr = da_202_summarise_by_corr(merg,by=NULL)
  
  summ_week_temp = da_201_summarise_by(merg,by=c("week"))
  summ_week_temp_corr = da_202_summarise_by_corr(merg,by=c("week"))
  
  summ_age_temp = da_201_summarise_by(merg,by=c("age_group"))
  summ_age_temp_corr = da_202_summarise_by_corr(merg,by=c("age_group"))
  
  summ_phase_temp = da_201_summarise_by(merg,by=c("phase"))
  summ_phase_temp_corr = da_202_summarise_by_corr(merg,by=c("phase"))
  
  summ_week_canton_temp = da_201_summarise_by(merg,by=c("phase","week","canton"))
  summ_week_canton_temp_corr = da_202_summarise_by_corr(merg,by=c("phase","week","canton"))
  
  summ_week_age_temp = da_201_summarise_by(merg,by=c("phase","week","age_group"))
  summ_week_age_temp_corr = da_202_summarise_by_corr(merg,by=c("phase","week","age_group"))
  
  # Save point
  save(list=ls(pattern = "summ_"),file="savepoint/summ4.Rdata")
}

# Start from save point upon sourcing
load("savepoint/summ4.Rdata")


# Block 3: descriptive figures ----

# Laboratory-confirmed deaths and excess death over time
summ_week_temp %>% 
  da_301_summary_plot()
summ_week_temp_corr %>% 
  da_302_summary_plot_corr()


summ_week_age_temp %>% 
  dplyr::filter(age_group=="80+") %>% 
  da_301_summary_plot()
summ_week_age_temp_corr %>% 
  dplyr::filter(age_group=="80+") %>% 
  da_302_summary_plot_corr()


summ_week_canton_temp_corr %>% 
  dplyr::filter(canton=="AG") %>% 
  da_302_summary_plot_corr()

# GLM development (ignoring uncertainty for now)
if(FALSE) {
  summ_week_temp %>% 
    ggplot() +
    geom_point(aes(x=labo_deaths,y=deaths,colour=week)) 
  summary(lm(deaths ~ exp_deaths_med + labo_deaths,data=summ_week_temp))
  # force no intercept
  summary(lm(deaths ~  exp_deaths_med + labo_deaths - 1,data=summ_week_temp))
}


# Block 4: GLM and Bayesian model averaging ----

# Regression and Bayesian model averaging procedure
if(FALSE) {
  source("R/NIMBLE_BMA.R") 
  source("R/Combine_Samples.R")
}

# Block 5: check outputs ----

if(FALSE) {
  
  # Load outputs from the multilevel regression and Bayesian model averaging procedure
  regbma = readRDS("savepoint/combined_samples_trun_temperature_corrected_OV")
  
  # Format outputs
  summ_regbma  = da_403_format_regbma2(regbma)
  
  # Plot
  da_404_plot_regbma(summ_regbma)
  
  regbma2$phase
}