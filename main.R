# Ascertainment of laboratory-confirmed COVID-19 deaths in Switzerland 
# Julien Riou, Anthony Hauser, Garyfallos Konstantinoudis
# Started 2022-02-17

# controls
end_date = as.Date("2022-04-19")
controls = list(source=TRUE,
                compute_sample=FALSE,
                update_bag_data=FALSE,
                merge_samples_bag_data=TRUE,
                summarise_merg=FALSE,
                compute_glm=TRUE,
                savepoint=paste0("savepoint_",end_date))

# set-up
source("R/da_setup.R")

if(controls$source) { # ignored upon sourcing
  
  # Block 0: compute excess mortality (from https://www.nature.com/articles/s41467-022-28157-3)
  if(controls$compute_sample) {
    da_000_compute_samples()
  }
  
  # Block 1: laboratory-confirmed deaths from the BAG ----
  if(controls$update_bag_data) {
    labd = da_002_load_lab_deaths()
    labd = da_102_clean_lab_deaths(labd)
    saveRDS(labd,file=file.path(controls$savepoint,"labd.rds"))
  } else {
    labd = readRDS(file.path(controls$savepoint,"labd.rds"))
  }
  
  # Block 2: samples of expected deaths from historical trends ----
  if(controls$merge_samples_bag_data) {
    samp = da_001_load_samples()
    samp = da_101_clean_samples(samp)
    samp = da_104_get_excess(samp)
  }
  
  # Block 3: merge ----
  if(controls$merge_samples_bag_data) {
    merg = da_103_merge(samp,labd)
    saveRDS(merg,file=file.path(controls$savepoint,"merged_samples.rds"))
  }
  merg = readRDS(file.path(controls$savepoint,"merged_samples.rds"))
  
  
  # Block 4: summarize ----
  if(controls$summarise_merg) {
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
    save(list=ls(pattern = "summ_"),file=file.path(controls$savepoint,"summ.Rdata"))
    load(file.path(controls$savepoint,"summ.Rdata"))
    
    
    # Block 5: descriptive figures ----
    
    # Laboratory-confirmed deaths and excess death over time
    summ_week_temp %>% 
      da_301_summary_plot()
    summ_week_temp_corr %>% 
      da_302_summary_plot_corr()
    
    # Select age group
    summ_week_age_temp %>% 
      dplyr::filter(age_group=="80+") %>% 
      da_301_summary_plot()
    summ_week_age_temp_corr %>% 
      dplyr::filter(age_group=="80+") %>% 
      da_302_summary_plot_corr()
    
    # Select canton
    summ_week_canton_temp_corr %>% 
      dplyr::filter(canton=="AG") %>% 
      da_302_summary_plot_corr()
    summ_week_canton_temp_corr %>% 
      dplyr::filter(canton=="BE") %>% 
      da_302_summary_plot_corr()
  }
  
  # Block 6: links between labo_deaths and observed deaths (ignoring uncertainty for now)
  
  if(controls$source) {
    summ_week_temp %>% 
      ggplot() +
      geom_point(aes(x=labo_deaths,y=deaths,colour=week)) 
    summary(lm(deaths ~ exp_deaths_med + labo_deaths,data=summ_week_temp))
    # force no intercept
    summary(lm(deaths ~  exp_deaths_med + labo_deaths - 1,data=summ_week_temp))
  }
  
  
  # Block 7: GLM and Bayesian model averaging ----
  
  if(controls$compute_glm) {
    # Regression and Bayesian model averaging procedure
    source("R/pr_201_Model_BetaModel.R")
    pr_201_Model_BetaModel(by=NULL,overdispersion=TRUE,correction_expected = TRUE)
    pr_201_Model_BetaModel(by="age_group",overdispersion=TRUE,correction_expected = TRUE)
    pr_201_Model_BetaModel(by="canton_name",overdispersion=TRUE,correction_expected = TRUE)
    pr_201_Model_BetaModel(by="phase",overdispersion=TRUE,correction_expected = TRUE)
    
    # Combine results
    source("R/pr_202_Model_CombineSamples.R")
    
    # Run various checks
    source("R/pr_301_Checks_Overdispersion.R")
    source("R/pr_302_Checks_PosteriorPredictive.R")
    source("R/pr_303_Checks_CompareOVmodels.R")
  }
  
  # Block 8: check outputs ----
  
  # Load outputs from the multilevel regression and Bayesian model averaging procedure
  regbma = readRDS(file.path(controls$savepoint,"combined_samples_trun_temperature_corrected_OV"))
  
  # Format outputs
  summ_regbma  = da_403_format_regbma2(regbma)
  
  # Plot
  da_404_plot_regbma(summ_regbma)
  
}