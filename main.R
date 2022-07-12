# Ascertainment of laboratory-confirmed COVID-19 deaths in Switzerland 
# Julien Riou, Anthony Hauser, Garyfallos Konstantinoudis
# Started 2022-02-17

# controls
end_date = as.Date("2022-06-15")
controls = list(source=TRUE,
                compute_sample=FALSE,
                update_bag_data=FALSE,
                merge_samples_bag_data=FALSE,
                summarise_merg=FALSE,
                compute_glm=FALSE,
                get_outputs=TRUE,
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
    samp = da_001_load_samples(); cat("1")
    samp = da_101_clean_samples(samp); cat("2")
    samp = da_104_get_excess(samp); cat("3")
  }
  
  # Block 3: merge ----
  if(controls$merge_samples_bag_data) {
    merg = da_103_merge(samp,labd); cat("4")
    saveRDS(merg,file=file.path(controls$savepoint,"merged_samples.rds"))
  }
  
  # Block 4: summarize ----
  if(controls$summarise_merg) {
    merg = readRDS(file.path(controls$savepoint,"merged_samples.rds"))
    
    # summarize excess and related metrics
    summ_all_temp = da_201_summarise_by(merg,by=NULL)
    summ_year_temp = da_201_summarise_by(merg,by="year")
    summ_week_temp = da_201_summarise_by(merg,by=c("week"))
    summ_age_temp = da_201_summarise_by(merg,by=c("age_group"))
    summ_age_phase_temp = da_201_summarise_by(merg,by=c("age_group","phase"))
    summ_phase_temp = da_201_summarise_by(merg,by=c("phase"))
    summ_week_canton_temp = da_201_summarise_by(merg,by=c("phase","week","canton"))
    summ_canton_temp = da_201_summarise_by(merg,by=c("canton"))
    summ_phase_canton_temp = da_201_summarise_by(merg,by=c("phase","canton"))
    summ_week_age_temp = da_201_summarise_by(merg,by=c("phase","week","age_group"))

    # correlation between excess and lab deaths
    summ_pearson_week = da_203_pearson(merg,by="week")
    summ_pearson_week_age = da_203_pearson(merg,by=c("week","age_group"))
    summ_pearson_week_canton = da_203_pearson(merg,by=c("week","canton"))

    # Save point
    save(list=ls(pattern = "summ_"),file=file.path(controls$savepoint,"summ.Rdata"))
  }
  
  
  # Block 5: descriptive figures ----
  if(FALSE) {
    load(file.path(controls$savepoint,"summ.Rdata"))
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
    summ_week_canton_temp_corr %>% 
      dplyr::filter(canton=="UR") %>% 
      da_302_summary_plot_corr()
  }
  
  # Block 6: explore links between labo_deaths and observed deaths (ignoring uncertainty for now)
  if(FALSE) {
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
    pr_201_Model_BetaModel(model_by=NULL,overdispersion=TRUE,correction_expected = TRUE)
    pr_201_Model_BetaModel(model_by="age_group",overdispersion=TRUE,correction_expected = TRUE)
    pr_201_Model_BetaModel(model_by="canton_name",overdispersion=TRUE,correction_expected = TRUE)
    pr_201_Model_BetaModel(model_by="phase",overdispersion=TRUE,correction_expected = TRUE)
    
    # Combine results
    source("R/pr_202_Model_CombineSamples.R")
    
    # Run various checks
    source("R/pr_301_Checks_Overdispersion.R")
    source("R/pr_302_Checks_PosteriorPredictive.R")
    source("R/pr_303_Checks_CompareOVmodels.R")
    
    # Load outputs from the multilevel regression and Bayesian model averaging procedure
    regbma = readRDS(file.path(controls$savepoint,"combined_samples_trun_temperature_corrected_OV_0.001.gz"))
    
    # Format outputs
    summ_regbma  = da_403_format_regbma2(regbma)
    
    # Compute deaths indirectly caused or averted (beta_2*expected)
    merg = readRDS(file.path(controls$savepoint,"merged_samples.rds"))
    summ_all_temp_indirect = da_204_summarise_indirect(merg,regbma)
    
    # Save point
    save(summ_regbma,summ_all_temp_indirect,file=file.path(controls$savepoint,"summ_bma.Rdata"))
  }
  
  # Block 8: load outputs ----
  if(controls$get_outputs) {
    # load summaries
    merg = readRDS(file.path(controls$savepoint,"merged_samples.rds"))
    load(file.path(controls$savepoint,"summ.Rdata"))
    load(file.path(controls$savepoint,"summ_bma.Rdata"))
  }
}