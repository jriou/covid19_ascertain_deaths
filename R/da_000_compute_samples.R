#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: compute expected mortality
#:::::::::::::::::::::::::::::


da_000_compute_samples <- function() {
  
  # 1 - download and format data ----
  
  if(FALSE) {
  ## public holidays in Switzerland by canton
  source("R/pr_001_Covariates_BankHol.R")
  ## weekly mean temperature by canton
  source("R/pr_002_Covariates_Temperature.R")
  ## observed deaths by canton
  source("R/pr_003_Outcome_Deaths.R")
  
  
  # 2 - model for population trends ----
  
  ## model comparison with cross validation
  source("R/pr_004_Population_Models.R")
  rmarkdown::render("R/pr_005_Population_CrossValidation.Rmd", output_dir = controls$savepoint)
  
  ## apply selected model
  source("R/pr_006_Population_Predictions.R")
  
  ## interpolate population from yearly to weekly
  source("R/pr_007_Population_Interpolation.R")
  }
  
  # 3 - model for expected deaths ----
  source("R/pr_102_ModelRun.R")
  
}