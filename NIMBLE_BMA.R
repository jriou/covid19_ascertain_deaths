

# Created 23.02.2022

# INLA for BMA


########################################################################


library(nimble)
library(tidyr)
library(doParallel) 
library(fastDummies) 


setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

end_date = as.Date("2022-01-30")
source("R/da_setup.R")

if(FALSE) { # ignored upon sourcing
  
  # Block 0: load all ----
  samp = da_001_load_samples()
  labd = da_002_load_lab_deaths(end_date)
  
  # Block 1: data management ----
  samp = da_101_clean_samples(samp)
  labd = da_102_clean_lab_deaths(labd)
  samp = da_103_merge(samp,labd)
  
  # Save point ----
  saveRDS(samp,"savepoint/merged_samples.rds")
}

# Start from save point upon sourcing ----
samp = readRDS("savepoint/merged_samples.rds")

# Reduce samples during development ----
if(FALSE) {
  samp$samples_base = dplyr::filter(samp$samples_base,it<=100)
  samp$samples_temp = dplyr::filter(samp$samples_temp,it<=100)
}


##
##
## generic function to put in parallel, sets the mcmc setting, cleans the data, and compiles/runs the model

nimble_sam <- function(J){
  
  params <- c("beta")
  
  # MCMC setting
  ni <- 50000  
  nt <- 40     
  nb <- 10000  
  
  dat_fin %>% filter(it %in% J) -> dat
  dat %>% dplyr::select(starts_with("exp_age")|starts_with("labo_age")) -> X
  
  if(ncol(X) == 1){
    X = dat %>% dplyr::select(starts_with("exp_")|starts_with("labo_"))
    X <- X[,-1]
  }
  
  inits <- list(
    beta = rep(0, times = ncol(X))
  )
  
  Dats = list(
    O = dat$deaths,  
    X.mat = X
  )
  
  Consts <-list(
    N = length(dat$deaths),
    K = ncol(X), 
    a = 0.000001
  )
  
  # define the model
  nimble_model <- nimbleModel(
    code = mod,
    data = Dats,
    constants = Consts,
    inits = inits,
    calculate = FALSE
  )
  
  Cmodel <- compileNimble(nimble_model)
  # Cmodel$calculate() 
  
  conf <- configureMCMC(nimble_model, monitors = params)
  MCMC <- buildMCMC(conf)
  
  # compile the MCMC
  cMCMC <- compileNimble(MCMC, project = Cmodel)
  
  # run the MCMC
  sampl <- runMCMC(cMCMC, niter = ni , nburnin = nb, thin = nt, samples = TRUE, summary = FALSE)
  return(sampl)
}



##
##
## The model

mod <- nimbleCode(
  {
    for (i in 1:N){
      O[i] ~ dpois(mu[i])                                
      mu[i] <- inprod(beta[1:K], X.mat[i,1:K]) + a
    }
    
    for(k in 1:K){
      beta[k] ~ dnorm(mean = 0, sd = 10)
    } 
  }
)


########
########
# Age, canton and phase

by <- NULL
by <- "age_group"
by <- "canton"
by <- "canton"

samp$samples_base %>% 
  group_by_at(vars("week", by, "it")) %>% 
  summarise(deaths=sum(deaths), 
            exp_deaths = sum(exp_deaths),
            labo_deaths = sum(labo_deaths)) -> dat

# select samples
dat %>% filter(it %in% 1:200) -> dat

if(is.null(by) == FALSE){
  
  dummies_interaction <- dummy_cols(dat$age_group)[,-1]
  dummies_interaction * rep(dat$exp_deaths, ncol(dummies_interaction)) -> exp_age_interaction
  dummies_interaction * rep(dat$labo_deaths, ncol(dummies_interaction)) -> labo_deaths_interaction
  
  
  if(by == "age_group"){
    colnames(exp_age_interaction) <- c("exp_age_0_39", "exp_age_40_59", "exp_age_60_69", "exp_age_70_79", "exp_age_80")
    colnames(labo_deaths_interaction) <- c("labo_age_0_39", "labo_age_40_59", "labo_age_60_69", "labo_age_70_79", "labo_age_80")
  }
  
  if(by == "canton"){
    colnames(exp_age_interaction) <- 
      gsub(pattern = ".data", replacement = "exp_age", colnames(exp_age_interaction))
    colnames(labo_deaths_interaction) <- 
      gsub(pattern = ".data", replacement = "labo_age", colnames(labo_deaths_interaction))
  }
  
  if(by == "phase"){
    colnames(exp_age_interaction) <- 
      gsub(pattern = ".data", replacement = "exp_age", colnames(exp_age_interaction))
    colnames(labo_deaths_interaction) <- 
      gsub(pattern = ".data", replacement = "labo_age", colnames(labo_deaths_interaction))
  }
  
  dat_fin <- cbind(dat, exp_age_interaction, labo_deaths_interaction)
  
}else{
  dat_fin <- dat
}


# summary(glm(deaths ~ exp_deaths:age_group + labo_deaths:age_group -1, family = poisson(link = "identity"), data = dat))


# 
# t_0 <- Sys.time()
# lapply(1:200, nimble_sam) -> sm
# t_1 <- Sys.time() 


no_cores <- 10 
cl <- makeCluster(no_cores) 

clusterExport(cl, c("dat_fin", "mod"))

clusterEvalQ(cl, {
  library(nimble)
  library(dplyr)
  library(tidyr)
})

result <- parLapply(cl, 1:200, nimble_sam)  
stopCluster(cl) 

# by = NULL, takes 18min



########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################


