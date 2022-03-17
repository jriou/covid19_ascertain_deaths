

# Created 03.03.2022

# NIMBLE BMA + SMOOTHING


########################################################################


library(nimble)
library(tidyr)
library(doParallel) 
library(fastDummies) 
library(dplyr)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

end_date = as.Date("2021-12-31")
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


# select to use the predictions including the temperature
temperature <- TRUE
# select whether to exclude phase 7 or not (TRUE includes it)
phase7 <- FALSE


##
##
## generic function to put in parallel, sets the mcmc setting, cleans the data, and compiles/runs the model

nimble_sam <- function(J){
  
  
  # MCMC setting
  ni <- 100000  
  nt <- 90     
  nb <- 10000  
  
  dat_fin %>% filter(it %in% J) -> dat
  dat %>% dplyr::select(starts_with("exp_int")|starts_with("labo_int")) -> X
  
  if(ncol(X) == 1){
    X = dat %>% dplyr::select(starts_with("exp_")|starts_with("labo_"))
    X <- X[,-1]
  }else{
    X <- X[,-c(1,2)]
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
    M = ncol(X)/2,
    b = 100, 
    a = 0.000001, 
    Tot = Tot
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
    
    for(k in 1:M){
      beta[k] ~ T(dnorm(mean.beta[1], sd = sd.beta[1]), 0, b)
    }
    
    for(k in (M+1):K){
      beta[k] ~ T(dnorm(mean.beta[2], sd = sd.beta[2]), 0, b)
    }
    
    if(Tot){
      mean.beta[1:2] <- c(0,0) 
      sd.beta[1:2] <- c(10,5) 
    }else{
      mean.beta[1] ~ T(dnorm(0, sd = 2), 0, b)
      sd.beta[1] ~ dexp(rate = 1)
      
      mean.beta[2] ~ T(dnorm(0, sd = 5), 0, b)
      sd.beta[2] ~ dexp(rate = 0.1)
    }
  }
)


########
########
# Total, age, canton and phase

by <- NULL # this corresponds to total
by <- "age_group"
by <- "canton"
by <- "phase"

# select samples and exclude phase 7


if(temperature == TRUE){
  dat <- samp$samples_temp
  nam <- "temperature"
}else{
  dat <- samp$samples_base
  nam <- "no_temperature"
}

if(phase7==FALSE){
  dat %>% filter(it %in% 1:200, !(phase %in% 7)) -> dat
}else{
  dat %>% filter(it %in% 1:200) -> dat
}

dat %>% 
  group_by_at(vars("week", by, "it")) %>% 
  summarise(deaths=sum(deaths), 
            exp_deaths = sum(exp_deaths),
            labo_deaths = sum(labo_deaths)) -> dat



if(is.null(by) == FALSE){
  
  Tot <- FALSE
  params <- c("beta", "mean.beta", "sd.beta")
  dummies_interaction <- dummy_cols(dat[,by])[,-1]
  dummies_interaction * rep(dat$exp_deaths, ncol(dummies_interaction)) -> exp_deaths_interaction
  dummies_interaction * rep(dat$labo_deaths, ncol(dummies_interaction)) -> labo_deaths_interaction
  
  
  if(by == "age_group"){
    colnames(exp_deaths_interaction) <- c("exp_int_0_39", "exp_int_40_59", "exp_int_60_69", "exp_int_70_79", "exp_int_80")
    colnames(labo_deaths_interaction) <- c("labo_int_0_39", "labo_int_40_59", "labo_int_60_69", "labo_int_70_79", "labo_int_80")
  }
  
  if(by == "canton" | by == "phase"){
    colnames(exp_deaths_interaction) <- 
      paste0("exp_int_", colnames(exp_deaths_interaction))
    colnames(labo_deaths_interaction) <- 
      paste0("labo_int_", colnames(labo_deaths_interaction))
  }
  
  dat_fin <- cbind(dat, exp_deaths_interaction, labo_deaths_interaction)
  
}else{
  dat_fin <- dat
  by <- "Total"
  Tot <- TRUE
  params <- c("beta")
}



# summary(glm(deaths ~ exp_deaths:age_group + labo_deaths:age_group -1, family = poisson(link = "identity"), data = dat))


# t_0 <- Sys.time()
# sm <- nimble_sam(1)
# t_1 <- Sys.time() 

# t_0 <- Sys.time()
# lapply(1:200, nimble_sam) -> sm
# t_1 <- Sys.time() 

t_0 <- Sys.time()
no_cores <- 10 
cl <- makeCluster(no_cores) 

clusterExport(cl, c("dat_fin", "mod", "Tot", "params"))

clusterEvalQ(cl, {
  library(nimble)
  library(dplyr)
  library(tidyr)
})

result <- parLapply(cl, 1:200, nimble_sam)  
stopCluster(cl) 
t_1 <- Sys.time()
t_1 - t_0

saveRDS(result, file = paste0("savepoint/SamplesBMAtrun_", by, "_", nam))

# by = NULL, takes ~20min
# by = age_group, takes ~1h
# by = canton, takes ~12h
# by = phase, takes ~30min


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################




