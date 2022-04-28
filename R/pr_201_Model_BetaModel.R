#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: NIMBLE BMA + SMOOTHING
#:::::::::::::::::::::::::::::

pr_201_Model_BetaModel = function(by, overdispersion, correction_expected) {
  # load data
  merg = readRDS(file.path(controls$savepoint,"merged_samples.rds"))
  # select to use the predictions including the temperature
  temperature <- TRUE
  # select whether to exclude phase 7 or not (TRUE includes it)
  phase7 <- FALSE
  select.rate.param <- 0.001 # 0.1 is set for the main one
  
  
  # by <- NULL # this corresponds to total
  # by <- "age_group"
  # by <- "canton_name"
  # by <- "phase"
  
  
  ## generic function to put in parallel, sets the mcmc setting, cleans the data, and compiles/runs the model
  nimble_sam <- function(J){
    
    # MCMC setting
    ni <- 100000  
    nt <- 90     
    nb <- 10000  
    
    dat_fin %>% filter(it %in% ran.sam.it[J]) -> dat
    dat %>% dplyr::select(starts_with("exp_int")|starts_with("labo_int")) -> X
    
    if(ncol(X) == 1){
      X = dat %>% dplyr::select(starts_with("exp_")|starts_with("labo_"))
      X <- X[,-1]
    }else{
      X <- X[,-c(1,2)]
    }
    
    if(overdispersion == TRUE){
      
      inits <- list(
        beta = rep(0, times = ncol(X)), 
        u = rep(10, times = length(dat$deaths)),
        sd.over = 1
      )
      
      Dats = list(
        O = dat$deaths,  
        X.mat = X, 
        one = 1
      )
      
      Consts <-list(
        N = length(dat$deaths),
        K = ncol(X), 
        M = ncol(X)/2,
        b = 100, 
        Tot = Tot, 
        rate.param = select.rate.param # previously as 0.1
      )
      
    }else{
      
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
        Tot = Tot, 
        a = 0.000001
      )
    }
    
    
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
        if(overdispersion){
          mu[i] <- inprod(beta[1:K], X.mat[i,1:K]) + u[i]
          u[i] ~ dnorm(0, sd = sd.over)
        }else{
          mu[i] <- inprod(beta[1:K], X.mat[i,1:K]) + a
        }
        
      }
      
      # add sum to zero contraints for u if we add the overdispersion parameter
      if(overdispersion){
        cond <- sum(u[1:N])
        one ~ dconstraint(cond = 0)
        sd.over ~ dexp(rate = rate.param)
      }else{
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

  
  if(temperature == TRUE){
    dat <- merg
    nam <- "temperature"
  }else{
    # this is in case we provide it with a list with the results without using the temperature
    dat <- merg$samples_base
    nam <- "no_temperature"
  }
  
  
  dat$it <- as.numeric(as.factor(dat$it))
  # take a random sample of the iterations
  set.seed(11)
  ran.sam.it <- sample(1:1000, size = 200)
  
  if(phase7==FALSE){
    dat %>% filter(it %in% ran.sam.it, !(phase %in% 7)) -> dat
  }else{
    dat %>% filter(it %in% ran.sam.it) -> dat
  }
  
  
  
  if(correction_expected == TRUE){
    dat %>% 
      group_by_at(vars("week", by, "it")) %>% 
      summarise(deaths=sum(deaths), 
                exp_deaths = sum(corr_exp_deaths),
                labo_deaths = sum(labo_deaths)) -> dat
    
    cor.nam <- "_corrected"
  }else{
    dat %>% 
      group_by_at(vars("week", by, "it")) %>% 
      summarise(deaths=sum(deaths), 
                exp_deaths = sum(exp_deaths),
                labo_deaths = sum(labo_deaths)) -> dat
    
    cor.nam <- ""
  }
  
  
  
  if(is.null(by) == FALSE){
    
    Tot <- FALSE
    
    if(overdispersion == TRUE){
      params <- c("beta", "mean.beta", "sd.beta", "u", "sd.over")
      ov <- "_OV"
      rateparam <- paste0("_", select.rate.param)
    }else{
      params <- c("beta", "mean.beta", "sd.beta")
      ov <- ""
      rateparam <- ""
    }
    
    dummies_interaction <- dummy_cols(dat[,by])[,-1]
    dummies_interaction * rep(dat$exp_deaths, ncol(dummies_interaction)) -> exp_deaths_interaction
    dummies_interaction * rep(dat$labo_deaths, ncol(dummies_interaction)) -> labo_deaths_interaction
    
    
    if(by == "age_group"){
      colnames(exp_deaths_interaction) <- c("exp_int_0_39", "exp_int_40_59", "exp_int_60_69", "exp_int_70_79", "exp_int_80")
      colnames(labo_deaths_interaction) <- c("labo_int_0_39", "labo_int_40_59", "labo_int_60_69", "labo_int_70_79", "labo_int_80")
    }
    
    if(by == "canton_name" | by == "phase"){
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
    
    if(overdispersion == TRUE){
      ov <- "_OV"
      rateparam <- paste0("_", select.rate.param)
      params <- c("beta", "u", "sd.over")
    }else{
      params <- c("beta")
      ov <- ""
      rateparam <- ""
    }
    
  }
  
  t_0 <- Sys.time()
  no_cores <- parallel::detectCores()
  cl <- makeCluster(no_cores) 
  
  if(overdispersion == TRUE){
    clusterExport(cl, c("dat_fin", "mod", "Tot", "params", "ran.sam.it", "select.rate.param","overdispersion"))
  }else{
    clusterExport(cl, c("dat_fin", "mod", "Tot", "params", "ran.sam.it"))
  }
  
  clusterEvalQ(cl, {
    library(nimble)
    library(dplyr)
    library(tidyr)
  })
  
  cat("Launch model on cluster")
  
  result <- parLapply(cl, 1:200, nimble_sam)  
  stopCluster(cl) 
  t_1 <- Sys.time()
  t_1 - t_0
  
  saveRDS(result, file = paste0(controls$savepoint,"SamplesBMAtrun_", by, "_", nam, cor.nam, ov, rateparam))
}

# by = NULL, takes ~20min
# by = age_group, takes ~1.5h
# by = canton, takes ~36h
# by = phase, takes ~30min


