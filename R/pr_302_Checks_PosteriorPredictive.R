#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Posterior predictive checks
#:::::::::::::::::::::::::::::



setwd("savepoint_2022-04-19/")
res <- lapply(c("combined_samples_trun_temperature_corrected", "combined_samples_trun_temperature_corrected_OV", "combined_samples_trun_temperature_corrected_OV_0.01", "combined_samples_trun_temperature_corrected_OV_0.001"), readRDS)


# Prepare data for counts
samp = readRDS("merged_samples.rds")

# select to use the predictions including the temperature
temperature <- TRUE
# select whether to exclude phase 7 or not (TRUE includes it)
phase7 <- FALSE
# correct for population changes after a wave
correction_expected <- TRUE
overdispersion <- TRUE # need to run the code once for FALSE and once for TRUE


by.loop <- list(NULL, "age_group", "canton_name", "phase")
dat.loop <- list()

for(i in 1:length(by.loop)){
  by <- by.loop[[i]]
  if(temperature == TRUE){
    dat <- samp
    nam <- "temperature"
  }else{
    # this is in case we provide it with a list with the results without using the temperature
    dat <- samp$samples_base
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
  }else{
    dat %>% 
      group_by_at(vars("week", by, "it")) %>% 
      summarise(deaths=sum(deaths), 
                exp_deaths = sum(exp_deaths),
                labo_deaths = sum(labo_deaths)) -> dat
  }
  
  if(is.null(by) == FALSE){
    
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
  }
  
  
  dat_fin %>% filter(it %in% 6) -> dat_fin
  dat.loop[[i]] <- dat_fin
  
}

length(dat.loop) # 4 things: null, age, canton, phase
length(res) # 4 things, the four models


##
## Corrected

if(overdispersion){
  
  # Overdispersion with rate 0.1
  nam <- c("age_group", "canton_name", "phase", "Total")
  ext <- "corrected_OV" 
  sampls <- lapply(paste0("SamplesBMAtrun_", nam, "_temperature_", ext), readRDS)
  
  set.seed(11)
  # retrieve 1000 of the combined posteriors and remove the u-s
  lapply(sampls, function(X){
    Y <- do.call(rbind, X)
    Y <- Y[sample(1:nrow(Y), size = 1000),]
    Y <- as_tibble(Y)
    return(Y)
  }) -> combined_samples
  
  names(combined_samples) <- nam
  
  X <- list(combined_samples$Total, combined_samples$age_group, combined_samples$canton_name, combined_samples$phase)
  
  
}else{
  X <- list(res[[1]]$Total, res[[1]]$age_group, res[[1]]$canton_name, res[[1]]$phase)
}


lapply(X, function(K){
  K %>% select(starts_with("beta") | starts_with("u")) -> dat.K
  return(dat.K)
}) -> dat.K

lapply(dat.loop, function(Y){
  Y %>% select(starts_with("exp") | starts_with("labo")) -> mod.mat
  return(mod.mat)
}) -> mod.mat

mod.mat[[1]]$week <- NULL
mod.mat[[2]]$week <- mod.mat[[2]]$age_group <- mod.mat[[2]]$exp_deaths <- mod.mat[[2]]$labo_deaths <- NULL
mod.mat[[3]]$week <- mod.mat[[3]]$canton_name <- mod.mat[[3]]$exp_deaths <- mod.mat[[3]]$labo_deaths <- NULL
mod.mat[[4]]$week <- mod.mat[[4]]$phase <- mod.mat[[4]]$exp_deaths <- mod.mat[[4]]$labo_deaths <- NULL


store.list <- list()

for(j in 1:4){
  print(j)
  
  fitted.vals <- list()
  
  if(overdispersion){
    for(i in 1:length(dat.K)){
      fitted.vals[[i]] <- as.matrix(dat.K[[j]][i, startsWith(colnames(dat.K[[j]]), "beta")]) %*% 
        t(as.matrix(mod.mat[[j]]))
      
      fitted.vals[[i]] <- fitted.vals[[i]] + dat.K[[j]][i, startsWith(colnames(dat.K[[j]]), "u[")]
    }
  }else{
    for(i in 1:length(dat.K)){
      fitted.vals[[i]] <- as.matrix(dat.K[[j]][i, ]) %*% t(as.matrix(mod.mat[[j]]))
    }
  }
  
  fitted.vals <- t(do.call(rbind, fitted.vals))
  
  # check correlation with median fitted
  cor.dat <- data.frame(median.fitted = apply(fitted.vals, 1, median), truth = dat.loop[[j]]$deaths) 
  
  # check the coverage
  fitted.quant <- as.data.frame(t(apply(fitted.vals, 1, quantile, probs = c(0.025, 0.975))))
  cov.prob = mean((fitted.quant$`2.5%` <= dat.loop[[j]]$deaths) & (fitted.quant$`97.5%` > dat.loop[[j]]$deaths))
  
  store.list[[j]] <- list(
    cor = quantile(apply(fitted.vals, 2, function(Z) cor(Z, dat.loop[[j]]$deaths)), 
                   probs = c(0.5, 0.025, 0.975)), 
    cor.dat = cor.dat, 
    cov.prob = cov.prob
  )
}



if(overdispersion){
  store.list.ov <- store.list
}else{
  store.list.no_ov <- store.list
}



##
## and compare
sapply(store.list.no_ov, function(X)X$cov.prob)
sapply(store.list.ov, function(X)X$cov.prob)

sapply(store.list.no_ov, function(X)X$cor)^2
sapply(store.list.ov, function(X)X$cor)^2



#############################################################
#############################################################
#############################################################
#############################################################


