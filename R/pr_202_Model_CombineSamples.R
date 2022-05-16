#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Extract samples
#:::::::::::::::::::::::::::::


nam <- c("age_group", "canton_name", "phase", "Total")
ext <- "_corrected" # _corrected", "_corrected_OV", "_corrected_OV_0.01", "_corrected_OV_0.001"
sampls <- lapply(paste0(controls$savepoint,"SamplesBMAtrun_", nam, "_temperature", ext), readRDS)

set.seed(11)
# retrieve 1000 of the combined posteriors and remove the u-s
lapply(sampls, function(X){
  Y <- do.call(rbind, X)
  Y <- Y[sample(1:nrow(Y), size = 1000),]
  Y <- as_tibble(Y)
  Y %>% select(starts_with("beta") | starts_with("mean") | starts_with("sd")) -> Y
  return(Y)
}) -> combined_samples

names(combined_samples) <- nam


# need to go back and retrieve the names of the coefficients, thus i need the results
# of the INLA modelling.
merg = readRDS(file.path(controls$savepoint,"merged_samples.rds"))


for(i in 1:length(nam)){
  print(i)
  by <- nam[i]
  
  if(by == "Total"){
    by <- NULL
  }
  
  dat <- samp
  
  set.seed(11)
  ran.sam.it <- sample(1:1000, size = 200)
  dat$it <- as.numeric(as.factor(dat$it))
  dat %>% filter(it %in% ran.sam.it, !(phase %in% 7)) -> dat
  
  dat %>% 
    group_by_at(vars("week", by, "it")) %>% 
    summarise(deaths=sum(deaths), 
              exp_deaths = sum(corr_exp_deaths),
              labo_deaths = sum(labo_deaths)) -> dat
  
  if(is.null(by)){
    by <- "Total"
    dummies_interaction <- c("exp", "lab")
    colnames(combined_samples[[by]]) <- 
      paste(colnames(combined_samples[[by]]), dummies_interaction, sep = ":")
  }else{
    dummies_interaction <- dummy_cols(dat[,by])[,-1]
    dummies_interaction <- colnames(dummies_interaction)
    
    colnames(combined_samples[[by]]) <- 
      c(paste(colnames(combined_samples[[by]])[1:c(2*length(dummies_interaction))], dummies_interaction, sep = ":"), 
        "mean.beta[1]", "mean.beta[2]", "sd.beta[1]", "sd.beta[2]")
  }
}

saveRDS(combined_samples, file = paste0(controls$savepoint,"combined_samples_trun_temperature", ext))

# get the summary statistics
lapply(combined_samples, function(Y) apply(Y, 2, quantile, probs = c(0.5, 0.025, 0.975)))





