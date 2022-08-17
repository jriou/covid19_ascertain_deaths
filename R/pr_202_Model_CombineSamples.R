#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Extract samples
#:::::::::::::::::::::::::::::

nam <- c("age_group", "canton", "phase", "Total")
ext <-  "_corrected_OV_0.001_finmodel"

sampls <- lapply(paste0("SamplesBMAtrun_", nam, "_temperature", ext), 
                 function(X) readRDS(file.path(controls$savepoint, X)))


set.seed(11)
# retrieve 1000 of the combined posteriors and remove the u-s
lapply(sampls, function(X){
  Y <- do.call(rbind, X)
  Y <- Y[sample(1:nrow(Y), size = 1000),]
  Y <- as_tibble(Y)
  Y %>% dplyr::select(starts_with("beta") | starts_with("mean") | starts_with("sd")) -> Y
  return(Y)
}) -> combined_samples

names(combined_samples) <- nam


# need to go back and retrieve the names of the coefficients, thus i need the results
# of the INLA modelling.
merg = readRDS(file.path(controls$savepoint, "merged_samples_finmodel.rds"))


for(i in 1:length(nam)){
  print(i)
  by <- nam[i]
  
  if(by == "Total"){
    by <- NULL
  }
  
  dat <- merg
  
  set.seed(11)
  ran.sam.it <- sample(1:1000, size = 200)
  dat$it <- as.numeric(as.factor(dat$it))
  dat %>% filter(it %in% ran.sam.it) -> dat
  
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


saveRDS(combined_samples, file.path(controls$savepoint, paste0("combined_samples_trun_temperature", ext)))





