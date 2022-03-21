

# Created 25.02.2022

# Extract samples

#############################################################


library(dplyr)
library(fastDummies)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

nam <- c("age_group", "canton", "phase", "Total")
sampls <- lapply(paste0("savepoint/SamplesBMAtrun_", nam, "_temperature"), readRDS)

# retrieve 1000 of the combined posteriors
lapply(sampls, function(X){
  Y <- do.call(rbind, X)
  return(Y[sample(1:nrow(Y), size = 1000),])
}) -> combined_samples

names(combined_samples) <- nam


# need to go back and retrieve the names of the coefficients, thus i need the results
# of the INLA modelling.
samp = readRDS("savepoint/merged_samples.rds")



for(i in 1:length(nam)){
  
  by <- nam[i]
  
  if(by == "Total"){
    by <- NULL
  }
  
  samp$samples_temp %>% 
    group_by_at(vars("week", by, "it")) %>% 
    summarise(deaths=sum(deaths), 
              exp_deaths = sum(exp_deaths),
              labo_deaths = sum(labo_deaths)) -> dat
  
  if(is.null(by)){
    by <- "Total"
    dummies_interaction <- c("exp", "lab")
  }else{
    dummies_interaction <- dummy_cols(dat[,by])[,-1]
    dummies_interaction <- colnames(dummies_interaction)
  }
  colnames(combined_samples[[by]]) <- paste(colnames(combined_samples[[by]]), dummies_interaction, sep = ":")
  
}

saveRDS(combined_samples, file = "savepoint/combined_samples_trun_temperature")

# get the summary statistics
lapply(combined_samples, function(Y) apply(Y, 2, quantile, probs = c(0.5, 0.025, 0.975)))


#############################################################
#############################################################
#############################################################



