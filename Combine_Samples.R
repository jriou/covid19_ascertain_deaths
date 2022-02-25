

# Created 25.02.2022

# Extract samples

#############################################################


library(dplyr)
library(fastDummies)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

nam <- c("age_group", "canton", "phase", "Total")
sampls <- lapply(paste0("savepoint/SamplesBMA_", nam), readRDS)

# retrieve 1000 of the combined posteriors
lapply(sampls, function(X){
  Y <- do.call(rbind, X)
  return(Y[sample(1:nrow(Y), size = 1000),])
}) -> combined_samples

names(combined_samples) <- nam




# need to go back and retrieve the names of the coefficients
samp = readRDS("savepoint/merged_samples.rds")

# Reduce samples during development ----
if(TRUE) {
  samp$samples_base = dplyr::filter(samp$samples_base,it<=100)
  samp$samples_temp = dplyr::filter(samp$samples_temp,it<=100)
}


for(i in 1:length(nam)){
  
  by <- nam[i]
  
  if(by == "Total"){
    by <- NULL
  }
  
  samp$samples_base %>% 
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

saveRDS(combined_samples, file = "savepoint/combined_samples")

# get the summary statistics
lapply(combined_samples, function(Y) apply(Y, 2, quantile, probs = c(0.5, 0.025, 0.975)))


#############################################################
#############################################################
#############################################################



