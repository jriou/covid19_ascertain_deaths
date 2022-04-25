
# Created 19.04.2022

# Posteriors of OV

#############################################################


library(dplyr)
library(fastDummies)
library(tidyr)
library(ggplot2)
library(cowplot)


setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/savepoint/")


nam <- c("age_group", "canton_name", "phase", "Total")
ext <- c("corrected_OV", "corrected_OV_0.01", "corrected_OV_0.001")

list.res <- list()

for(i in 1:length(ext)){
  print(i)
  sampls <- lapply(paste0("SamplesBMAtrun_", nam, "_temperature_", ext[i]), readRDS)
  
  set.seed(11)
  # retrieve 1000 of the combined posteriors
  lapply(sampls, function(X){
    Y <- do.call(rbind, X)
    Y <- Y[sample(1:nrow(Y), size = 1000),]
    Y <- as_tibble(Y)
    return(Y)
  }) -> combined_samples
  
  names(combined_samples) <- nam
  
  list.res[[i]] <- combined_samples
  
}



# age
ggplot() + 
  geom_line(data = list.res[[1]]$age_group, aes(x = 1:1000, y = sd.over), col = "red", alpha = 0.5) + 
  geom_line(data = list.res[[2]]$age_group, aes(x = 1:1000, y = sd.over), col = "blue", alpha = 0.5) + 
  geom_line(data = list.res[[3]]$age_group, aes(x = 1:1000, y = sd.over), col = "black", alpha = 0.5)

# canton
ggplot() + 
  geom_line(data = list.res[[1]]$canton_name, aes(x = 1:1000, y = sd.over), col = "red", alpha = 0.5) + 
  geom_line(data = list.res[[2]]$canton_name, aes(x = 1:1000, y = sd.over), col = "blue", alpha = 0.5) + 
  geom_line(data = list.res[[3]]$canton_name, aes(x = 1:1000, y = sd.over), col = "black", alpha = 0.5)

# phase
ggplot() + 
  geom_line(data = list.res[[1]]$phase, aes(x = 1:1000, y = sd.over), col = "red", alpha = 0.5) + 
  geom_line(data = list.res[[2]]$phase, aes(x = 1:1000, y = sd.over), col = "blue", alpha = 0.5) + 
  geom_line(data = list.res[[3]]$phase, aes(x = 1:1000, y = sd.over), col = "black", alpha = 0.5)

# total
ggplot() + 
  geom_line(data = list.res[[1]]$Total, aes(x = 1:1000, y = sd.over), col = "red", alpha = 0.5) + 
  geom_line(data = list.res[[2]]$Total, aes(x = 1:1000, y = sd.over), col = "blue", alpha = 0.5) + 
  geom_line(data = list.res[[3]]$Total, aes(x = 1:1000, y = sd.over), col = "black", alpha = 0.5)

# the posterior of the sd are the same based on the different priors



#############################################################
#############################################################
#############################################################
#############################################################



