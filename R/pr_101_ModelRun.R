


# Created 12.12.2022


# INLA prediction model


#######################################################################################

remove(list=ls())
library(tidyverse)
library(INLA)
library(sf)
library(dplyr)
library(spdep)
INLA:::inla.dynload.workaround()

#inla.setOption(pardiso.license = "~/.")


# set as working directory the folder where all the R files are.
setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/") 

# select if you want the predictions to include
add.temperature <- FALSE


finaldb=readRDS("data/fin")
shp = read_sf("data/shp.shp")
W.nb <- poly2nb(shp)
nb2INLA("W.adj", W.nb) 

# Select data for the period 2015-2019 
data = finaldb

# Create indexes
data$id.space = as.numeric(as.factor(data$ID_space))
data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
data$id.tmp <- inla.group(data$mean.temp, n = 100, method = "cut", idx.only = TRUE)
data$id.year <- as.numeric(data$year) - 2014

# create groups for age
data$id.age <- as.numeric(as.factor(data$age.group))
data$id.sex <- as.numeric(as.factor(data$sex)) # females is the baseline, ie 1
data$id.sex <- data$id.sex - 1




if(add.temperature){
  
  formula = 
    deaths ~ 1 + offset(log(population)) + hol + sex + 
    f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) +
    f(id.year, model='iid', hyper=hyper.iid, constr = TRUE) + 
    f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
    f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE, scale.model = TRUE, cyclic = TRUE) +
    f(id.space, model='bym2', graph="W.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym)
  
  datCV_firslop <- data
  truth <- datCV_firslop$deaths[as.numeric(datCV_firslop$year) > 2019] 
  datCV_firslop$deaths[as.numeric(data$year) > 2019] <- NA
  datCV_firslop <- datCV_firslop[datCV_firslop$year < 2022,] 
  thet <- c(6.8695738, 6.7051410, -0.6536874, 6.4729316, 5.6696581, -0.9573297) # These are obtained with a quick run
  
}else{
  
  formula = 
    deaths ~ 1 + offset(log(population)) + hol + sex + 
    f(id.year, model='iid', hyper=hyper.iid, constr = TRUE) + 
    f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
    f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE, scale.model = TRUE, cyclic = TRUE) +
    f(id.space, model='bym2', graph="W.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym)
  
  datCV_firslop <- data
  truth <- datCV_firslop$deaths[as.numeric(datCV_firslop$year) > 2019] 
  datCV_firslop$deaths[as.numeric(data$year) > 2019] <- NA
  thet <- c(6.7051410, -0.6536874, 6.4729316, 5.6696581, -0.9573297)
  
}



# INLA SET UP
# priors
hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# Under Poisson uses default set up
control.family=inla.set.control.family.default()



in.mod = inla(formula,
              data=datCV_firslop,
              family="Poisson",  
              verbose = TRUE, 
              control.family=control.family,
              control.compute=list(config = TRUE), 
              control.mode=list(theta=thet, restart=T),
              num.threads = round(parallel::detectCores()*.1), 
              control.predictor = list(link = 1))




# now I need to sample from a Poisson


set.seed(11)
post.samples <- inla.posterior.sample(n = 1000, result = in.mod)
predlist <- do.call(cbind,
                    lapply(post.samples,
                           function(X) exp(X$latent[startsWith(rownames(X$latent), "Pred")])))

pois.samples <- apply(predlist, 2,
                      function(Z) rpois(n = length(Z), lambda = Z))
pois.samples <- as.data.frame(pois.samples)

pois.samples$EURO_LABEL <- datCV_firslop$EURO_LABEL
pois.samples$ID_space <- datCV_firslop$ID_space
pois.samples$NAME.POP <- datCV_firslop$NAME.POP
pois.samples$deaths <- data$deaths 
pois.samples$population <- datCV_firslop$population 
pois.samples$year <- datCV_firslop$year
pois.samples$age.group <- datCV_firslop$age.group 
pois.samples$sex <- datCV_firslop$sex
pois.samples$year <- datCV_firslop$year

if(add.temperature){
  saveRDS(pois.samples, file = "savepoint/pois.samples.withtemperature")
}else{
  saveRDS(pois.samples, file = "savepoint/pois.samples.2022")
}



#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################



