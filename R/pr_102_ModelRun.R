


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
add.temperature <- TRUE


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
  datCV_firslop <- datCV_firslop[datCV_firslop$year < 2022,] 
  data_true_deaths <- datCV_firslop
  datCV_firslop$deaths[as.numeric(data_true_deaths$year) > 2019] <- NA
  thet <- c(6.8695738, 6.7051410, -0.6536874, 6.4729316, 5.6696581, -0.9573297) # These are obtained with a quick run
  
}else{
  
  formula = 
    deaths ~ 1 + offset(log(population)) + hol + sex + 
    f(id.year, model='iid', hyper=hyper.iid, constr = TRUE) + 
    f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
    f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE, scale.model = TRUE, cyclic = TRUE) +
    f(id.space, model='bym2', graph="W.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym)
  
  datCV_firslop <- data
  data_true_deaths <- datCV_firslop
  datCV_firslop$deaths[as.numeric(data$year) > 2019] <- NA
  thet <- c(6.7051410, -0.6536874, 6.4729316, 5.6696581, -0.9573297)
  
}



# INLA SET UP
# priors
hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# Under Poisson uses default set up
control.family=inla.set.control.family.default()



# read population samples
pop.list <- readRDS("savepoint/popfinCH_list")



# here i have predicted 2022 just in case we update the analysis

funpar <- function(X){
  pop.tmp <- pop.list[[X]] %>% filter(year != 2022) 
  pop.tmp$year <- NULL

  datCV_firslop.tmp <- left_join(datCV_firslop, pop.tmp, 
                                 by = c("age.group" = "age", 
                                        "sex" = "sex", 
                                        "EURO_LABEL" = "EURO_LABEL", 
                                        "NAME.POP" = "NUTS318CD"))
  
  colnames(datCV_firslop.tmp)[colnames(datCV_firslop.tmp) %in% "population.y"] <- "population"
  
  in.mod = inla(formula,
                data=datCV_firslop.tmp,
                family="Poisson",  
                verbose = FALSE, 
                control.family=control.family,
                control.compute=list(config = TRUE), 
                control.mode=list(theta=thet, restart=T),
                num.threads = 10, 
                control.predictor = list(link = 1))

  
  # now I need to sample from a Poisson
  post.samples <- inla.posterior.sample(n = 1000, result = in.mod)
  predlist <- do.call(cbind,
                      lapply(post.samples,
                             function(X) exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  pois.samples <- apply(predlist, 2,
                        function(Z) rpois(n = length(Z), lambda = Z))
  pois.samples <- as.data.frame(pois.samples)
  
  pois.samples$EURO_LABEL <- datCV_firslop.tmp$EURO_LABEL
  pois.samples$ID_space <- datCV_firslop.tmp$ID_space
  pois.samples$NAME.POP <- datCV_firslop.tmp$NAME.POP
  pois.samples$deaths <- data_true_deaths$deaths 
  pois.samples$population <- datCV_firslop.tmp$population 
  pois.samples$year <- datCV_firslop.tmp$year
  pois.samples$age.group <- datCV_firslop.tmp$age.group 
  pois.samples$sex <- datCV_firslop.tmp$sex
  pois.samples$year <- datCV_firslop.tmp$year

  return(pois.samples)
  gc()
  
}

# t_0 <- Sys.time()
# list.loop <- list()
# for(k in 1:200){
#   print(k)
#   list.loop[[k]] <- funpar(k)
# }
# t_1 <- Sys.time()
# t_1 - t_0




t_0 <- Sys.time()

# Set up parallel environment
ncores <- 5
k <- 1:200
cl_inla <- makeCluster(ncores, methods=FALSE)

# extract packages on parallel environment 
clusterEvalQ(cl_inla, {
  library(INLA)
  library(dplyr)
  library(sf)
  library(spdep)
})

# extract R objects on parallel environment
clusterExport(cl_inla, c("datCV_firslop", "pop.list", "formula", "hyper.bym", "hyper.iid",
                         "funpar", "W.nb", "k", "thet", "data_true_deaths", "control.family"))

# run the the function in parallel
outpar <- parLapply(cl = cl_inla, k, funpar)

# close parallel environment
stopCluster(cl_inla)
t_1 <- Sys.time()
t_1 - t_0 # 9h

if(add.temperature){
  saveRDS(outpar, file = "savepoint/pois.samples.withtemperature.uppop")
}else{
  saveRDS(outpar, file = "savepoint/pois.samples.2022.uppop")
}


# I need to break the dataset into smaller dateset to be able to sample, as there are
# memory issues


outpar <- readRDS("savepoint/pois.samples.withtemperature.uppop") # 15min

# keep the relevant rows
cols <- outpar[[1]][,c("EURO_LABEL", "ID_space", "NAME.POP", "deaths", "year", "age.group", "sex")]

do.call(cbind, lapply(outpar, function(X) X[,"population"])) -> outpar.pop
do.call(cbind, lapply(outpar, function(X) X[,1:1000])) -> outpar.excess
colnames(outpar.excess) <- paste0(paste0("V", 1:1000), "_", rep(1:200, each = 1000))

set.seed(11)
sample(1:ncol(outpar.excess), 1000) -> sams
finsamples <- outpar.excess[,sams]
finsamples <- data.frame(finsamples)


finpop <- outpar.pop[, as.numeric(gsub(".*_", "", colnames(finsamples)))]
colnames(finpop) <- paste0("pop_", gsub(".*_", "", colnames(finsamples)))

finsamples <- cbind(cols, finsamples, finpop)
saveRDS(finsamples, file = "savepoint/pois.samples.temp.bma")



#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


