#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Fit the best performing population model
#:::::::::::::::::::::::::::::


pop <- readRDS(file.path(controls$savepoint,"pop2010_2020.rds"))

# Now fit the model


data <- pop

# Create indexes
data$id.space = as.numeric(as.factor(data$canton))
data$id.time <- data$year - 2009
data$id.age <- as.numeric(as.factor(data$age))
data$id.spaceage <- as.numeric(as.factor(paste0(data$age, data$canton)))
data$id.timeage <- as.numeric(as.factor(paste0(data$age, data$year)))
data$id.spacetime <- as.numeric(as.factor(paste0(data$year, data$canton)))
data$id.spaceagesex <- as.numeric(as.factor(paste0(data$sex, data$canton, data$age)))
data$id.spacetimeagesex <- 1:nrow(data)


# very vague prior
hyper.iid <- list(theta = list(prior="pc.prec", param=c(10, 0.1)))

# Under Poisson uses default set up
control.family=inla.set.control.family.default()


# INLA SET UP
# priors


##
## base+interactions+overdispersion
formula = 
  population ~ 1 +  
  year + factor(sex) +
  f(id.time, model='iid', hyper=hyper.iid, constr = TRUE) + 
  f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
  f(id.space, model='iid', hyper=hyper.iid, constr = TRUE) +
  f(id.timeage, model='iid', hyper=hyper.iid, constr = TRUE) + 
  f(id.spacetime, model='iid', hyper=hyper.iid, constr = TRUE) + 
  f(id.spaceage, model='iid', hyper=hyper.iid, constr = TRUE) +
  f(id.spacetimeagesex, model='iid', hyper=hyper.iid, constr = TRUE)

thet <- c(9.9185942, -0.1716981, -0.34466803, 7.1791676, 11.3838301, 5.0533784, 4.2859812)

m = inla(formula,
         data=data,
         family="Poisson",
         control.family=control.family,
         verbose = TRUE,
         num.threads = round(parallel::detectCores()*.8),
         control.compute=list(config = TRUE),
         control.mode=list(restart=TRUE, theta = thet),
         control.predictor=list(link = 1))

summary(m)
m <- inla.rerun(m)

post.samples <- inla.posterior.sample(n = 200, result = m)

index <- startsWith(names(post.samples[[1]]$latent[,1]), "Predictor")
poppred <- lapply(post.samples, function(X) exp(X$latent[index]))

pred.samples <- do.call(cbind, poppred)
set.seed(11)
pois.samples <-  apply(pred.samples, 2, function(X) rpois(n = length(X), lambda = X))


datsamples <- data[, c("year", "canton", "sex", "age", "population")]
datsamples <- cbind(datsamples, pois.samples)


datsamples <- datsamples[is.na(datsamples$population),]
datsamples$population <- NULL


saveRDS(datsamples, file = file.path(controls$savepoint,"pois.samples.population.OV.INT.rds"))
