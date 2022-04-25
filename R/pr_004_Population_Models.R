#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Model for population Switzerland
#:::::::::::::::::::::::::::::


# download the file from https://www.pxweb.bfs.admin.ch/pxweb/en/
# and select:
# Topic: Population 
# Variable: age class, canton, sex and year
# Year: 2010:2020
# Then click on: Permanent and non-permanent resident population by canton, citizenship (selection), country of birth, sex and age class, 2010-2020
# Select all the years, all the cantons, male and femals and the age classes. 

# Store it as data/popCH2010_2020.csv
pop <- read.csv("data/popCH2010_2020.csv", sep = ";", header = TRUE, fileEncoding = "ISO-8859-3")

head(pop)
colnames(pop)[2] <- "canton"
pop$Population.type <- NULL


pop %>% rowwise() %>% mutate(X0_39 = sum(c_across(X0.Jahre:X39.Jahre), na.rm = T), 
                             X40_59 = sum(c_across(X40.Jahre:X59.Jahre), na.rm = T), 
                             X60_69 = sum(c_across(X60.Jahre:X69.Jahre), na.rm = T), 
                             X70_79 = sum(c_across(X70.Jahre:X79.Jahre), na.rm = T), 
                             X80plus = sum(c_across(X80.Jahre:X100.Jahre.und.mehr), na.rm = T)) %>% 
  dplyr::select(Jahr, canton, Geschlecht, X0_39, X40_59, X60_69, X70_79, X80plus) -> pop


pop$canton <- gsub("- ", "", pop$canton) 
pop$Geschlecht[pop$Geschlecht %in% "Mann"] <- "males"
pop$Geschlecht[pop$Geschlecht %in% "Frau"] <- "females"

head(pop)

# lets exclude 2020 since its the first year of covid and the pop of CH is calculated based on 31 Dec
pop %>% rename(year = Jahr, sex = Geschlecht) %>% filter(year<2020) -> pop
pop <- gather(pop, age, population, X0_39:X80plus, factor_key=TRUE)
head(pop)


expand.grid(year = c(2020, 2021, 2022), 
            canton = unique(pop$canton),
            sex = c("males", "females"), 
            age = c("X0_39", "X40_59", "X60_69", "X70_79", "X80plus")) -> pred

pred <- as.data.frame(pred)
pred$population <- NA

pop <- rbind(pop, pred)

saveRDS(pop, file = "data/pop2010_2020")


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

# select model
ints = c("nointeractions", "nointeractionsov", "interactionsov", "randomslope", "randomslopeov", "randomslopeintov")



for(i in 1:length(ints)){
  print(i)
  int <- as.character(ints[i])
  
  # define the formulas
  
  # INLA formula
  
  if(int == "nointeractions"){
    formula = 
      population ~ 1 +  
      year + factor(sex) +
      f(id.time, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.space, model='iid', hyper=hyper.iid, constr = TRUE) 
    
    thet <- c(11.48356218, -0.08424075, -0.31760865)
  }
  
  if(int == "nointeractionsov"){
    formula = 
      population ~ 1 +  
      year + factor(sex) +
      f(id.time, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.space, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.spacetimeagesex, model='iid', hyper=hyper.iid, constr = TRUE)
    
    thet <- c(11.48356218, -0.08424075, -0.31760865, 4.2859812)
  }
  
  if(int == "interactionsov"){
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
  }
  
  
  if(int == "randomslope"){
    formula = 
      population ~ 1 +  
      year + factor(sex) +
      f(id.time, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.space, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.spaceagesex, year, model='iid', hyper=hyper.iid, constr = TRUE)
    thet <- c(11.300259, -5.650470, -3.915808, 9.243239)
  }
  
  if(int == "randomslopeov"){
    formula = 
      population ~ 1 +  
      year + factor(sex) +
      f(id.time, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.space, model='iid', hyper=hyper.iid, constr = TRUE)  + 
      f(id.spaceagesex, year, model='iid', hyper=hyper.iid, constr = TRUE) +
      f(id.spacetimeagesex, model='iid', hyper=hyper.iid, constr = TRUE)
    thet <- c(13.1353630, -2.8515870, -0.2633263, 19.0374519, 6.5011976)
    
  }
  
  if(int == "randomslopeintov"){
    formula = 
      population ~ 1 +  
      year + factor(sex) +
      f(id.time, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.age, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.space, model='iid', hyper=hyper.iid, constr = TRUE)  + 
      f(id.timeage, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.spacetime, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.spaceage, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.spacetimeagesex, model='iid', hyper=hyper.iid, constr = TRUE) + 
      f(id.spaceagesex, year, model='iid', hyper=hyper.iid, constr = TRUE) 
    
    thet <- c(9.4634220, -0.8705926, -0.2501508, 6.8640970, 8.9706450, 7.5160566, 7.7418240, 19.1484860)
  }

  
  t_0 <- Sys.time()
  list2store <- list()
  years <- 2010:2019
  
  for(j in 1:length(years)){
    datCV <- datCV_reset <- data[complete.cases(data),]
    testIndexes <- which(datCV$year %in% years[j], arr.ind=TRUE)
    datCV$population[testIndexes] <- NA
    
    in.mod <- 
      inla(formula,
           data=datCV,
           family="Poisson",  
           verbose = F, 
           control.family=control.family,
           control.compute=list(config = TRUE), 
           control.mode=list(restart=TRUE, theta = thet),
           num.threads = round(parallel::detectCores()*.8), 
           control.predictor = list(link = 1)
      )
    
    in.mod <- inla.rerun(in.mod)
    
    post.samples <- inla.posterior.sample(n = 1000, result = in.mod)
    
    # store the results
    list.CV.results <- list(
      prediction = lapply(post.samples, function(X) exp(X$latent[testIndexes])),
      true_values =  datCV_reset$population[testIndexes]
    )
    
    list2store[[j]] <- list.CV.results
  } 
  
  t_1 <- Sys.time()
  print(t_1 - t_0)
  
  saveRDS(list2store, file = paste0("savepoint/popcv_", int))
  
  # and leave the past 3 years cross validation
  
  list2store <- list()
  years <- 2017:2019
  
  datCV <- datCV_reset <- data[complete.cases(data),]
  testIndexes <- which(datCV$year %in% years, arr.ind=TRUE)
  datCV$population[testIndexes] <- NA
  
  in.mod <- 
   inla(formula,
        data=datCV,
        family="Poisson",  
        verbose = F, 
        control.family=control.family,
        control.compute=list(config = TRUE), 
        control.mode=list(restart=TRUE, theta = thet),
        num.threads = round(parallel::detectCores()*.8), 
        control.predictor = list(link = 1)
   )
  
  in.mod <- inla.rerun(in.mod)
  
  post.samples <- inla.posterior.sample(n = 1000, result = in.mod)
  
  # store the results
  list.CV.results <- list(
   prediction = lapply(post.samples, function(X) exp(X$latent[testIndexes])),
   true_values =  datCV_reset$population[testIndexes]
  )
  
  saveRDS(list.CV.results, file = paste0("savepoint/popcv3y_", int))

}

# [1] 1
# Time difference of 6.907203 mins
# [1] 2
# Time difference of 6.788268 mins
# [1] 3
# Time difference of 1.563339 hours
# [1] 4
# Time difference of 10.80058 mins
# [1] 5
# Time difference of 10.68735 mins
# [1] 6
# Time difference of 1.339423 hours







