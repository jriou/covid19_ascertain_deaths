

# Created 08.04.2022


# Population per week


######################################################################################


library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(patchwork)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

popCH <- read.csv("data/popCH_2014_2019.csv", sep = ";")
# popCH <- popCH[-1,-c(1, 4, 5, 7)] 

# colnames(popCH) <- unlist(c("year", "canton", "perpop", "sex", as.vector(popCH[1,5:length(popCH[1,])])))
# popCH <- popCH[-1,]
# popCH$year <- na.locf(popCH$year)
# popCH$canton <- na.locf(popCH$canton)
# popCH$perpop <- na.locf(popCH$perpop)

# focus on the perminent population


# popCH %>% filter(!is.na(sex)) -> popCH
# focus on the Permanent resident population
popCH %>% filter(Population.type %in% "Permanent resident population") -> popCH


6*2*26 # correct
# Create the age classes

# 40<, 41-59, 60-69, 70-79, 80-89, >80

popCH <- as.data.frame(popCH)
for(i in 5:25){
  popCH[,i] <- as.numeric(popCH[,i])
}

popCH$less40 <- rowSums(as.matrix(popCH[,colnames(popCH)[5:44]]))
popCH$`40_59` <- rowSums(as.matrix(popCH[,colnames(popCH)[45:64]]))
popCH$`60_69` <- rowSums(as.matrix(popCH[,colnames(popCH)[65:74]]))
popCH$`70_79` <- rowSums(as.matrix(popCH[,colnames(popCH)[75:84]]))
popCH$`80plus` <- rowSums(as.matrix(popCH[,colnames(popCH)[85:105]]))

colnames(popCH)
popCH %>% select(Year, Canton, Population.type, Sex, less40, `40_59`, `60_69`, `70_79`, `80plus`) -> popCH

# aggregate by perm pop

popCH %>% group_by(Year, Canton, Sex) %>% summarise_at(vars(less40:`80plus`), sum) -> popCH

data_long <- gather(popCH, age, population, less40:`80plus`, factor_key=TRUE)
data_long






pop <- data_long
colnames(pop)[4] <- "ageg"
colnames(pop)[2] <- "NUTS318CD"

pop$ageg <- as.character(pop$ageg)

pop$ageg[pop$ageg %in% "40_59"] <- "40-59"
pop$ageg[pop$ageg %in% "60_69"] <- "60-69"
pop$ageg[pop$ageg %in% "70_79"] <- "70-79"


colnames(pop)[c(1, 3)] <- c("year", "sex")

pop$sex <- as.character(pop$sex)
pop$sex[pop$sex %in% "Female"] <- "female"
pop$sex[pop$sex %in% "Male"] <- "male"

pop$year <- pop$year + 1

# the ISO weeks file
EUROSTAT <- read_excel("data/EUROSTAT_ISO_HMEROLOGIO.xls")
EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))
EUROSTAT %>% filter(year <= 2023 & year>= 2015) -> EUROSTAT


# the population file should have the following format:
# NUTS318CD   ageg  sex year population
# 1         1 less40 male 2015      64769
# 2         1 less40 male 2016      62578
# 3         1 less40 male 2017      68788
# 4         1 less40 male 2018      62038
# 5         1 less40 male 2019      67761
# 6         1 less40 male 2020      60105


# NUTS318CD is the NUTS3 code
# ageg the following age groups c("less40", "40-59", "60-69", "70-79", "80plus")
# sex the following sexes c("male", "female")
# year 2015:2020
# and population the population counts


expand.grid(age = c("less40", "40-59", "60-69", "70-79", "80plus"), 
            sex = c("male", "female"), 
            region = unique(pop$NUTS318CD), 
            EURO_LABEL = unique(EUROSTAT$EURO_LABEL)) -> pop_weekly



pop_weekly$EURO_LABEL <- as.character(pop_weekly$EURO_LABEL)


EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))

EUROSTAT %>% filter(year <= 2023) %>% group_by(EURO_LABEL) %>% filter(row_number()==4) -> EUROSTAT
EUROSTAT %>% group_by(year) %>% mutate(refdate = as.Date(paste0(year, "-01-01"))) -> EUROSTAT
EUROSTAT$day2pred <- as.numeric(EUROSTAT$EURO_TIME - as.Date("2015-01-01") + 1)



pop_weekly <- left_join(pop_weekly, EUROSTAT[,c("EURO_LABEL", "year", "day2pred", "refdate")], 
                        by = c("EURO_LABEL" = "EURO_LABEL"))



# and now merge with the population
max(pop_weekly$year)

# and now merge with the population



# Add the predictions

pred2021_23 <- readRDS("savepoint/pois.samples.population.OV.INT")


# since with CH is the 31st of Dec of each year, I will make this 1st Jan of the next year
pred2021_23$year <- pred2021_23$year + 1

# clean the pred2021_23 to be compatible with the pop

pred2021_23$age <- 
recode(pred2021_23$age, X0_39 = "less40", X40_59 = "40-59", X60_69 = "60-69", X70_79 = "70-79", X80plus = "80plus")

pred2021_23$sex <- 
  recode(pred2021_23$sex, females = "female", males = "male")

sum(!(names(table(pred2021_23$canton)) %in% names(table(pop$NUTS318CD)))) # the canton names are identical

colnames(pred2021_23)[c(1:4)] <- c("year", "NUTS318CD", "sex", "ageg")





# I will create a list of pop data frames and loop over it


pop_store <- pop
pop_weekly_store <- pop_weekly
listpop <- list()



for(i in 1:200){
  
  if(i %% 10 == 0){
    print(i)
  }
  
  pop <- pop_store
  pop_weekly <- pop_weekly_store
  
  pop$year <- as.character(pop$year)
  pop_weekly$year <- as.character(pop_weekly$year)
  
  pop_loop <- pred2021_23[,c("year", "NUTS318CD", "sex", "ageg")]
  pop_loop$year <- as.character(pop_loop$year)
  pop_loop$population <- pred2021_23[,4+i]
  pop <- rbind(pop, pop_loop)
  
  pop_weekly$age <- as.character(pop_weekly$age)
  pop$ageg <- as.character(pop$ageg)
  
  pop_weekly$year <- as.character(pop_weekly$year)
  pop_weekly <- left_join(pop_weekly, pop, by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "NUTS318CD"))
  
  
  # need to add the population of next year
  pop$year <- as.numeric(pop$year)
  pop$year <- pop$year - 1
  colnames(pop)[5] <- "pop.next.year"
  pop_weekly$year <- as.numeric(pop_weekly$year)
  
  pop_weekly <- left_join(pop_weekly, pop, by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "NUTS318CD"))
  
  # get the next years ref date
  pop_weekly$refdate2 <- as.Date(paste0(pop_weekly$year + 1, "-01-01"))
  pop_weekly$X <- NULL
  
  pop_weekly %>% mutate(lambda = (pop.next.year - population)/as.numeric((refdate2 - refdate))) %>% 
    mutate(beta0 = population - lambda*as.numeric(refdate - as.Date("2015-01-01") + 1)) %>% 
    mutate(popfin = beta0 + lambda*day2pred) -> pop_weekly
  
  pop_weekly <- pop_weekly[complete.cases(pop_weekly$popfin),]
  
  
  pop_weekly$day2pred <- pop_weekly$refdate <- pop_weekly$population <- pop_weekly$pop.next.year <- pop_weekly$refdate2 <-
    pop_weekly$lambda <- pop_weekly$beta0 <-  pop_weekly$days2plot <- NULL
  
  colnames(pop_weekly)[3] <- "NUTS318CD"
  colnames(pop_weekly)[6] <- "population"
  
  listpop[[i]] <- pop_weekly
}



# a couple of checks

ageg = "less40"
sexg = "male"
regiong = "Glarus"


pop_weekly <- listpop[[2]]
pop_weekly$days2plot <- as.numeric(as.factor(pop_weekly$EURO_LABEL))

pop_weekly$days2plot <- as.numeric(as.factor(pop_weekly$EURO_LABEL))
pop_weekly %>% filter(age == ageg, sex == sexg, NUTS318CD == regiong) %>% 
     ggplot() + geom_point(aes(x = days2plot, y = population))

saveRDS(listpop, file = "savepoint/popfinCH_list")




######################################################################################
######################################################################################
######################################################################################
######################################################################################
