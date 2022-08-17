#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Population per week
#:::::::::::::::::::::::::::::

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

pop <- readRDS(file.path("data","pop2010_2020"))
pop %>% filter(year < 2020) -> pop
pop$year <- pop$year + 1 # this is because the swiss files give the population for the 31 Dec of a year


EUROSTAT_ISO <- readRDS("data/EUROSTAT_ISO_2")


# the population file should have the following format:
# NUTS318CD   ageg  sex year population
# 1         1 less40 male 2015      64769
# 2         1 less40 male 2016      62578
# 3         1 less40 male 2017      68788
# 4         1 less40 male 2018      62038
# 5         1 less40 male 2019      67761
# 6         1 less40 male 2020      60105


pop$age <- 
  recode(pop$age, X0_39 = "less40", X40_59 = "40-59", X60_69 = "60-69", X70_79 = "70-79", X80plus = "80plus")
pop$sex <- 
  recode(pop$sex, females = "female", males = "male")
colnames(pop)[c(1:4)] <- c("year", "NUTS318CD", "sex", "ageg")

expand.grid(age = c("less40", "40-59", "60-69", "70-79", "80plus"), 
            sex = c("male", "female"), 
            region = unique(pop$NUTS318CD), 
            EURO_LABEL = unique(EUROSTAT_ISO$EURO_LABEL)) -> pop_weekly



pop_weekly$EURO_LABEL <- as.character(pop_weekly$EURO_LABEL)


EUROSTAT_ISO$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT_ISO$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT_ISO$year <- as.numeric(format(EUROSTAT_ISO$EURO_TIME, "%Y"))

EUROSTAT_ISO %>% filter(year <= 2023) %>% group_by(EURO_LABEL) %>% filter(row_number()==4) -> EUROSTAT_ISO
EUROSTAT_ISO %>% group_by(year) %>% mutate(refdate = as.Date(paste0(year, "-01-01"))) -> EUROSTAT_ISO
EUROSTAT_ISO$day2pred <- as.numeric(EUROSTAT_ISO$EURO_TIME - as.Date("2015-01-01") + 1)



pop_weekly <- left_join(pop_weekly, EUROSTAT_ISO[,c("EURO_LABEL", "year", "day2pred", "refdate")], 
                        by = c("EURO_LABEL" = "EURO_LABEL"))



# Add the predictions

pred2021_23 <- readRDS(file.path("savepoint","pois.samples.population.OV.INT"))


# since with CH is the 31st of Dec of each year, I will make this 1st Jan of the next year
pred2021_23$year <- pred2021_23$year + 1

# clean the pred2021_23 to be compatible with the pop

pred2021_23$age <- 
recode(pred2021_23$age, X0_39 = "less40", X40_59 = "40-59", X60_69 = "60-69", X70_79 = "70-79", X80plus = "80plus")

pred2021_23$sex <- 
  recode(pred2021_23$sex, females = "female", males = "male")

sum(!(names(table(pred2021_23$canton)) %in% names(table(pop$NUTS318CD)))) # the canton names are identical

colnames(pred2021_23)[c(1:4)] <- c("year", "NUTS318CD", "sex", "ageg")



# create a list of pop data frames and loop over it


pop_store <- pop
pop_weekly_store <- pop_weekly
listpop <- list()

t_0 <- Sys.time()

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
  table(pop$year)
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

t_1 <- Sys.time()
t_1 - t_0 # 8 minutes

saveRDS(listpop, file = file.path("data","popfinCH_list_10_22"))


