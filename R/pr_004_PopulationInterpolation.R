

# Created 08.04.2021


# Population per week


######################################################################################


library(readxl)
library(dplyr)
library(tidyr)
library(zoo)

popCH <- read.csv("E:/Postdoc Imperial/Projects/COVID19 Greece/data/population/popCH_2014_2019.csv", sep = ";")
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
EUROSTAT <- read_excel("E:/Postdoc Imperial/Projects/COVID19 Greece/data/deaths/EUROSTAT_ISO_HMEROLOGIO.xls")
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


pop$year <- as.character(pop$year)
pop_weekly$year <- as.character(pop_weekly$year)

pop$year <- as.numeric(pop$year)
pop %>% group_by(NUTS318CD, sex, ageg) %>% 
  summarise(population = as.vector(coef(lm(population ~ year)) %*% c(1, 2021))) %>% 
  mutate(year = 2021) -> pop2021

pop %>% group_by(NUTS318CD, sex, ageg) %>% 
  summarise(population = as.vector(coef(lm(population ~ year)) %*% c(1, 2022))) %>% 
  mutate(year = 2022) -> pop2022

pop %>% group_by(NUTS318CD, sex, ageg) %>% 
  summarise(population = as.vector(coef(lm(population ~ year)) %*% c(1, 2023))) %>% 
  mutate(year = 2023) -> pop2023


pop2021 <- pop2021[,colnames(pop)]
pop2022 <- pop2022[,colnames(pop)]
pop2023 <- pop2023[,colnames(pop)]

pop <- rbind(pop, pop2021, pop2022, pop2023)


pop$year <- as.character(pop$year)

class(pop_weekly$age)
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

# a couple of checks

ageg = "60-69"
sexg = "male"
regiong = "Basel-Landschaft"

library(ggplot2)
pop_weekly$days2plot <- as.numeric(as.factor(pop_weekly$EURO_LABEL))

pop_weekly %>% filter(age == ageg, sex == sexg, region == regiong) %>% 
  ggplot() + geom_point(aes(x = days2plot, y = popfin))
dev.off()


table(pop_weekly$age)


pop_weekly$day2pred <- pop_weekly$refdate <- pop_weekly$population <- pop_weekly$pop.next.year <- pop_weekly$refdate2 <-
  pop_weekly$lambda <- pop_weekly$beta0 <-  pop_weekly$days2plot <- NULL

colnames(pop_weekly)[3] <- "NUTS318CD"
colnames(pop_weekly)[6] <- "population"

saveRDS(pop_weekly, file = "C:/Users/gkonstan/Desktop/ch excess 2/popfinCH")
pop_weekly <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/population/popfinCH")



ageg = "60-69"
sexg = "male"
regiong = "ZH"

library(ggplot2)
pop_weekly$days2plot <- as.numeric(as.factor(pop_weekly$EURO_LABEL))

pop_weekly %>% filter(age == ageg, sex == sexg, NUTS318CD == regiong) %>% 
  filter(EURO_LABEL %in% c(paste0(2015:2020, "-W01"))) %>% 
  ggplot() + geom_point(aes(x = days2plot, y = population), size = 2) + 
  xlab("Years") + ylab("Population") +
  theme_light() + scale_x_continuous(breaks = c(1, 54, 106, 158, 210, 262), labels = 2015:2020) -> p1


pop_weekly %>% filter(age == ageg, sex == sexg, NUTS318CD == regiong) %>% 
  filter(EURO_LABEL %in% c(paste0(2015:2020, "-W01"), "2020-W53")) %>% 
  ggplot() + geom_point(aes(x = days2plot, y = population), size = 2) + 
  xlab("Years") + ylab("Population") +
  theme_light() + scale_x_continuous(breaks = c(1, 54, 106, 158, 210, 262, 314), labels = 2015:2021) + 
  geom_point(aes(x = 314, y = 74529.14), size = 2, col = "red") -> p2



pop_weekly %>% filter(age == ageg, sex == sexg, NUTS318CD == regiong) %>% 
  filter((year %in% 2015)|(EURO_LABEL %in% "2016-W01")) %>% 
  ggplot() + geom_point(aes(x = days2plot, y = population), size = 1) + 
  xlab("Years") + ylab("Population") +
  theme_light() + scale_x_continuous(breaks = c(1, 54), labels = 2015:2016) + 
  geom_point(aes(x = c(1), y = c(70249.99)), size = 2, col = "red") + 
  geom_point(aes(x = c(54), y = c(70739.62)), size = 2, col = "red") -> p3


pop_weekly %>% filter(age == ageg, sex == sexg, NUTS318CD == regiong) %>% 
  filter(EURO_LABEL %in% c(paste0(2015:2020, "-W01"), "2020-W53")) -> tmp
         
pop_weekly %>% filter(age == ageg, sex == sexg, NUTS318CD == regiong) %>% 
  ggplot() + geom_point(aes(x = days2plot, y = population), size = 0.5) + 
  xlab("Years") + ylab("Population") +
  theme_light() + scale_x_continuous(breaks = tmp$days2plot, labels = tmp$year) + 
  geom_point(data = tmp, aes(x = days2plot, y = population), size = 2, col = "red") + 
  geom_point(aes(x = c(54), y = c(70739.62)), size = 2, col = "red") -> p4
dev.off()


library(patchwork)


png("linearInterpolation.png", width = 16, height = 12, units = "cm", res = 300)
(p1|p2)/(p3|p4)
dev.off()



######################################################################################
######################################################################################
######################################################################################
######################################################################################

library(sf)
shp <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/shp/YearsCancerRegistry_canton")
shp <- st_as_sf(shp)
plot(shp$geometry)


linkCH <- data.frame(
  NAME = unique(shp$NAME), 
  NUTS3 = c("CH056", "CH021", "CH012", "CH011", "CH070", 
            "CH055", "CH040", "CH022", "CH061", "CH033", 
            "CH062", "CH057", "CH063", "CH025", "CH024", 
            "CH023", "CH051", "CH032", "CH064", "CH065", 
            "CH013", "CH052", "CH053", "CH066", "CH054", 
            "CH031"), 
  CN = c("GR", "BE", "VS", "VD", "TI", "SG", "ZH", "FR", "LU", "AG", "UR", "TG", "SZ", "JU", "NE", "SO", "GL", "BL", "OW", 
         "NW", "GE", "SH", "AR", "ZG", "AI", "BS")
)



finaldb <- readRDS("C:/Users/gkonstan/Desktop/michela code/data/Switzerland/finaldb")
head(finaldb)
head(pop_weekly)


pop_weekly <- left_join(pop_weekly, linkCH, by = c("NUTS318CD" = "CN"))

finaldb <- left_join(finaldb, pop_weekly, by = c("ID_space" = "NUTS3", "age.group" = "age", "sex" = "sex", "EURO_LABEL" = "EURO_LABEL"))
plot(finaldb$population.x, finaldb$population.y)


finaldb$year.y <- finaldb$population.x <- NULL
colnames(finaldb)[5] <- "year"
colnames(finaldb)[10] <- "population"
head(finaldb)

finaldb$NAME <- finaldb$NUTS318CD <- NULL

saveRDS(finaldb, file = "C:/Users/gkonstan/Desktop/michela code/data/Switzerland//finaldb")

