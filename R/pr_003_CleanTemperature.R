


# Created 28.04.2021

# Clean temperature data CH


################################################################################

library(ncdf4)
library(plyr)
library(tidyr)
library(pbapply)
library(sf)
library(tidyverse)
library(maptools)
library(lctools)
library(raster)
library(lubridate)
library(spdep)
library(FNN)
library(patchwork)
library(dplyr)
library(readxl)
library(XML)
library(httr)

setwd("C:/Users/gkonstan/Desktop/ch excess 2/temperature/")

temperature <- nc_open("TabsD_ch01r.swiss.lv95_201901010000_201912310000.nc")
tmp.rstr <- raster("TabsD_ch01r.swiss.lv95_201901010000_201912310000.nc")
plot(tmp.rstr[[1]])

extr.tmp <- ncvar_get(temperature, varid="TabsD")
dim(extr.tmp)
lon <- ncvar_get(temperature,"lon")
lat <- ncvar_get(temperature,"lat")
hour <- ncvar_get(temperature,"time")
# hours since 1900-01-01
hour_tr <- as.POSIXct(hour*3600*24, origin="1900-01-01 00:00")
hour_tr <- format(as.POSIXct(hour_tr,format='%Y-%m-%d %H:%M:%S GMT'),format='%Y-%m-%d')

# take the daily mean
dim(extr.tmp) # longitute x latitude x hourly data

sum(is.na(extr.tmp))/length(extr.tmp)

# since I requested land temperature, I assume that the missings are values in the sea. 
# We are going to have a problem with the islands
dim(lon)
dim(lat)
dim(extr.tmp)

listtemp <- list()
for(i in 1:length(hour_tr)){
  print(i)
  mat2store <- data.frame(
    lon = as.vector(as.matrix(lon)),
    lat = as.vector(as.matrix(lat)),
    temperature =  as.vector(as.matrix(extr.tmp[,,i])), 
    date = hour_tr[i])
  
  mat2store <- mat2store[complete.cases(mat2store$temperature),]
  listtemp[[i]] <- mat2store
}

library(ggplot2)
ggplot() + geom_point(data = listtemp[[1]], aes(x = lon, y = lat, col = temperature))



GetTemperature <- do.call(rbind, listtemp)

GetTemperature %>% 
  mutate(ID = group_indices(., lon, lat)) -> GetTemperature

saveRDS(GetTemperature, file = "C:/Users/gkonstan/Desktop/ch excess 2/tmp_ch_2019")

GetTemperature <- readRDS("C:/Users/gkonstan/Desktop/ch excess 2/tmp_ch_2015")

# Now I need to project it on the swiss grid and get the cantonal and weekly mean.

GR.Municipalities <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/shp/YearsCancerRegistry_canton")
GR.Municipalities <- st_as_sf(GR.Municipalities)
plot(GR.Municipalities$geometry)

DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(GR.Municipalities))
DT_sf <- st_coordinates(DT_sf)
head(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)
head(GetTemperature)
tail(GetTemperature)
max(GetTemperature$date)

set.seed(11)
sam <- sample(1:nrow(DT_sf), size = 1000)
plot(DT_sf[sam,])

plot(GR.Municipalities$geometry)
points(DT_sf[sam,], pch = 19, col = "red")

# need to also get the weekly means
class(GetTemperature$date)
GetTemperature$week <- week(GetTemperature$date)
GetTemperature$year <- year(GetTemperature$date)

EUROSTAT_ISO_HMEROLOGIO <- read_excel("E:/Postdoc Imperial/Projects/COVID19 Greece/data/deaths/EUROSTAT_ISO_HMEROLOGIO.xls")

EUROSTAT_ISO_HMEROLOGIO %>% 
  mutate(EURO_TIME = as.Date(format(as.POSIXct(EUROSTAT_ISO_HMEROLOGIO$EURO_TIME,format='%Y-%m-%d UTC'),format='%Y-%m-%d'))) %>% 
  filter(EURO_TIME < as.Date("2016-01-01")) %>% 
  filter(EURO_TIME > as.Date("2014-12-31")) %>% 
  mutate(YEAR = format(EURO_TIME, "%Y")) %>% 
  dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> 
  EUROSTAT_ISO_HMEROLOGIO

# merge the EUROSTAT_ISO with the temperature
EUROSTAT_ISO_HMEROLOGIO$EURO_TIME <- as.character(EUROSTAT_ISO_HMEROLOGIO$EURO_TIME)
GetTemperature <- left_join(GetTemperature, EUROSTAT_ISO_HMEROLOGIO, by = c("date" = "EURO_TIME"))

GetTemperature$ID.wy <- paste0(GetTemperature$ID, GetTemperature$EURO_LABEL)

# take the mean per week
GetTemperature %>% dplyr::group_by(ID.wy) %>% 
  dplyr::mutate(weekly.mean = mean(temperature, na.rm = TRUE)) -> GetTemperature


# remove the daily temperature
GetTemperature_tmp <- GetTemperature[!duplicated(GetTemperature$ID.wy),]
head(GetTemperature_tmp)
dim(GetTemperature_tmp)
GetTemperature_tmp$temperature <- NULL
names(table(GetTemperature_tmp$EURO_LABEL)) -> namtab

53*2 + 4*52
length(namtab)
# seems that all the weeks are here

# Now I need to overlay it on the shp and take the mean by canton and week

loopID <- unique(GetTemperature_tmp$EURO_LABEL)
list.loop <- list()
list.plot <- list()

head(GR.Municipalities)
dim(GR.Municipalities)
GR.Municipalities$CodeELSTAT <- as.numeric(as.factor((GR.Municipalities$NAME)))


for(i in 1:length(loopID)){
  
  print(i)
  
  tmp <- GetTemperature_tmp %>% filter(EURO_LABEL %in% loopID[i])
  
  # summary(tmp) # , no NAs here
  tmp_sf <- st_as_sf(tmp, coords = c("X", "Y"), crs = st_crs(GR.Municipalities))
  # summary(tmp_sf) # , no NAs here
  tmp_sf$X <- tmp$X
  tmp_sf$Y <- tmp$Y
  
  tmp_stjoin <- st_join(GR.Municipalities, tmp_sf)

  # the missings are basically the same week and year, so I will impute accordingly
  tmp_stjoin$EURO_LABEL[is.na(tmp_stjoin$EURO_LABEL)] <- tmp_stjoin$EURO_LABEL[!is.na(tmp_stjoin$EURO_LABEL)][1]
  
  # # Impute temperature
  # # get the NN for the missings
  impNAcoords <- tmp_stjoin[is.na(tmp_stjoin$weekly.mean), c("X", "Y")]
  impNAcoords <- st_coordinates(st_centroid(impNAcoords))
  
  tmp_stjoin <- as.data.frame(tmp_stjoin)
  tmp_stjoin$geometry <- NULL
  
  indNN <- get.knnx(tmp[,c("X", "Y")], impNAcoords, k = 1)$nn.index
  tmp_stjoin$weekly.mean[is.na(tmp_stjoin$weekly.mean)] <- tmp$weekly.mean[indNN]
   
  # and calculate mean temperature of points that fall in a particular municipality 
  
  tmp_stjoin %>% dplyr::group_by(CodeELSTAT) %>% 
    dplyr::mutate(mean.temp = mean(weekly.mean, na.rm = TRUE)) %>% 
    filter(!duplicated(CodeELSTAT)) -> tmp_stjoin
  
  
  tmp_stjoin <- tmp_stjoin[,c("CodeELSTAT", "EURO_LABEL", "mean.temp")]
  tmp_stjoin$CodeELSTAT <- as.character(tmp_stjoin$CodeELSTAT)
  GR.Municipalities$CodeELSTAT <- as.character(GR.Municipalities$CodeELSTAT)
  
  list.loop[[i]] <- tmp_stjoin
  
  GR.Municipalities <- left_join(GR.Municipalities, tmp_stjoin[,c("CodeELSTAT", "mean.temp")], 
                                 by = c("CodeELSTAT" = "CodeELSTAT"))
  
  
  ggplot() + geom_sf(data = GR.Municipalities, aes(fill = mean.temp), col = NA) + theme_bw() + 
    scale_fill_viridis_c() + ggtitle(loopID[i]) -> list.plot[[i]]
  
  GR.Municipalities$mean.temp <- NULL
}

loop.df <- do.call(rbind, list.loop)
colnames(loop.df)
# list.plot[[30]]

boxplot(loop.df$mean.temp)
summary(loop.df) # great there are no missings here

saveRDS(loop.df, file = "TemperatureWeeklyCH_2015")
# loop.df <- readRDS("TemperatureWeeklyCH_210428")


GR.Municipalities$CodeELSTAT


# url <- "https://en.wikipedia.org/wiki/NUTS_statistical_regions_of_Switzerland"
# r <- GET(url)
# doc <- readHTMLTable(
#   doc=content(r, "text"))
# 
# linkCH <- doc$`NULL`
# linkCH <- linkCH[,c(1,2)]
# 
# colnames(linkCH) <- linkCH[1,]
# linkCH <- linkCH[-c(1:2),]
# 
# dim(linkCH)





p1 <- list.plot[[1]]
p2 <- list.plot[[24]]
p3 <- list.plot[[82]]
p4 <- list.plot[[132]]
p5 <- list.plot[[215]]
p6 <- list.plot[[314]]


png("ExploratoryTemperatureCH.png", width = 38, height = 18, res = 300, units = "cm")
print((p1|p2|p3)/(p4|p5|p6))
dev.off()

ggplot() + geom_boxplot(data = loop.df, aes(x=EURO_LABEL, y=mean.temp)) + theme_bw()

sum(is.na(loop.df$mean.temp))

length(table(loop.df$EURO_LABEL))





linkCH <- data.frame(
  NAME = unique(GR.Municipalities$NAME), 
  NUTS3 = c("CH056", "CH021", "CH012", "CH011", "CH070", 
            "CH055", "CH040", "CH022", "CH061", "CH033", 
            "CH062", "CH057", "CH063", "CH025", "CH024", 
            "CH023", "CH051", "CH032", "CH064", "CH065", 
            "CH013", "CH052", "CH053", "CH066", "CH054", 
            "CH031")
)


MunLink <- as.data.frame(GR.Municipalities[,c("NAME", "CodeELSTAT")])
MunLink$geometry <- NULL
MunLink <- left_join(MunLink, linkCH)

loop.df <- left_join(loop.df, MunLink, by = c("CodeELSTAT" = "CodeELSTAT"))
loop.df$id <- paste0(loop.df$EURO_LABEL, loop.df$NUTS3)
loop.df <- loop.df[!duplicated(loop.df$id),]
314*26 # correct

saveRDS(loop.df, file = "TemperatureWeeklyCH_210428")

# 
# tab2link <- as.data.frame(GR.Municipalities[,c("SIGLA", "CodeELSTAT")])
# tab2link$geometry <- NULL
# loop.df <- left_join(loop.df, tab2link, by = c("CodeELSTAT" = "CodeELSTAT"))
# colnames(loop.df)[1] <- "ID"

# saveRDS(loop.df, file = "TemperatureWeeklyItaly_210420")

# great no NAs here

#---- Explanation for removing the weeks "2016-W53" "2018-W53" "2019-W53"
#
#
# # there are some weird missings
# length(names(table(loop.df$EURO_LABEL)))
# length(names(table(deaths$EURO_LABEL)))
# 
# 
# # 4 weeks are missing from the temperature. Which?
# names(table(deaths$EURO_LABEL))[!names(table(deaths$EURO_LABEL)) %in% names(table(loop.df$EURO_LABEL))]
# names(table(loop.df$EURO_LABEL))[!names(table(loop.df$EURO_LABEL)) %in% names(table(deaths$EURO_LABEL))]
# 
# # "2016-W53" "2018-W53" "2019-W53", these weeks are not there, why?
# 
# Tha thelame na sas enimerosoume oti symfona me to diethnes protipo imerominion ISO 8601 kata tin exaetia
# 2015-2020, ta eti 2015 kai 2020 exoun 53 evdomades, eno ta endiamesa eti 2016, 2017, 2018 kai 2019 exoun
# 52 evdomades
# 
# # I have to note that for year 2016:2019 the weeks are 1 less. For 2017 it is in the data
# # nevertheless for 2016, 2018 and 2019 there is W53 but its zero. Thus the missings
# # and the additional weeks make sense and they need to be removed. 
# --- #

















##
##
## Bring CH file together
# 
# deathsCH <- read.csv("E:/Postdoc Imperial/Projects/COVID19 Greece/data/deaths/HistDeathsCH.csv", sep = ";", header = TRUE)
# deathsCH$Obs_status <- NULL
# 
# deathsCH %>% filter(AGE != "_T") %>% filter(SEX != "T") %>% filter(GEO != "CH") -> deathsCH
# deathsCH$year <- substr(deathsCH$TIME_PERIOD, start = 1, stop = 4)
# deathsCH %>% filter(year >= 2015) -> deathsCH
# 
# 
# deathsCH2020 <- read.csv("E:/Postdoc Imperial/Projects/COVID19 Greece/data/deaths/DeathCH_2020.csv", sep = ";", header = TRUE)
# deathsCH2020$OBS_STATUS <- NULL
# 
# deathsCH2020 %>% filter(AGE != "_T") %>% filter(SEX != "T") %>% filter(GEO != "CH") -> deathsCH2020
# deathsCH2020$year <- substr(deathsCH2020$TIME_PERIOD, start = 1, stop = 4)
# 
# colnames(deathsCH)[5] <- "deaths"
# colnames(deathsCH2020)[5] <- "deaths"
# 
# deathsCH <- rbind(deathsCH, deathsCH2020)
# table(deathsCH$AGE)
# 
# # need to create a new one
# 
# deathsCH$ageg <- NA
# deathsCH$ageg[deathsCH$AGE %in% c("Y0T4", "Y5T9", "Y10T14", "Y15T19", 
#                                   "Y20T24", "Y25T29", "Y30T34", "Y35T39")] <- "less40"
# deathsCH$ageg[deathsCH$AGE %in% c("Y40T44", "Y45T49", "Y50T54", "Y55T59")] <- "40-59"
# deathsCH$ageg[deathsCH$AGE %in% c("Y60T64", "Y65T69")] <- "60-69" 
# deathsCH$ageg[deathsCH$AGE %in% c("Y70T74", "Y75T79")] <- "70-79" 
# deathsCH$ageg[deathsCH$AGE %in% c("Y80T84", "Y85T89", "Y_GE90")] <- "80plus" 
#   
# 
# head(deathsCH)
# deathsCH %>% dplyr::group_by(TIME_PERIOD, GEO, SEX, ageg) %>% dplyr::summarise(deaths = sum(deaths)) -> deathsCH
# deathsCH$year <- substr(deathsCH$TIME_PERIOD, start = 1, stop = 4) 
#   
# # cool now the deaths are ready
#  
# length(table(deathsCH$GEO))
# length(table(deathsCH$TIME_PERIOD)) 
# length(table(deathsCH$ageg)) 
# 5*2*314*26  
#   
# # I guess the others are zeros
# expand.grid(TIME_PERIOD = unique(deathsCH$TIME_PERIOD), 
#             GEO = unique(deathsCH$GEO), 
#             SEX = unique(deathsCH$SEX), 
#             ageg = unique(deathsCH$ageg)) -> deathCH_tot
#  
# deathCH_tot <- left_join(deathCH_tot, deathsCH)
# deathCH_tot$deaths[is.na(deathCH_tot$deaths)] <- 0
# sum(deathCH_tot$deaths == 0)/81640
# 
# hist(deathCH_tot$deaths[deathCH_tot$ageg %in% "less40"])
# hist(deathCH_tot$deaths[deathCH_tot$ageg %in% "40-59"])
# hist(deathCH_tot$deaths[deathCH_tot$ageg %in% "60-69"])
# hist(deathCH_tot$deaths[deathCH_tot$ageg %in% "70-79"])
# hist(deathCH_tot$deaths[deathCH_tot$ageg %in% "80plus"])
# 
# sum(deathCH_tot$deaths[deathCH_tot$ageg %in% "less40"] == 0)/81640
# sum(deathCH_tot$deaths[deathCH_tot$ageg %in% "40-59"] == 0)/81640
# sum(deathCH_tot$deaths[deathCH_tot$ageg %in% "60-69"] == 0)/81640
# sum(deathCH_tot$deaths[deathCH_tot$ageg %in% "70-79"] == 0)/81640
# sum(deathCH_tot$deaths[deathCH_tot$ageg %in% "80plus"] == 0)/81640
# 
# 
# deathCH_tot$year <- substr(deathCH_tot$TIME_PERIOD, start = 1, stop = 4)
# 
# # add temperature
# deathCH_tot <- left_join(deathCH_tot, loop.df, by = c("TIME_PERIOD" = "EURO_LABEL", "GEO" = "NUTS3"))
# 
# 
# # add holiday
# holCH <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/holCH")
# 
# datlink <- data.frame(
#   nam = unique(holCH$canton),
#   NUTS = c("NA", "CH040", "CH062", "CH061", "CH013", "CH033", "CH021", "CH063", 
#            "CH022", "CH054", "CH056", "CH023", "CH053", "CH064", "CH070", 
#            "CH065", "CH031", "CH032", "CH051", "CH066", "CH052", "CH012", 
#            "CH057", "CH024", "CH011", "CH025", "CH055")
# )
# 
# 
# holCH <- left_join(holCH, datlink, by = c("canton" = "nam"))
# 
# # back to the EUROSTAT
# holCH$date <- as.Date(holCH$date)
# holCH <- left_join(holCH, EUROSTAT_ISO_HMEROLOGIO, by = c("date" = "EURO_TIME"))
# 
# # add to deaths
# head(deathCH_tot)
# head(holCH)
# 
# # first add the national holidays
# holCH %>% filter(canton %in% "CH") %>% dplyr::select(EURO_LABEL) %>% mutate(nat_hol = 1) %>% 
#   left_join(deathCH_tot, ., by = c("TIME_PERIOD" = "EURO_LABEL")) -> deathCH_tot
# 
# 
# holCH %>% filter(!(canton %in% "CH")) %>% dplyr::select(EURO_LABEL, NUTS) %>% mutate(cant_hol = 1) %>% 
#   left_join(deathCH_tot, ., by = c("TIME_PERIOD" = "EURO_LABEL", "GEO" = "NUTS")) -> deathCH_tot
# 
# sum(!is.na(deathCH_tot$nat_hol))
# sum(!is.na(deathCH_tot$cant_hol))
# 
# 
# deathCH_tot$nat_hol[is.na(deathCH_tot$nat_hol)] <- 0
# deathCH_tot$cant_hol[is.na(deathCH_tot$cant_hol)] <- 0
# deathCH_tot$hol <- deathCH_tot$nat_hol + deathCH_tot$cant_hol
# deathCH_tot$hol[deathCH_tot$hol == 2] <- 1
# table(deathCH_tot$hol)
# 
# deathCH_tot$nat_hol <- deathCH_tot$cant_hol <- NULL
# 
# 
# # add population
# popCH <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/population/popfinCH")
# 
# head(popCH)
# head(deathCH_tot)
# 
# table(deathCH_tot$SEX)
# 
# 
# table(deathCH_tot$ageg)
# table(popCH$age)
# 
# popCH$age <- as.character(popCH$age)
# popCH$age[popCH$age %in% "40_59"] <- "40-59"
# popCH$age[popCH$age %in% "60_69"] <- "60-69"
# popCH$age[popCH$age %in% "70_79"] <- "70-79"
# 
# 
# deathCH_tot$SEX[deathCH_tot$SEX %in% "F"] <- "female"
# deathCH_tot$SEX[deathCH_tot$SEX %in% "M"] <- "male"
# popCH$sex[popCH$sex %in% "Female"] <- "female"
# popCH$sex[popCH$sex %in% "Male"] <- "male"
# 
# # need to fix the canton names in the population
# datlink$canton_abbr <- substr(datlink$nam, start = 4, stop = 5)
# 
# popCH <- left_join(popCH, datlink, by = c("region" = "canton_abbr"))
# sum(is.na(popCH$NUTS))
# 
# ## check tabs
# table(deathCH_tot$ageg)
# table(popCH$age)
# 
# table(deathCH_tot$SEX)
# table(popCH$sex)
# 
# table(deathCH_tot$GEO)
# table(popCH$NUTS)
# 
# table(deathCH_tot$TIME_PERIOD)
# table(popCH$EURO_LABEL)
# 
# ## check class
# class(deathCH_tot$ageg)
# class(popCH$age)
# 
# class(deathCH_tot$SEX)
# class(popCH$sex)
# 
# class(deathCH_tot$GEO)
# class(popCH$NUTS)
# 
# class(deathCH_tot$TIME_PERIOD)
# class(popCH$EURO_LABEL)
# 
# popCH %>% dplyr::select(age, sex, EURO_LABEL, popfin, NUTS) %>% 
#   left_join(deathCH_tot, ., by = c("TIME_PERIOD" = "EURO_LABEL", 
#                                    "GEO" = "NUTS", 
#                                    "SEX" = "sex", 
#                                    "ageg" = "age")) -> deathCH_tot
# 
# summary(deathCH_tot)
# 
# sum(is.na(deathCH_tot$NAME))
# sum(is.na(deathCH_tot$id))
# sum(is.na(deathCH_tot$CodeELSTAT))
# sum(is.na(deathCH_tot$ageg))
# sum(is.na(deathCH_tot$sex))
# sum(is.na(deathCH_tot$GEO))
# 
# # rename to have it in the stucture desired for the CV to run
# 
# # the structure of the data
# # ID_PE,        the ID of the spatial units
# # age,          the age group. Can be "40<", "40-59", "60-69", "70-79", "80+"
# # sex2,         the sex. Can be "male" or "female"
# # EURO-LABEL,   the id of the week based on eurostat. Can be 2015-W01, 2015-W02, ...
# # year,         the year. Takes values 2015:2020
# # deaths,       the number of deaths.
# # popfin,       the number of population counts
# # hol,          0-1 indicating holiday week
# # mean.temp,    the mean temperature
# 
# 
# head(deathCH_tot)
# colnames(deathCH_tot) <- 
# c("EURO-LABEL", "ID_PE", "sex2", "age", "deaths", "year", "CodeELSTAT", "mean.temp", 
#   "NAME", "id",  "hol", "popfin")
# 
# deathCH_tot %>% dplyr::select(ID_PE, age, sex2, `EURO-LABEL`, year, deaths, popfin, hol, mean.temp) ->
#   deathCH_tot
# 
# saveRDS(deathCH_tot, file = "E:/Postdoc Imperial/Projects/COVID19 Greece/data/deaths/dat4analysis_210429_CH")




################################################################################
################################################################################
################################################################################
################################################################################
