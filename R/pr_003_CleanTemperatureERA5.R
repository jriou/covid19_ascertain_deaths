# Created 07.10.2021

# Clean and download temperature


#---------------------------------------------------------------------------------

# Step 1. Download temperature data from ERA5
getwd()

# load packages
library(ecmwfr)

# You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, 
# agree with the terms here: https://cds.climate.copernicus.eu/cdsapp/#!/terms/licence-to-use-copernicus-products,
# log in and once you are ok and logged in, click on your name on the top right next to logout
# and retrieve the information about the API key.

cds.user <- "" # Insert your CDS user here
cds.key <- "" #"Insert_your_CDS_API_KEY_here"

# Set up the API and UID
wf_set_key(user = cds.user, key = cds.key, service = "cds")

if(is.null(cds.user) | is.null(cds.key)) {
  print("You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, and once you are ok and logged in, click on your name on the top right next to logout and retrieve the information about the API key.")
}


request <- list(
  dataset_short_name = "reanalysis-era5-land",
  product_type   = "reanalysis",
  format = "netcdf",
  variable = "2m_temperature",
  date = "2009-12-20/2014-12-27", # this is to match the ISO weeks
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
           "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
           "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(48, 5, 45, 11),
  target = "temperature2010_2022_CH.nc"
)

request <- list(
  dataset_short_name = "reanalysis-era5-land",
  product_type   = "reanalysis",
  format = "netcdf",
  variable = "2m_temperature",
  date = "2014-12-28/2022-05-31", # this is to match the ISO weeks
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
           "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
           "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(48, 5, 45, 11),
  target = "temperature2010_2022_CH.nc"
)



file <- wf_request(user = cds.user,
                   request = request,
                   transfer = TRUE,
                   path = "",
                   time_out = 3600*24,
                   verbose = TRUE)


# and you will get a temperature_Italy.nc file on your working directory. 



# Alternatively one can use this link https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
# select download data, and make the following selection: Temperature: 2m temperature, Years: 2015-2020, Months: Select all, Days: All, Time: Select all, 
# and for the geographical area, select sub-region extraction and use 48, 6, 34, 20 specified as N, W, S, E. Store this file on
# a new folder called "Output" in your working directory as temperature_Italy.nc.



# The temperature_Italy.nc file is also provided for download here: 
# https://drive.google.com/drive/folders/1H7F4PuiLlcRwWtbmsJAGPEWJtLu30BN6?usp=sharing







# Step 2. Clean the temperature file


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
library(stringr)
library(data.table)
library(tibble)

# read the files
temperature <- nc_open("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/data/temperature2015_2022_CH.nc")
extr.tmp <- ncvar_get(temperature, varid="t2m")

# extract space and time
lon <- ncvar_get(temperature,"longitude")
lat <- ncvar_get(temperature,"latitude")
hour <- ncvar_get(temperature,"time")
# hours since 1900-01-01
hour_tr <- as.POSIXct(hour*3600, origin="1900-01-01 00:00")
# Set time zone (UTC)
attr(hour_tr, "tzone") <- "UTC"

# set the correct timezone for Italy
hour_tr <- format(hour_tr, format='%Y-%m-%d', tz = "Europe/Rome")


# and from this string we need to remove the dates outside the 2010-2020 ISO weeks, ie everything before 2014-12-29 and
# after 2021-01-03

# ISO weeks file
EUROSTAT_ISO <- data.frame(
  EURO_TIME = seq(as.Date("2009-12-28"), as.Date("2022-05-01"), by="days")
)

EUROSTAT_ISO %>% mutate(num.week = lubridate::isoweek(EURO_TIME), 
                        YEAR_ISO = lubridate::isoyear(EURO_TIME), 
                        YEAR = year(EURO_TIME)) %>% 
  mutate(CD_EURO = paste0("W", str_pad(num.week, 2, pad = "0")), 
         EURO_LABEL = paste(YEAR_ISO, CD_EURO, sep = "-")) %>% 
  dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> EUROSTAT_ISO

saveRDS(EUROSTAT_ISO, file = "data/EUROSTAT_ISO_2")
UROSTAT_ISO %>% View()

extr.tmp[,,hour_tr>="2014-12-29" & hour_tr<="2022-05-01"] -> tmp
hour_tr[hour_tr>="2014-12-29" & hour_tr<="2022-05-01"] -> hour_tr
dat <- data.frame(start = seq(from = 1, to = length(hour_tr), by = 24), 
                  stop = seq(from = 24, to = length(hour_tr)+23, by = 24))

un.hour <- unique(hour_tr)
un.hour <- un.hour[order(un.hour)]
dat$date <- un.hour


# function to retrieve daily mean
DailyMean <- function(start, stop, date){
  
  tmp <- aaply(extr.tmp[,,start:stop], .margin = c(1,2), .fun = function(Y) mean(Y-273.15))
  tmp <- as.data.frame(tmp)
  
  colnames(tmp) <- lat
  rownames(tmp) <- lon
  
  mat2store <- expand.grid(lon, lat)
  colnames(mat2store) <- c("lon", "lat")
  mat2store <- cbind(mat2store, as.vector(as.matrix(tmp)))  
  
  mat2store <- as.data.frame(mat2store)
  colnames(mat2store)[3] <- "temperature"
  
  mat2store <- as.data.frame(mat2store)
  mat2store$date <- as.Date(date)
  
  mat2store <- mat2store[complete.cases(mat2store$temperature),]
  
  return(mat2store)
}


GetTemperature <- 
  pbapply(dat, 1, function(X){
    
    return(DailyMean(start = X[1], stop = X[2], date = X[3]))
    
  } 
  ) # approximately 5min



GetTemperature <- do.call(rbind, GetTemperature)

GetTemperature %>% 
  group_by(lon, lat) %>% 
  mutate(ID = cur_group_id()) -> GetTemperature


# Now we need the shp in Italy.
mun <- read_sf("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/data/shp.shp")

# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))
DT_sf <- st_coordinates(DT_sf)
DT_sf <- as.data.frame(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)


# We also need to get the weekly means
GetTemperature$week <- week(GetTemperature$date)
GetTemperature$year <- year(GetTemperature$date)


# merge the EUROSTAT_ISO with the temperature
GetTemperature <- left_join(GetTemperature, EUROSTAT_ISO, by = c("date" = "EURO_TIME"))
head(GetTemperature)

GetTemperature$ID.wy <- paste0(GetTemperature$ID, GetTemperature$EURO_LABEL)

# take the mean per week
GetTemperature %>% dplyr::group_by(ID.wy) %>% 
  dplyr::mutate(weekly.mean = mean(temperature, na.rm = TRUE)) -> GetTemperature


# remove the daily temperature
GetTemperature_tmp <- GetTemperature[!duplicated(GetTemperature$ID.wy),]
GetTemperature_tmp$temperature <- NULL
names(table(GetTemperature_tmp$EURO_LABEL)) -> namtab

# Now I need to overlay it on the shp and take the mean by municipality and week
loopID <- unique(GetTemperature_tmp$EURO_LABEL)

mun$IDSpace <- 1:nrow(mun)
mun$IDSpace <- as.character(mun$IDSpace)

# Work on data.table to speed up the filter() computation (line 240)
GetTemperature_tmp <- as.data.table(GetTemperature_tmp)

pblapply(1:length(loopID), function(X){
  
  i <- X
  tmp <- GetTemperature_tmp[EURO_LABEL == loopID[i]]
  tmp_sf <- st_as_sf(tmp, coords = c("X", "Y"), crs = st_crs(mun))
  tmp_sf$X <- tmp$X
  tmp_sf$Y <- tmp$Y
  
  tmp_stjoin <- st_join(mun, tmp_sf)
  
  tmp_stjoin <- as.data.frame(tmp_stjoin)
  tmp_stjoin$geometry <- NULL
  
  # the missings are basically the same week and year, so I will impute accordingly
  tmp_stjoin$EURO_LABEL[is.na(tmp_stjoin$EURO_LABEL)] <- tmp_stjoin$EURO_LABEL[!is.na(tmp_stjoin$EURO_LABEL)][1]
  
  mun %>% filter(NAME %in% "Basel-Stadt") %>% st_centroid() %>% st_coordinates -> nn_g
  cords <- tmp_sf[,c("X", "Y")]
  cords$geometry <- NULL
  tmp_stjoin$weekly.mean[tmp_stjoin$NAME %in% "Basel-Stadt"] <- tmp_sf$weekly.mean[get.knnx(cords, nn_g, k = 1)$nn.index]
  
  
  
  # and calculate mean temperature of points that fall in a particular municipality 
  tmp_stjoin %>% group_by(IDSpace) %>% 
    mutate(mean.temp = mean(weekly.mean, na.rm = TRUE)) %>% 
    filter(!duplicated(IDSpace)) -> tmp_stjoin
  
  tmp_stjoin <- tmp_stjoin[,c("IDSpace", "EURO_LABEL", "mean.temp")]
  tmp_stjoin$IDSpace <- as.character(tmp_stjoin$IDSpace)
  
  return(tmp_stjoin)
}
) -> list.loop


loop.df <- do.call(rbind, list.loop)
tab2link <- as.data.frame(mun[,c("ID_PE", "IDSpace")])
tab2link$geometry <- NULL
loop.df <- left_join(loop.df, tab2link, by = c("IDSpace" = "IDSpace"))
colnames(loop.df)[1] <- "ID"



# The temperature file clean
saveRDS(loop.df, file = "E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/data/TemperatureWeekly2015_2022_CH")


loop.df.old <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/data/TemperatureWeekly2010_2014_CH")

tmp <- rbind(loop.df, loop.df.old)

saveRDS(tmp, file = "E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/data/TemperatureWeekly2010_2022_CH")

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################


