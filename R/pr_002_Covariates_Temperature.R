#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Clean temperature data CH
#:::::::::::::::::::::::::::::



temp.names <- list.files("sensitive/")[startsWith(list.files("sensitive/"), "TabsD")]
# we do not need 2014
temp.names <- temp.names[-1]
temperature.list <- lapply(paste0("sensitive/", temp.names), nc_open)
# the shp
shp <- read_sf("data/shp.shp")

# ISO weeks file
EUROSTAT_ISO <- data.frame(
  EURO_TIME = seq(as.Date("2014-12-29"), as.Date("2023-01-03"), by="days")
)

EUROSTAT_ISO %>% mutate(num.week = lubridate::isoweek(EURO_TIME), 
                        YEAR_ISO = lubridate::isoyear(EURO_TIME), 
                        YEAR = year(EURO_TIME)) %>% 
  mutate(CD_EURO = paste0("W", str_pad(num.week, 2, pad = "0")), 
         EURO_LABEL = paste(YEAR_ISO, CD_EURO, sep = "-")) %>% 
  dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> EUROSTAT_ISO


# store it because is needed for the other rfiles too.
saveRDS(EUROSTAT_ISO, file = "data/EUROSTAT_ISO")

# run the loop
temperature.loop.list <- list()

t_0 <- Sys.time()
for(i in 1:length(temp.names)){

  print(i)
  
  # extract lat-lon and tim
  temperature <- temperature.list[[i]]
  
  extr.tmp <- ncvar_get(temperature, varid="TabsD")
  
  lon <- ncvar_get(temperature,"lon")
  lat <- ncvar_get(temperature,"lat")
  hour <- ncvar_get(temperature,"time")
  # hours since 1900-01-01
  hour_tr <- as.POSIXct(hour*3600*24, origin="1900-01-01 00:00")
  hour_tr <- format(as.POSIXct(hour_tr,format='%Y-%m-%d %H:%M:%S GMT'),format='%Y-%m-%d')
  
  
  listtemp <- list()
  for(j in 1:length(hour_tr)){
    mat2store <- data.frame(
      lon = as.vector(as.matrix(lon)),
      lat = as.vector(as.matrix(lat)),
      temperature =  as.vector(as.matrix(extr.tmp[,,j])), 
      date = hour_tr[j])
    
    mat2store <- mat2store[complete.cases(mat2store$temperature),]
    listtemp[[j]] <- mat2store
  }
  
  GetTemperature <- do.call(rbind, listtemp)
  
  GetTemperature %>% 
    group_by(lon, lat) %>% 
    mutate(ID = cur_group_id()) -> GetTemperature
  
  # project it to CH grid
  DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
  DT_sf <- st_transform(DT_sf, crs = st_crs(shp))
  DT_sf <- st_coordinates(DT_sf)
  DT_sf <- as_tibble(DT_sf)
  
  GetTemperature <- cbind(GetTemperature, DT_sf)
  
  # need to also get the weekly means
  GetTemperature$week <- week(GetTemperature$date)
  GetTemperature$year <- year(GetTemperature$date)
  
  year <- as.numeric(substr(temp.names[i], start = 24, stop = 27))
  date.start <- paste(year-1, "-12-31", sep = "")
  date.end <- paste(year+1, "-01-01", sep = "")
  
  EUROSTAT_ISO %>% 
    mutate(EURO_TIME = as.Date(format(as.POSIXct(EUROSTAT_ISO$EURO_TIME,format='%Y-%m-%d UTC'),format='%Y-%m-%d'))) %>% 
    filter(EURO_TIME < as.Date(date.end)) %>% 
    filter(EURO_TIME > as.Date(date.start)) %>% 
    mutate(YEAR = format(EURO_TIME, "%Y")) %>% 
    dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> 
    EUROSTAT_ISO_loop
  
  # merge the EUROSTAT_ISO with the temperature
  EUROSTAT_ISO_loop$EURO_TIME <- as.character(EUROSTAT_ISO_loop$EURO_TIME)
  GetTemperature <- left_join(GetTemperature, EUROSTAT_ISO_loop, by = c("date" = "EURO_TIME"))
  
  GetTemperature$ID.wy <- paste0(GetTemperature$ID, GetTemperature$EURO_LABEL)
  
  # take the mean per week
  GetTemperature %>% dplyr::group_by(ID.wy) %>% 
    dplyr::mutate(weekly.mean = mean(temperature, na.rm = TRUE)) -> GetTemperature
  
  
  # remove the daily temperature
  GetTemperature_tmp <- GetTemperature[!duplicated(GetTemperature$ID.wy),]
  GetTemperature_tmp$temperature <- NULL
  
  # Now I need to overlay it on the shp and take the mean by canton also
  
  coords.id <- GetTemperature_tmp %>% as_tibble() %>% dplyr::select(ID, X, Y)
  coords.id <- coords.id[!duplicated(coords.id$ID),]
  
  tmp_sf <- st_as_sf(coords.id, coords = c("X", "Y"), crs = st_crs(shp))
  tmp_sf$X <- coords.id$X
  tmp_sf$Y <- coords.id$Y
  
  tmp_stjoin <- st_join(tmp_sf, shp)
  # there are NAs that are either outside the domain (extend of the domain) or in lakes
  tmp_sf$ID_PE <- tmp_stjoin$ID_PE
  tmp_sf$NAME <- tmp_stjoin$NAME
  
  tmp_sf$geometry <- tmp_sf$X <- tmp_sf$Y <- NULL
  GetTemperature_tmp <- left_join(GetTemperature_tmp, tmp_sf, 
                                  by = c("ID" = "ID"))
  
  # I will remove the NAs and then take the mean by canton
  GetTemperature_tmp <- GetTemperature_tmp[!is.na(GetTemperature_tmp$NAME),]
  
  GetTemperature_tmp %>% 
    group_by(NAME, EURO_LABEL) %>% 
    summarize(temperature = mean(weekly.mean, na.rm = TRUE)) -> temperature.loop.list[[i]]
    
}
t_1 <- Sys.time()
t_1 - t_0 # approx 1.4h


# gather temperature
loop.df <- do.call(rbind, temperature.loop.list)
saveRDS(loop.df, file = file.path(controls$savepoint,"TemperatureWeeklyCH.rds"))

