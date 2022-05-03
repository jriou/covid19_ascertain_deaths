#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: deaths
#:::::::::::::::::::::::::::::

# download deaths from: https://www.bfs.admin.ch/bfs/en/home/statistics/population/births-deaths/deaths.assetdetail.22324726.html
# and store it as ch_deaths_YEAR.csv in the data folder

# Clean deaths

deaths_2000_2021 <- read.csv("data/ch_deaths_2000_2021.csv", sep = ";", header = TRUE)
deaths_2022 <- read.csv("data/ch_deaths_2022.csv", sep = ";", header = TRUE) %>% 
  dplyr::rename(Obs_status=OBS_STATUS,Obs_value=OBS_VALUE)


deaths_10_22 = dplyr::bind_rows(deaths_2000_2021,deaths_2022)

head(deaths_10_22)
tail(deaths_10_22)

sum(deaths_10_22$Obs_value == 0)

deaths_10_22 %>% filter(!(AGE %in% "_T")) %>% filter(!(GEO %in% "CH")) -> deaths_10_22
deaths_10_22 %>% filter(!(SEX %in% "T")) -> deaths_10_22
table(deaths_10_22$AGE,useNA="always")


deaths_10_22$ageg <- NA
deaths_10_22$ageg[deaths_10_22$AGE %in% c("Y0T4", "Y5T9", "Y10T14", "Y15T19", "Y20T24", "Y25T29", "Y30T34", "Y35T39")] <- "less40"
deaths_10_22$ageg[deaths_10_22$AGE %in% c("Y40T44", "Y45T49", "Y50T54", "Y55T59")] <- "40-59"
deaths_10_22$ageg[deaths_10_22$AGE %in% c("Y60T64", "Y65T69")] <- "60-69"
deaths_10_22$ageg[deaths_10_22$AGE %in% c("Y70T74", "Y75T79")] <- "70-79"
deaths_10_22$ageg[deaths_10_22$AGE %in% c("Y80T84", "Y85T89", "Y_GE90")] <- "80plus"
table(deaths_10_22$ageg,useNA="always")

deaths_10_22$Obs_status <- deaths_10_22$AGE <- NULL
head(deaths_10_22)

deaths_10_22$SEX[deaths_10_22$SEX %in% "F"] <- "female"
deaths_10_22$SEX[deaths_10_22$SEX %in% "M"] <- "male"

deaths_10_22 %>% group_by(TIME_PERIOD, GEO, SEX, ageg) %>% summarise(deaths = sum(Obs_value)) -> deaths_10_22


expand.grid(ID_space = unique(deaths_10_22$GEO), age.group = unique(deaths_10_22$ageg), sex = unique(deaths_10_22$SEX), 
            EURO_LABEL = unique(deaths_10_22$TIME_PERIOD)) -> dat2

table(deaths_10_22$GEO)
table(deaths_10_22$ageg)
table(deaths_10_22$SEX)
table(deaths_10_22$TIME_PERIOD)

length(unique(deaths_10_22$GEO))*
  length(unique(deaths_10_22$ageg))*
  length(unique(deaths_10_22$SEX))*
  length(unique(deaths_10_22$TIME_PERIOD))

colnames(deaths_10_22) <- c("EURO_LABEL", "ID_space", "sex", "age.group", "deaths")

deaths_10_22 <- left_join(dat2, deaths_10_22, by = c("EURO_LABEL" = "EURO_LABEL", "sex" = "sex", "age.group" = "age.group", "ID_space" = "ID_space"))
deaths_10_22$deaths[is.na(deaths_10_22$deaths)] <- 0
deaths_10_22$year <- substr(deaths_10_22$EURO_LABEL, start = 1, stop = 4)


# ggplot(deaths_10_22) +
#   geom_point(aes(x=EURO_LABEL,y=deaths,colour=age.group)) +
#   facet_wrap(~ ID_space,scales = "free")

findata <- deaths_10_22
findata %>% filter(year >= 2015) -> findata


##
##
## Bring everything together


hol <- readRDS(file.path(controls$savepoint,"holCH.rds"))
tmp <- readRDS(file.path(controls$savepoint,"TemperatureWeeklyCH.rds"))
shp <- sf::read_sf("data/shp.shp")


nam <- c("Graubünden", "Bern", "Valais", "Vaud", "Ticino", "St. Gallen", "Zürich", "Fribourg", "Luzern", 
  "Aargau", "Uri", "Thurgau", "Schwyz", "Jura", "Neuchâtel", "Solothurn", "Glarus", "Basel-Landschaft", 
  "Obwalden", "Nidwalden", "Genève", "Schaffhausen", "Appenzell Ausserrhoden", "Zug", "Appenzell Innerrhoden", 
  "Basel-Stadt")

linkCH <- data.frame(
  NAME = nam, 
  NUTS3 = c("CH056", "CH021", "CH012", "CH011", "CH070", 
            "CH055", "CH040", "CH022", "CH061", "CH033", 
            "CH062", "CH057", "CH063", "CH025", "CH024", 
            "CH023", "CH051", "CH032", "CH064", "CH065", 
            "CH013", "CH052", "CH053", "CH066", "CH054", 
            "CH031"), 
  CN = c("GR", "BE", "VS", "VD", "TI", "SG", "ZH", "FR", "LU", "AG", "UR", "TG", "SZ", "JU", "NE", "SO", "GL", "BL", "OW", 
         "NW", "GE", "SH", "AR", "ZG", "AI", "BS")
)


EUROSTAT_ISO <- readRDS("data/EUROSTAT_ISO")
findata <- left_join(findata, linkCH, by = c("ID_space" = "NUTS3")) 

EUROSTAT_ISO$EURO_TIME <- as.character(EUROSTAT_ISO$EURO_TIME)
hol <- left_join(hol, EUROSTAT_ISO, by = c("date" = "EURO_TIME"))

holch <- hol %>% filter(canton %in% "CH") %>% filter(!duplicated(EURO_LABEL))
holcn <- hol %>% filter(!(canton %in% "CH")) %>% mutate(ID = paste0(canton, EURO_LABEL)) %>% filter(!duplicated(ID))
holcn$CN <- substr(holcn$canton, start = 4, stop = 5)


findata <- left_join(findata, holch[, c("EURO_LABEL", "hol")], by = c("EURO_LABEL" = "EURO_LABEL"))
findata <- left_join(findata, holcn[, c("EURO_LABEL", "hol", "CN")], by = c("EURO_LABEL" = "EURO_LABEL", "CN" = "CN"))
findata$hol.x[is.na(findata$hol.x)] <- 0
findata$hol.y[is.na(findata$hol.y)] <- 0
findata$hol <- findata$hol.x + findata$hol.y
findata$hol[findata$hol>=1] <- 1

findata$hol.x <- findata$hol.y <- NULL

# and add the temperature 
findata <- left_join(findata, tmp, by = c("NAME" = "NAME", "EURO_LABEL" = "EURO_LABEL"))  

# add canton names
findata <-  left_join(findata,cantons_ids,by=c("CN"="canton"))

saveRDS(findata, file = file.path(controls$savepoint,"findata.rds"))

