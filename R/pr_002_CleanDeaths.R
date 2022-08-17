

library(dplyr)
library(readxl)

# Clean deaths

findata <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/michela code/data/Switzerland/finaldb")
findata$hol <- findata$mean.temperature <- findata$population <- NULL
table(findata$age.group)

UPDATEDDEATHS <- read.csv("C:/Users/gkonstan/Desktop/ch excess 2/ch_deaths_22.csv", sep = ";")


sum(UPDATEDDEATHS$OBS_VALUE == 0)
sum(findata$deaths == 0)

UPDATEDDEATHS %>% filter(!(AGE %in% "_T")) %>% filter(!(GEO %in% "CH")) -> UPDATEDDEATHS
UPDATEDDEATHS %>% filter(!(SEX %in% "T")) -> UPDATEDDEATHS
table(UPDATEDDEATHS$AGE)


UPDATEDDEATHS$ageg <- NA
UPDATEDDEATHS$ageg[UPDATEDDEATHS$AGE %in% c("Y0T4", "Y5T9", "Y10T14", "Y15T19", "Y20T24", "Y25T29", "Y30T34", "Y35T39")] <- "less40"
UPDATEDDEATHS$ageg[UPDATEDDEATHS$AGE %in% c("Y40T44", "Y45T49", "Y50T54", "Y55T59")] <- "40-59"
UPDATEDDEATHS$ageg[UPDATEDDEATHS$AGE %in% c("Y60T64", "Y65T69")] <- "60-69"
UPDATEDDEATHS$ageg[UPDATEDDEATHS$AGE %in% c("Y70T74", "Y75T79")] <- "70-79"
UPDATEDDEATHS$ageg[UPDATEDDEATHS$AGE %in% c("Y80T84", "Y85T89", "Y_GE90")] <- "80plus"
table(UPDATEDDEATHS$ageg)

UPDATEDDEATHS$OBS_STATUS <- UPDATEDDEATHS$AGE <- NULL
head(UPDATEDDEATHS)

UPDATEDDEATHS$SEX[UPDATEDDEATHS$SEX %in% "F"] <- "female"
UPDATEDDEATHS$SEX[UPDATEDDEATHS$SEX %in% "M"] <- "male"


UPDATEDDEATHS %>% group_by(TIME_PERIOD, GEO, SEX, ageg) %>% summarise(deaths = sum(OBS_VALUE)) -> UPDATEDDEATHS


head(findata)
expand.grid(ID_space = unique(UPDATEDDEATHS$GEO), age.group = unique(UPDATEDDEATHS$ageg), sex = unique(UPDATEDDEATHS$SEX), 
            EURO_LABEL = unique(UPDATEDDEATHS$TIME_PERIOD)) -> dat2


table(UPDATEDDEATHS$GEO)
table(UPDATEDDEATHS$ageg)
table(UPDATEDDEATHS$SEX)
table(UPDATEDDEATHS$TIME_PERIOD)

length(unique(UPDATEDDEATHS$GEO))*
  length(unique(UPDATEDDEATHS$ageg))*
  length(unique(UPDATEDDEATHS$SEX))*
  length(unique(UPDATEDDEATHS$TIME_PERIOD))

colnames(UPDATEDDEATHS) <- c("EURO_LABEL", "ID_space", "sex", "age.group", "deaths")

UPDATEDDEATHS <- left_join(dat2, UPDATEDDEATHS, by = c("EURO_LABEL" = "EURO_LABEL", "sex" = "sex", "age.group" = "age.group", "ID_space" = "ID_space"))
UPDATEDDEATHS$deaths[is.na(UPDATEDDEATHS$deaths)] <- 0
UPDATEDDEATHS$year <- substr(UPDATEDDEATHS$EURO_LABEL, start = 1, stop = 4)




table(UPDATEDDEATHS$EURO_LABEL)

head(UPDATEDDEATHS)
head(findata)
UPDATEDDEATHS <- UPDATEDDEATHS[,colnames(findata)]


findata <- rbind(findata, UPDATEDDEATHS)

saveRDS(findata, file = "C:/Users/gkonstan/Desktop/ch excess 2/datdeaths")


##
##
## Bring everything together


hol <- readRDS("C:/Users/gkonstan/Desktop/ch excess 2/holCH")
pop <- readRDS("C:/Users/gkonstan/Desktop/ch excess 2/popfinCH")

tmp <- rbind(
  readRDS("C:/Users/gkonstan/Desktop/ch excess 2/temperature/TemperatureWeeklyCH_2015"), 
  readRDS("C:/Users/gkonstan/Desktop/ch excess 2/temperature/TemperatureWeeklyCH_2016"), 
  readRDS("C:/Users/gkonstan/Desktop/ch excess 2/temperature/TemperatureWeeklyCH_2017"), 
  readRDS("C:/Users/gkonstan/Desktop/ch excess 2/temperature/TemperatureWeeklyCH_2018"), 
  readRDS("C:/Users/gkonstan/Desktop/ch excess 2/temperature/TemperatureWeeklyCH_2019"), 
  readRDS("C:/Users/gkonstan/Desktop/ch excess 2/temperature/TemperatureWeeklyCH_2020"),
  readRDS("C:/Users/gkonstan/Desktop/ch excess 2/temperature/TemperatureWeeklyCH_2021")
)

library(sf)
shp <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/shp/YearsCancerRegistry_canton")

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


EUROSTAT_ISO_HMEROLOGIO <- read_excel("E:/Postdoc Imperial/Projects/COVID19 Greece/data/deaths/EUROSTAT_ISO_HMEROLOGIO.xls")

EUROSTAT_ISO_HMEROLOGIO %>% 
  mutate(EURO_TIME = as.Date(format(as.POSIXct(EUROSTAT_ISO_HMEROLOGIO$EURO_TIME,format='%Y-%m-%d UTC'),format='%Y-%m-%d'))) %>% 
  filter(EURO_TIME < as.Date("2023-01-01")) %>% 
  filter(EURO_TIME > as.Date("2014-12-31")) %>% 
  mutate(YEAR = format(EURO_TIME, "%Y")) %>% 
  dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> 
  EUROSTAT_ISO_HMEROLOGIO
findata <- left_join(findata, linkCH, by = c("ID_space" = "NUTS3")) 

EUROSTAT_ISO_HMEROLOGIO$EURO_TIME <- as.character(EUROSTAT_ISO_HMEROLOGIO$EURO_TIME)
hol <- left_join(hol, EUROSTAT_ISO_HMEROLOGIO, by = c("date" = "EURO_TIME"))

holch <- hol %>% filter(canton %in% "CH") %>% filter(!duplicated(EURO_LABEL))
holcn <- hol %>% filter(!(canton %in% "CH")) %>% mutate(ID = paste0(canton, EURO_LABEL)) %>% filter(!duplicated(ID))
holcn$CN <- substr(holcn$canton, start = 4, stop = 5)


findata <- left_join(findata, holch[, c("EURO_LABEL", "hol")], by = c("EURO_LABEL" = "EURO_LABEL"))
holcn$hol2 <- holcn$hol
holcn$hol <- NULL

head(holcn)
findata <- left_join(findata, holcn[, c("EURO_LABEL", "hol2", "CN")], by = c("EURO_LABEL" = "EURO_LABEL", "CN" = "CN"))
findata$hol[is.na(findata$hol)] <- 0
findata$hol2[is.na(findata$hol2)] <- 0
findata$hol <- findata$hol + findata$hol2
findata$hol[findata$hol>=1] <- 1
table(findata$hol)

findata$hol2 <- NULL
head(findata)

datlinkname <- data.frame(NAME.POP = sort(unique(pop$NUTS318CD)), NAME = as.character(sort(unique(findata$NAME))))
findata <- left_join(findata, datlinkname, by = c("NAME" = "NAME"))
findata <- left_join(findata, pop, by = c("NAME.POP" = "NUTS318CD", "age.group" = "age", "sex" = "sex", "EURO_LABEL" = "EURO_LABEL"))


GR.Municipalities <- readRDS("E:/Postdoc Imperial/Projects/COVID19 Greece/data/shp/YearsCancerRegistry_canton")
GR.Municipalities <- st_as_sf(GR.Municipalities)
GR.Municipalities$IDtmp <- as.numeric(as.factor((GR.Municipalities$NAME)))
linktmp <- data.frame(GR.Municipalities$IDtmp, GR.Municipalities$NAME)
linktmp <- linktmp[!duplicated(linktmp$GR.Municipalities.NAME),]
findata2 <- left_join(findata, linktmp, by = c("NAME" = "GR.Municipalities.NAME"))

26*length(unique(tmp$EURO_LABEL))
tmp %>% filter(CodeELSTAT %in% 10) %>% View()
tmp %>% group_by(CodeELSTAT, EURO_LABEL) %>% summarize(mean.temp = mean(mean.temp)) -> tmp


findata2$GR.Municipalities.IDtmp <- as.character(findata2$GR.Municipalities.IDtmp)

findata2 <- left_join(findata2, tmp, by = c("GR.Municipalities.IDtmp" = "CodeELSTAT", "EURO_LABEL" = "EURO_LABEL"))
summary(findata2)

saveRDS(findata2, file = "C:/Users/gkonstan/Desktop/ch excess 2/fin")


findata2 %>% filter(EURO_LABEL %in% "2015-W01", sex == "male", age.group == "40-59") 



################################################################################
################################################################################
################################################################################
################################################################################


