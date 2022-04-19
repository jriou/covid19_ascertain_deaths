


# Created 12.04.2022


# Compare with FSO


###################################################################


library(dplyr)
library(lubridate)
library(ggplot2)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

ch_excess <- read.csv("data/FSO_CH_excess.csv", sep = ";", header = TRUE)
finsamples <- readRDS("savepoint/pois.samples.temp.bma")

ch_excess$NoDeaths_EP <- as.numeric(ch_excess$NoDeaths_EP)
ch_excess$Expected <- as.numeric(ch_excess$Expected)
ch_excess$Diff <- ch_excess$NoDeaths_EP - ch_excess$Expected
sum(ch_excess$Diff, na.rm = TRUE)


ch_excess %>% group_by(Ending) %>% summarize(deaths = sum(NoDeaths_EP), 
                                       expected = sum(Expected), 
                                       LL = sum(LowerB), 
                                       UL = sum(UpperB)) -> tot


tot$week <- as.Date(tot$Ending, format = "%d.%m.%Y")
tot$Ending <- NULL
tot <- tot[-1,]
tot$year <- year(tot$week)


tot %>% arrange(week) -> tot
tot %>% filter(year <= 2021) -> tot
tot$x <- 1:nrow(tot)

#
# and now the ones we calculated

finsamples$EURO_LABEL
finsamples %>% filter(year >= 2020) %>% select(-c(2:8)) %>% group_by(EURO_LABEL) %>% summarize_all(sum) -> weeklydeaths

finsamples %>% 
  group_by(EURO_LABEL) %>% 
  summarize(deaths = sum(deaths))-> deaths


tmpbind <- data.frame(t(apply(weeklydeaths[,-1], 1, quantile, probs = c(0.5, 0.025, 0.975))))
cbind(weeklydeaths$EURO_LABEL, tmpbind) -> tmpplot

tmpplot$x <- 1:nrow(tmpplot)

tmpplot$year <- as.numeric(substr(tmpplot$`weeklydeaths$EURO_LABEL`, start = 1, stop = 4))
tmpplot$month <- as.numeric(substr(tmpplot$`weeklydeaths$EURO_LABEL`, start = 7, stop = 8))

tmpplot %>% group_by(year) %>% summarise(min(x)) -> xax


 ggplot() + 
  geom_ribbon(data = tmpplot, aes(x = x, ymax = X97.5., ymin = X2.5.), alpha = 0.2, fill = "blue") + 
  geom_ribbon(data = tot, aes(x = x, ymax = UL, ymin = LL), alpha = 0.2, fill = "red") + 
  geom_line(data = tot, aes(x = x, y = deaths)) + theme_bw()  





###################################################################
###################################################################
###################################################################
###################################################################



