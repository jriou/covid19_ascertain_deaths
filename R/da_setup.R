#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: initialize R session
#:::::::::::::::::::::::::::::

# load libraries -----

library(MASS)
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(flextable)

# manage encoding ----
Sys.setlocale("LC_CTYPE","french")
Sys.setlocale("LC_TIME", "English")

# set paths ----
path_script = "R/"
path_mm_root = "//adb.intra.admin.ch/BAG$/Teams/Krisen/2019_nCov/03 AG Lage/07_R/_datadump/"
path_dashboard = "//adb.intra.admin.ch/BAG$/Teams/Krisen/2019_nCov/03 AG Lage/07_R/_datadump/Dashboard/"

# source functions ----
fili <- dir(path_script,pattern="da_[0123456789]")
lapply(X = fili, FUN = function(x) {source(paste0(path_script, x), echo=FALSE)} )

# connections ----
link = readRDS("data/link_caterpilar")

# commons ----
cantons_ids = read_csv2("data/correspondance.csv",show_col_types = FALSE)
breaks_age_classes = c(0,40,60,70,80,150)
age_classes = c("0-39","40-59","60-69","70-79","80+")
phases = readRDS(paste0(path_dashboard,gsub("-","",substr(end_date,3,10)),"/COVID19Death_geoRegion_AKL10_w.rds")) %>%
  as_tibble() %>%
  dplyr::filter(!(geoRegion %in% c("CH","CHFL","FL","CH01","CH02","CH03","CH04","CH05","CH06","CH07"))) %>% 
  dplyr::mutate(isoweek=paste0(substr(datum,1,4),"-W",substr(datum,5,6),"-1"),
                week=ISOweek::ISOweek2date(isoweek),
                phase=1+rowSums(across(starts_with("timeframe_phase")))) %>% 
  dplyr::select(week,phase) %>% 
  dplyr::distinct()
date_phases = phases %>% 
  dplyr::mutate(phase=as.character(phase)) %>% 
  dplyr::group_by(phase) %>% 
  dplyr::summarise(start_date=min(week),
                   end_date=max(week)+6)

# small custom functions
qsum = function(a,b,c) paste0(formatC(a, format="f", big.mark=",", digits=0),
                              " (",
                              formatC(b, format="f", big.mark=",", digits=0),
                              " to ",
                              formatC(c, format="f", big.mark=",", digits=0),
                              ")")

# aesthetics
theme_set(theme_bw())
col_labd = "firebrick"
col_excess1 = "chartreuse3"
col_excess2 = "dodgerblue"
