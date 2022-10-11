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
# library(flextable)
library(ISOweek)
library(data.table)
library(jsonlite)
library(ncdf4)
library(maptools)
library(lctools)
library(raster)
library(spdep)
library(FNN)
library(stringr)
library(INLA)
library(patchwork)
library(zoo)
library(nimble)
library(doParallel) 
library(fastDummies) 
library(cowplot)
library(RColorBrewer)
library(flextable)
library(scales)

# set paths ----
path_script = "R/"
path_mm_root = "//adb.intra.admin.ch/BAG$/Teams/Krisen/2019_nCov/03 AG Lage/07_R/_datadump/"
path_dashboard = "//adb.intra.admin.ch/BAG$/Teams/Krisen/2019_nCov/03 AG Lage/07_R/_datadump/Dashboard/"

# create savepoint repertory if not existing ----
dir.create(file.path(".", controls$savepoint), showWarnings = FALSE)

# source functions ----
fili = dir(path_script,pattern="da_[0123456789]")
fili = fili[!grepl("407",fili)]
lapply(X = fili, FUN = function(x) {source(paste0(path_script, x), echo=FALSE)} )

# connections ----
link = readRDS("data/link_caterpilar")

# commons ----
cantons_ids = read.csv("data/correspondance.csv", fileEncoding = "ISO-8859-3")
breaks_age_classes = c(0,40,60,70,80,150)
age_classes = c("0-39","40-59","60-69","70-79","80+")
phases = readRDS("data/phases.rds") 
date_phases = phases %>%
  dplyr::mutate(phase=as.character(phase)) %>%
  dplyr::group_by(phase) %>%
  dplyr::summarise(start_date=min(week),
                   end_date=max(week)+6)
# update if needed using da_105_update_phases.R

# small custom functions ----
qsum = function(a,b,c) paste0(formatC(a, format="f", big.mark=",", digits=0),
                              " (",
                              formatC(b, format="f", big.mark=",", digits=0),
                              " to ",
                              formatC(c, format="f", big.mark=",", digits=0),
                              ")")
qsumperc = function(a,b,c) paste0(formatC(a*100, format="f", big.mark=",", digits=0),
                              "% (",
                              formatC(b*100, format="f", big.mark=",", digits=0),
                              " to ",
                              formatC(c*100, format="f", big.mark=",", digits=0),
                              ")")
qsumperc2 = function(a,b,c) paste0(formatC(a*100, format="f", big.mark=",", digits=1),
                                  "% (95%CrI: ",
                                  formatC(b*100, format="f", big.mark=",", digits=1),
                                  " to ",
                                  formatC(c*100, format="f", big.mark=",", digits=1),
                                  ")")
qsumperc2alt = function(a,b,c) paste0(formatC(a*100, format="f", big.mark=",", digits=1),
                                   "%, 95%CrI: ",
                                   formatC(b*100, format="f", big.mark=",", digits=1),
                                   " to ",
                                   formatC(c*100, format="f", big.mark=",", digits=1),
                                   "")
qsum2 = function(a,b,c) paste0(formatC(a, format="f", big.mark=",", digits=2),
                              " (95%CrI: ",
                              formatC(b, format="f", big.mark=",", digits=2),
                              " to ",
                              formatC(c, format="f", big.mark=",", digits=2),
                              ")")
summ_samples = function(x) c(quantile(x,probs=c(0.5,0.025,0.975)),prob1=mean(x>1))



# aesthetics ----
theme_set(theme_bw())
col_labd = "firebrick"
col_excess1 = "chartreuse4"
col_excess2 = "dodgerblue"
col_expected = "skyblue"


reverse2_trans <- function() {
  scales::trans_new(
    "reverse2",
    function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
    function(x) -1 * as.numeric(x)
  )
}