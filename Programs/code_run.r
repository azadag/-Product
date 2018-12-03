rm(list = ls(all = TRUE))
setwd("Z:\\Shared With Me\\MemInfo (2)\\R&E (Dec 2017)\\Products\\HAI\\!Product\\")
source(".\\Programs\\helper_code_functions.r")
# load_or_install(c("xlsx","data.table","here","tidyr", "dplyr","plyr", "zoo","tempdisagg","financial", "stringr","lubridate","here"))

library(readxl)
library(xlsx)
library(lubridate)
library(dplyr)
library(tidyr)
library(here)
library(zoo)
library(tempdisagg)
library(financial)
library(stringr)
## read in helper .r

fileloc <- paste0(getwd())

#### Variable Edits  ####
##Change These

CurrentPeriod <- "Q3"    # "XXX ENTER  QUARTER IE "Q1" XXX"
CurrentYear <-  2018     # "XXX ENTER  YEAR    IE 2016 XXX"
IntRate <-   4.77        # "XXX ENTERenter interest rate as a PCT ie 4.01 for 4%  
IntRateFTB <-  3.92      # Enter in from the FTB interest rate calculators -> (using average of last 3 months of http://www.freddiemac.com/pmms/pmms5.htm 5 yr )
Sales_US <-   266900      # Enter in NAR figure, if not available keep "0" until ready
Sales_USt0 <-  268000
Sales_USty0 <- 254700

ll <- parse(file = "./Programs/helper_code_calculations.r")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}
