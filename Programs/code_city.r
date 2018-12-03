library(readr)
library(purrr)
library(xlsx)
library(readxl)
# library(dplyr)
# "%wo%" <- function(x, y) x[!x %in% y]
library(lubridate)
library(dplyr)
library(tidyr)
library(here)
library(zoo)
library(tempdisagg)
library(financial)
library(stringr)

#### Variable Edits  ####
##Change These from HAI normal

CurrentPeriod <- "Q2"    # "XXX ENTER  QUARTER IE "Q1" XXX"
CurrentYear <-  2018     # "XXX ENTER  YEAR    IE 2016 XXX"
IntRate <-   4.7        # "XXX ENTERenter interest rate as a PCT ie 4.01 for 4%  
IntRateFTB <-  3.8      # Enter in from the FTB interest rate calculators -> (using average of last 3 months of http://www.freddiemac.com/pmms/pmms5.htm 5 yr )
Sales_US <-   269000      # Enter in NAR figure, if not available keep "0" until ready
Sales_USt0 <-  245500
Sales_USty0 <- 255600
IntRateT0 <- 4.44 
IntRateTY0 <- 4.09



## don't change these 
city_filter <- read_excel("Z:\\Shared With Me\\MemInfo (2)\\R&E\\Jordan\\TrendsProd\\Raw\\Maps\\ProductCities.xlsx")
df_rates <- read.csv(".\\Raw_Data\\Input\\rates.csv", na.strings = "", stringsAsFactors = FALSE)
df_rates$Period <- (as.character(df_rates$Period))
df_rates$X <- NULL

# df_rates_zoo <- zoo(df_rates, order.by = as.yearqtr(df_rates$Period))
# IntRateT0 <- as.numeric(df_rates_zoo[length(df_rates$HAIrate)-1,][[3]])
# IntRateTY0 <- as.numeric(df_rates_zoo[length(df_rates$HAIrate)-4,][[3]])


setwd("Z:\\Shared With Me\\MemInfo (2)\\R&E (Dec 2017)\\Products\\HAI\\!Product\\")
fileloc <- paste0(getwd())
source("./Programs/helper_code_citycalcs.r")

## run in _R_Trends project _city_prices_ingest.r
# if you need to re-run older data combine cityprices

in_city_prices_0 <- read.csv("./Raw_Data/CityPrices/CityPrices.csv")

# in_city_prices_0$yearQtr2 <- zoo::as.yearqtr(paste(str_subin_city_prices_0$Quarter, format = "%Yy.%q")
in_city_prices_0$yearQtr2 <- as.yearqtr(paste( stringr::str_sub(in_city_prices_0$Quarter, start = 1, end = 4)," Q", stringr::str_sub(in_city_prices_0$Quarter, start = 6, end =7)))
in_city_prices <- in_city_prices_0 %>% select(-Quarter, -X, -County)

in_city_prices_YQ <- in_city_prices %>% filter(yearQtr2 == Y_Qc) %>% filter(!is.na(place))
in_city_prices_YQc0 <- in_city_prices %>% filter(yearQtr2 == Y_Qc0)%>% filter(!is.na(place))
in_city_prices_YQcY0 <- in_city_prices %>% filter(yearQtr2 == Y_QcY0) %>% filter(!is.na(place)) #) fix with data later hard coding for now

names(in_city_prices_YQ) <- c("place", Y_Qc, "yearQtr2")
names(in_city_prices_YQc0) <- c("place", Y_Qc0, "yearQtr2")
names(in_city_prices_YQcY0) <- c("place", Y_QcY0, "yearQtr2")

in_city_prices_time <- (in_city_prices_YQ %>% select(-yearQtr2)) %>% 
  left_join((in_city_prices_YQc0 %>% select(-yearQtr2)),  by = c("place" = "place")) %>% 
  left_join((in_city_prices_YQcY0 %>% select(-yearQtr2)), by = c("place" = "place")) 

in_city_prices_time <- na.locf(in_city_prices_time, fromLast = TRUE) %>% mutate(place = as.factor(place)) %>% mutate_if(is.character, as.numeric)   

in_city_prices_YQ  <- in_city_prices_time %>% select(place, eval(Y_Qc)) # %>% filter(yearQtr2 == Y_Qc) 
in_city_prices_YQc0  <- in_city_prices_time %>% select(place, eval(Y_Qc0))
in_city_prices_YQcY0  <- in_city_prices_time %>% select(place, eval(Y_QcY0))

in_city_prices_calc <- in_city_prices_time
in_city_prices_calc$principal <-  .8 * in_city_prices_calc[[eval(Y_Qc)]]
in_city_prices_calc$pmt <- tvm(i=IntRate / 12, n = Term , pv= - I(in_city_prices_calc[[eval(Y_Qc)]] * DP), pmt = NA, pyr = 1, cyr = 1)[,5]
in_city_prices_calc$t_i <- (in_city_prices_calc[[eval(Y_Qc)]] *.0138)/12 ## tax rate
in_city_prices_calc$pti <- (in_city_prices_calc$t_i + in_city_prices_calc$pmt)
in_city_prices_calc$min_inc <- (in_city_prices_calc$pti  * 12)/ AffordRate

city_df.combined <- (empdataY_Y0 %>% mutate( region = toupper(region) )%>% filter(Year == Y_Qc) %>% select( -Year, -yearQtr2)) %>% 
  inner_join(in_city_prices_calc, by = c("region" = "place" )) %>% arrange(region, bracket2)

df.city.HAI <- city_df.combined %>%   
  mutate( HAIt = case_when (
    city_df.combined$min_inc >  city_df.combined$bracket2 ~  0,
    city_df.combined$min_inc <= city_df.combined$bracket2 & city_df.combined$min_inc >  city_df.combined$bracket1    
    ~  (city_df.combined$bracket2 - city_df.combined$min_inc) / (city_df.combined$bracket2 - city_df.combined$bracket1) * city_df.combined$Pct,
    city_df.combined$min_inc <= city_df.combined$bracket2 & city_df.combined$min_inc <= city_df.combined$bracket1    
    ~  city_df.combined$Pct )) %>% 
  group_by( region ) %>% summarise( HAI = sum(HAIt))

df.city.TradAfford <- city_df.combined %>% filter(bracket1 == 0) %>% select(region, eval(Y_Qc),eval(Y_Qc0), eval(Y_Qc0), principal,pmt, t_i, pti, min_inc) %>% 
  inner_join(df.city.HAI, by = "region")

#t0


# names(in_city_prices_YQcY0) <- c("place", Y_QcY0, "yearQtr2")
in_city_prices_time_YQc0 <- in_city_prices_YQc0 %>%
  inner_join(in_city_prices_YQcY0, by = c("place" = "place")) 

in_city_prices_calc_YQc0 <- in_city_prices_time_YQc0
in_city_prices_calc_YQc0$principal <-  .8 * in_city_prices_calc_YQc0[[eval(Y_Qc0)]]
in_city_prices_calc_YQc0$pmt <- tvm(i=IntRateT0 / 12, n = Term , pv= - I(in_city_prices_calc_YQc0[[eval(Y_Qc0)]] * DP), pmt = NA, pyr = 1, cyr = 1)[,5]
in_city_prices_calc_YQc0$t_i <- (in_city_prices_calc_YQc0[[eval(Y_Qc0)]] *.0138)/12 ## tax rate
in_city_prices_calc_YQc0$pti <- (in_city_prices_calc_YQc0$t_i + in_city_prices_calc_YQc0$pmt)
in_city_prices_calc_YQc0$min_inc <- (in_city_prices_calc_YQc0$pti  * 12)/ AffordRate

city_df.combined_YQc0 <- ((empdataY_Y0 %>% mutate( region = toupper(region) ) %>%  filter(Year == (Y_Qc0)) %>% select( -Year, -yearQtr2)) %>% 
          inner_join(in_city_prices_calc_YQc0, by = c("region" = "place" )) %>% arrange(region, bracket2))

df.city.HAI_YQc0 <- city_df.combined_YQc0 %>%   
  mutate( HAIt = case_when (
    city_df.combined_YQc0$min_inc >  city_df.combined_YQc0$bracket2 ~  0,
    city_df.combined_YQc0$min_inc <= city_df.combined_YQc0$bracket2 & city_df.combined_YQc0$min_inc >  city_df.combined_YQc0$bracket1    
    ~  (city_df.combined_YQc0$bracket2 - city_df.combined_YQc0$min_inc) / (city_df.combined_YQc0$bracket2 - city_df.combined_YQc0$bracket1) * city_df.combined_YQc0$Pct,
    city_df.combined_YQc0$min_inc <= city_df.combined_YQc0$bracket2 & city_df.combined_YQc0$min_inc <= city_df.combined_YQc0$bracket1    
    ~  city_df.combined_YQc0$Pct )) %>% 
  group_by( region ) %>% summarise( HAI = sum(HAIt))

df.city.TradAfford_YQc0 <- city_df.combined_YQc0 %>% filter(bracket1 == 0) %>% select(region, eval(Y_Qc0), eval(Y_Qc0), principal,pmt, t_i, pti, min_inc) %>% 
  inner_join(df.city.HAI_YQc0, by = "region")

#Y0

in_city_prices_time_YQcY0 <- (in_city_prices_YQcY0)
in_city_prices_calc_YQcY0 <- in_city_prices_time_YQcY0
in_city_prices_calc_YQcY0$principal <-  .8 * in_city_prices_calc_YQcY0[[eval(Y_QcY0)]]
in_city_prices_calc_YQcY0$pmt <- tvm(i=IntRateTY0 / 12, n = Term , pv= - I(in_city_prices_calc_YQcY0[[eval(Y_QcY0)]] * DP), pmt = NA, pyr = 1, cyr = 1)[,5]
in_city_prices_calc_YQcY0$t_i <- (in_city_prices_calc_YQcY0[[eval(Y_QcY0)]] *.0138)/12 ## tax rate
in_city_prices_calc_YQcY0$pti <- (in_city_prices_calc_YQcY0$t_i + in_city_prices_calc_YQcY0$pmt)
in_city_prices_calc_YQcY0$min_inc <- (in_city_prices_calc_YQcY0$pti  * 12)/ AffordRate

city_df.combined_YQcY0 <- (empdataY_Y0 %>% mutate( region = toupper(region) ) %>% filter(Year == Y_QcY0) %>% select( -Year, -yearQtr2)) %>% 
  inner_join(in_city_prices_calc_YQcY0, by = c("region" = "place" )) %>% arrange(region, bracket2)

df.city.HAI_YQcY0 <- city_df.combined_YQcY0 %>%   
  mutate( HAIt = case_when (
    city_df.combined_YQcY0$min_inc >  city_df.combined_YQcY0$bracket2 ~  0,
    city_df.combined_YQcY0$min_inc <= city_df.combined_YQcY0$bracket2 & city_df.combined_YQcY0$min_inc >  city_df.combined_YQcY0$bracket1    
    ~  (city_df.combined_YQcY0$bracket2 - city_df.combined_YQcY0$min_inc) / (city_df.combined_YQcY0$bracket2 - city_df.combined_YQcY0$bracket1) * city_df.combined_YQcY0$Pct,
    city_df.combined_YQcY0$min_inc <= city_df.combined_YQcY0$bracket2 & city_df.combined_YQcY0$min_inc <= city_df.combined_YQcY0$bracket1    
    ~  city_df.combined_YQcY0$Pct )) %>% 
  group_by( region ) %>% summarise( HAI = sum(HAIt))

df.city.TradAfford_YQcY0 <- city_df.combined_YQcY0 %>% filter(bracket1 == 0) %>% select(region, eval(Y_QcY0), eval(Y_QcY0), principal,pmt, t_i, pti, min_inc) %>% 
  inner_join(df.city.HAI_YQcY0, by = "region")

names(df.city.TradAfford) <- c("region", paste0("Price",eval(Y_Qdir)), paste0("Price",eval(Y_Q0dir)), paste0("principal",eval(Y_Qdir)),
                               paste0("pmt",eval(Y_Qdir)), paste0("t_i",eval(Y_Qdir)), paste0("pti",eval(Y_Qdir)),
                               paste0("min_inc",eval(Y_Qdir)), paste0("HAI",eval(Y_Qdir)))
df.city.TradAfford_YQc0 <- (df.city.TradAfford_YQc0[-2])

names(df.city.TradAfford_YQc0) <- c("region", paste0("principal",eval(Y_Q0dir)),
                                    paste0("pmt",eval(Y_Q0dir)), paste0("t_i",eval(Y_Q0dir)), paste0("pti",eval(Y_Q0dir)),
                                    paste0("min_inc",eval(Y_Q0dir)), paste0("HAI",eval(Y_Q0dir)))

names(df.city.TradAfford_YQcY0) <- c("region", paste0("Price",eval(Y_Qy0dir)), paste0("principal",eval(Y_Qy0dir)),
                                    paste0("pmt",eval(Y_Qy0dir)), paste0("t_i",eval(Y_Qy0dir)), paste0("pti",eval(Y_Qy0dir)),
                                    paste0("min_inc",eval(Y_Qy0dir)), paste0("HAI",eval(Y_Qy0dir)))

df.city.HAI.out <- (df.city.TradAfford) %>% left_join(df.city.TradAfford_YQc0, by = 
                                      c("region" = "region")) %>% left_join(df.city.TradAfford_YQcY0, by = c("region" = "region"))

df.city.HAI.out <- df.city.HAI.out %>% distinct(region, .keep_all = TRUE  )

df.city.HAI.out <- df.city.HAI.out %>% filter(region %in% city_filter$city_OUT)


mainDir <- paste0(fileloc, "/Output_Data")
subDir <- paste0("HAI",Y_Q)

write.csv(df.city.HAI.out, file = paste0(file.path( mainDir, subDir),"/city",subDir,".csv") )



df.city.HAI.output <- df.city.HAI.out %>%  select(1,2,4,5,6,7,8,9)
df.city.HAI.output$Quarter <- Y_Qdir
df.city.HAI.output$QuarterEnding <- as.Date(zoo::as.yearqtr(Y_Qdir, format = "%Y-Q%q"))
df.city.HAI.output$X1 <- NA
df.city.HAI.output2 <- df.city.HAI.output[, c("X1","region", "Quarter", paste0("Price",eval(Y_Qdir)), "QuarterEnding", paste0("pmt",eval(Y_Qdir)),  paste0("principal",eval(Y_Qdir)), paste0("t_i",eval(Y_Qdir)), paste0("pti",eval(Y_Qdir)), paste0("min_inc",eval(Y_Qdir)), paste0("HAI",eval(Y_Qdir)))]
names(df.city.HAI.output2) <- c("X1", "region", "Quarter", "Price", "QuarterEnding", "pmt", "principal", "t_i", "pti", "min_inc", "HAI")

df.city.HAI.output2$QuarterEnding <- as.character(df.city.HAI.output2$QuarterEnding)

write.csv(df.city.HAI.output2, file = paste0(file.path( mainDir, subDir),"/cityLONGbi",subDir,".csv") )



# #read in powerbi and paste back in new data and write back out 
# data_city_bi <- read_csv( "./Output_Data/City_Out/hai_city_powerbi_2017-2018Q22018.csv" ) 
# # data_city_bi <- data_city_bi %>% select(-X1)
# data_city_bi <- bind_rows(data_city_bi, df.city.HAI.output2 )
# write_csv(data_city_bi , "./Output_Data/City_Out/hai_city_powerbinew.csv" )
