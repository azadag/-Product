#### Don't Change These! 
Term <- 360
DP <- .8
AffordRate <- .3

# FTB Values
DPftb <- .9
AffordRateFTB<- .4


#### Calculations from here Onward ####
Y_Q <- as.yearqtr(paste(CurrentYear,CurrentPeriod))
Y_Qc <- as.character(Y_Q)
Y_Qdir <-  gsub(" ", "-", Y_Qc)                   ## current period, hyphenated format 2015-Q1 etc
Y_Q0dir <- gsub(" ", "-", as.character(Y_Q - .25))  ## one quarter back, i want hyphenated format
Y_Q0dir_Year  <- lubridate::year((Y_Q - .25))  ## YEAR of previous quarter
Y_Qy0dir <- gsub(" ", "-", as.character( Y_Q - 1))   ## one year back from current period, hyphenated format
Y_Qy0dir_Year <- lubridate::year((Y_Q - 1))    ## YEAR of previous year

Y_Qdir2 <-  paste0("Q", lubridate::quarter((Y_Q)),lubridate::year((Y_Q)))  ## current period, hyphenated format 2015-Q1 etc
Y_Q0dir2 <- paste0("Q", lubridate::quarter((Y_Q - .25)),lubridate::year((Y_Q - .25)))  ## one quarter back, i want hyphenated format
Y_Qy0dir2 <- paste0("Q", lubridate::quarter((Y_Q - 1)),lubridate::year((Y_Q - 1)))   ## one year back from current period, hyphenated format

CurrentYearT0 <- CurrentYear -1
Y_Qc0 <- as.character(Y_Q-.25)
Y_QcY0 <- as.character(Y_Q-1)


df_region <- read.csv(".\\Raw_Data\\Region\\region2.csv", na.strings = "", stringsAsFactors = FALSE) %>% select(- OutputOrder2)
df_rates <- read.csv(".\\Raw_Data\\Input\\rates.csv", na.strings = "", stringsAsFactors = FALSE)
df_rates$Period <- (as.character(df_rates$Period))
df_rates$X <- NULL

df_us_prices <- read.csv(".\\Raw_Data\\Input\\us_prices.csv", na.strings = "", stringsAsFactors = FALSE)
df_us_prices$Period <- (as.character(df_us_prices$Period))
df_us_prices$X <- NULL

## checks to see if US prices is in the sheet, skips, or writes in the new values
ifelse( df_us_prices[length(df_us_prices$Period),][[1]] == Y_Qc, print("US price for this quarter Exist in us_prices.csv, Edit sheet manually, if necessary"), 
        df_us_prices[length(df_us_prices$Period)+1,] <- c((as.character(Y_Q)), Sales_US))
write.csv(df_us_prices, ".\\Raw_Data\\Input\\us_prices.csv")

## checks to see if this period rate is already in sheet, skips, or writes in the new values
ifelse( df_rates[length(df_rates$Period),][[1]] == Y_Qc, print("Rates for this quarter exists in the sheet, Edit rates.csv manually, if necessary"), 
        df_rates[length(df_rates$Period)+1,] <- c((as.character(Y_Q)), IntRateFTB, IntRate))
write.csv(df_rates, ".\\Raw_Data\\Input\\rates.csv")

df_rates_zoo <- zoo(df_rates, order.by = as.yearqtr(df_rates$Period))

# read in sales and price quarterly data #  "\\\\laserver\\Meminfo\\R&E (Dec 2017)\\Products\\Sales & Price Report\\"
data_price <-   read.xlsx( paste0(".\\Raw_Data\\Prices\\", 
                                 CurrentYear,"\\",
                                 Y_Qdir, "\\",
                                 Y_Qdir,
                                 " County Sales & Price Statistics.xlsx"), sheetIndex =1, startRow =6, endRow = 67, colIndex = c(1:6), header = TRUE, colClasses = c('character', 'numeric', 'numeric', 'character', 'numeric', 'character'), stringsAsFactors = FALSE) #, region = tbl[i])

data_price$State.Region.County <- str_replace_all(string = data_price$State.Region.County, pattern = "Mariposa And Tuolumne", replacement = "Mariposa")
data_price$State.Region.County <- str_replace_all(string = data_price$State.Region.County, pattern = "Contra-Costa \\(Central County\\)", replacement = "Contra-Costa")

data_price <- data_price[complete.cases(data_price[,1:2]),] ## or use new helper function for this for readability
names(data_price) <-  c("region", as.character(Y_Q), as.character(Y_Q -.25), "Quarter_Rev", as.character(Y_Q - 1), "Year_Rev")
data_price$region <-  as.character(data_price$region) # convert from factor to character

#manually add United States data row into the county Price data.frame; this is
#mildly sketchy as coercing data into a data frame is not a great solution here
USvec <-  list("United States", as.numeric(as.character(df_us_prices[length(df_us_prices$Period),][2])), 
               as.numeric(as.character(df_us_prices[length(df_us_prices$Period)-1,][2])),
               "1",
               as.numeric(as.character(df_us_prices[length(df_us_prices$Period)-4,][2])),
               "1")

data_price <- rbind(data_price, USvec)  
data_priceT0 <- data_price
data_priceTY0 <- data_price


data_price$principal <-  .8 * data_price[[eval(Y_Qc)]]
data_price$pmt <- tvm(i=IntRate / 12, n = Term , pv= - I(data_price[[eval(Y_Qc)]] * DP), pmt = NA, pyr = 1, cyr = 1)[,5]
data_price$t_i <- (data_price[[eval(Y_Qc)]] *.0138)/12 ## tax rate
data_price$pti <- (data_price$t_i + data_price$pmt)
data_price$min_inc <- (data_price$pti  * 12)/ AffordRate

## read income data from income read clean 

tbl = list.files(path=paste0(fileloc,"/Raw_Data/Income/Claritas ", as.character(CurrentYear),"-", as.character(CurrentYear + 5), "/"), pattern="*.xlsx")
tbl1 <- paste0(fileloc,"/Raw_Data/Income/Claritas ",as.character(CurrentYear),"-",as.character(CurrentYear + 5),"/",tbl)
out <- vector("list", length(tbl1)) 

if(CurrentYear < 2018) {
  for(i in seq_along(tbl1)) {
    out[[i]] <- read.xlsx(tbl1[i], sheetIndex =3, startRow =8, endRow = 19, header = FALSE, region = tbl[i])
  }
} else {
  for(i in seq_along(tbl1)) {
    out[[i]] <- read.xlsx(tbl1[i], sheetIndex =1, startRow =7, endRow = 18, header = FALSE, region = tbl[i])
    out[[i]]$X3 <- out[[i]]$X3 / 100
    out[[i]]$X5 <- out[[i]]$X5 / 100
    out[[i]]$X7 <- out[[i]]$X7 / 100
  }
} ## spotlight / claritas data output changed slightly for income data starting with 2018.

empdata <- data.table::rbindlist(out)
names_df  <- as.data.frame(t(as.data.frame(stringi::stri_split_fixed(empdata$region, ",", n = 2))))
empdata$county <- names_df$V1
empdata$region <- NULL
names(empdata) <- c("price","Pop2010Num","Pop2010Pct","Pop2016Num","Pop2016Pct",
                    "Pop2021Num","Pop2021Pct", "region")

empdata2 <- empdata %>% select(price, region, Pop2016Pct, Pop2021Pct)
names(empdata2) <-  c("price" , "region" , "2016", "2021")
names(empdata2) <-  c("price" , "region" , as.character(CurrentYear), as.character(CurrentYear +5))

empdata2 <- as.data.frame(empdata2)
empdata2[as.character(CurrentYear+1)]   <- empdata2[as.character(CurrentYear)] + ( empdata2[ as.character( CurrentYear + 5 ) ] - empdata2[ as.character( CurrentYear) ] ) / 5
empdata2[as.character( paste0( CurrentYear," Q1" ))] <- empdata2[as.character(CurrentYear)] + ( empdata2[ as.character( CurrentYear + 1 ) ] - empdata2[ as.character( CurrentYear) ] ) / 4 
empdata2[as.character( paste0( CurrentYear," Q2" ))] <- empdata2[as.character(CurrentYear)] + ( empdata2[ as.character( CurrentYear + 1 ) ] - empdata2[ as.character( CurrentYear) ] ) / 4 * 2 
empdata2[as.character( paste0( CurrentYear," Q3" ))] <- empdata2[as.character(CurrentYear)] + ( empdata2[ as.character( CurrentYear + 1 ) ] - empdata2[ as.character( CurrentYear) ] ) / 4 * 3
empdata2[as.character( paste0( CurrentYear," Q4" ))] <- empdata2[as.character(CurrentYear)] + ( empdata2[ as.character( CurrentYear + 1 ) ] - empdata2[ as.character( CurrentYear) ] ) / 4 * 4

empdata2a <- empdata2 %>% gather( Year, Pct, -region, -price )
empdata2a <- empdata2a %>% filter( Year != as.character(CurrentYear), Year != as.character(CurrentYear +1 ), Year != as.character(CurrentYear +5) )

empdata2a$yearQtr2 <- as.yearqtr(as.character(empdata2a$Year))

empdata2a$bracket2 <- plyr::mapvalues(empdata2a$price, from = levels(empdata2a$price), 
                                      to = c("124999", "149999", "24999", "199999", "249999", "34999", "499999", "49999", "74999", 
                                             "500000", "99999", "14999"))

empdata2a$bracket1 <- plyr::mapvalues(empdata2a$bracket2, from = levels(empdata2a$bracket2), 
                                      to = c("99999", "124999", "14999", "149999", "199999", "24999", "249999", "34999", "49999",
                                             "499999", "74999", "0"))

empdata2a$bracket1 <- as.numeric(as.character(empdata2a$bracket1))
empdata2a$bracket2 <- as.numeric(as.character(empdata2a$bracket2))

df.dplyr01a <-  empdata2a %>% inner_join(df_region, by = c("region" = "RegionIncomeClar"))
df.dplyr02 <- z.na.keep(df.dplyr01a, col = 8 ) ## uses helper na function to remove income rows not matched to CAR price stats

df.combined <- df.dplyr02 %>%  inner_join(data_price, by = c("RegionSalesCar" = "region" ))

## Affordability calculation
df.combined <- df.combined %>% filter(yearQtr2 == Y_Q) %>% arrange(RegionSalesCar, bracket2)

df.HAI <- df.combined %>%   
  mutate( HAIt = case_when (
    df.combined$min_inc >  df.combined$bracket2                           
    ~  0,
    df.combined$min_inc <= df.combined$bracket2 & df.combined$min_inc >  df.combined$bracket1    
    ~  (df.combined$bracket2 - df.combined$min_inc) / (df.combined$bracket2 - df.combined$bracket1) * df.combined$Pct,
    df.combined$min_inc <= df.combined$bracket2 & df.combined$min_inc <= df.combined$bracket1    
    ~  df.combined$Pct )) %>% 
  group_by( RegionSalesCar ) %>% summarise( HAI = sum(HAIt))

df.TradAfford <-  df.combined  %>% filter(bracket1 == 0) %>% select(RegionSalesCar, 12:15, principal,pmt, t_i, pti, min_inc, OutputOrder3) %>% 
  inner_join(df.HAI, by = "RegionSalesCar") %>% arrange(as.numeric(as.character(OutputOrder3))) %>% select(-OutputOrder3)

### FTB calculation
data_priceFTB <- data_price
data_priceFTB[[eval(Y_Qc)]] <- .85 * data_priceFTB[[eval(Y_Qc)]]
data_priceFTB$principal <-  DPftb * data_priceFTB[[eval(Y_Qc)]]
data_priceFTB$pmt <- tvm(i=IntRateFTB / 12, n = Term , pv= - I(data_priceFTB[[eval(Y_Qc)]] * DPftb), pmt = NA, pyr = 1, cyr = 1)[,5]
data_priceFTB$t_i <- (data_priceFTB[[eval(Y_Qc)]] *.0138)/12 ## tax rate
data_priceFTB$pti <- (data_priceFTB$t_i + data_priceFTB$pmt)
data_priceFTB$min_inc <- (data_priceFTB$pti  * 12)/ AffordRateFTB

df.combinedFTB <- df.dplyr02 %>%  inner_join(data_priceFTB, by = c("RegionSalesCar" = "region" ))

## FTB Affordability calculation
df.combinedFTB <- df.combinedFTB %>% filter(yearQtr2 == Y_Q) %>% arrange(RegionSalesCar, bracket2)

df.HAIFTB <- df.combinedFTB %>%   
  mutate( HAIt = case_when (
    df.combinedFTB$min_inc >  df.combinedFTB$bracket2                           
    ~  0,
    df.combinedFTB$min_inc <= df.combinedFTB$bracket2 & df.combinedFTB$min_inc >  df.combinedFTB$bracket1    
    ~  (df.combinedFTB$bracket2 - df.combinedFTB$min_inc) / (df.combinedFTB$bracket2 - df.combinedFTB$bracket1) * df.combinedFTB$Pct,
    df.combinedFTB$min_inc <= df.combinedFTB$bracket2 & df.combinedFTB$min_inc <= df.combinedFTB$bracket1    
    ~  df.combinedFTB$Pct )) %>% 
  group_by(RegionSalesCar) %>% 
  summarise( HAI = sum(HAIt))

df.FTBAfford <-  df.combinedFTB  %>% filter(bracket1 == 0) %>% select(RegionSalesCar, 12:15, principal,pmt, t_i, pti, min_inc, OutputOrder3) %>% inner_join(df.HAIFTB, by = "RegionSalesCar") %>% 
  arrange(as.numeric(as.character(OutputOrder3))) %>% select(-OutputOrder3)

### PREVIOUS QUARTER / YEAR INCOME REDOS ####
## read income data from income read clean 


tblt0 = list.files( path=paste0(fileloc,"/Raw_Data/Income/Claritas ", as.character(CurrentYearT0), "-" , as.character(CurrentYearT0 + 5), "/"), pattern="*.xlsx")
#tbl = list.files(path=paste0(file,"/Claritas 2016-2021/"), pattern="*.xlsx")
tbl1t0 <- paste0( fileloc,"/Raw_Data/Income/Claritas ",as.character( CurrentYearT0 ),"-",as.character( CurrentYearT0 + 5 ),"/",tblt0)
outt0 <- vector( "list", length( tbl1t0 )) 

if(CurrentYearT0 < 2018) {
  for(i in seq_along(tbl1t0)) {
    outt0[[i]] <- read.xlsx(tbl1t0[i], sheetIndex =3, startRow =8, endRow = 19, header = FALSE, region = tblt0[i])
  }
} else{
  
  for(i in seq_along(tbl1t0)) {
    outt0[[i]] <- read.xlsx(tbl1t0[i], sheetIndex =1, startRow =7, endRow = 18, header = FALSE, region = tblt0[i])
    outt0[[i]]$X3 <- outt0[[i]]$X3 / 100
    outt0[[i]]$X5 <- outt0[[i]]$X5 / 100
    outt0[[i]]$X7 <- outt0[[i]]$X7 / 100
  }
} ## spotlight / claritas data output changed slightly for income data starting with 2018.


empdatat0 <- data.table::rbindlist(outt0)
names_dft0  <- as.data.frame(t(as.data.frame(stringi::stri_split_fixed(empdatat0$region, ",", n = 2))))
empdatat0$county <- names_dft0$V1
empdatat0$region <- NULL
names(empdatat0) <- c("price","Pop2010Num","Pop2010Pct","Pop2015Num","Pop2015Pct",
                      "Pop2020Num","Pop2020Pct", "region")

### XXX fix year names...

empdatat02 <- empdatat0 %>% select(price, region, Pop2015Pct, Pop2020Pct)
names(empdatat02) <-  c("price" , "region" , as.character(CurrentYearT0), as.character(CurrentYearT0 +5))
empdatat02 <- as.data.frame(empdatat02)

empdatat02[as.character(CurrentYearT0+1)]   <- empdatat02[as.character(CurrentYearT0)] + ( empdatat02[ as.character( CurrentYearT0 + 5 ) ] - empdatat02[ as.character( CurrentYearT0) ] ) / 5
empdatat02[as.character( paste0( CurrentYearT0," Q1" ))] <- empdatat02[as.character(CurrentYearT0)] + ( empdatat02[ as.character( CurrentYearT0 + 1 ) ] - empdatat02[ as.character( CurrentYearT0) ] ) / 4 
empdatat02[as.character( paste0( CurrentYearT0," Q2" ))] <- empdatat02[as.character(CurrentYearT0)] + ( empdatat02[ as.character( CurrentYearT0 + 1 ) ] - empdatat02[ as.character( CurrentYearT0) ] ) / 4 * 2 
empdatat02[as.character( paste0( CurrentYearT0," Q3" ))] <- empdatat02[as.character(CurrentYearT0)] + ( empdatat02[ as.character( CurrentYearT0 + 1 ) ] - empdatat02[ as.character( CurrentYearT0) ] ) / 4 * 3
empdatat02[as.character( paste0( CurrentYearT0," Q4" ))] <- empdatat02[as.character(CurrentYearT0)] + ( empdatat02[ as.character( CurrentYearT0 + 1 ) ] - empdatat02[ as.character( CurrentYearT0) ] ) / 4 * 4

empdatat02a <- empdatat02 %>% gather( Year, Pct, -region, -price )
empdatat02a <- empdatat02a %>% filter( Year != as.character(CurrentYearT0), Year != as.character(CurrentYearT0 +1 ), Year != as.character(CurrentYearT0 +5) )

empdatat02a$yearQtr2 <- as.yearqtr(as.character(empdatat02a$Year))

empdatat02a$bracket2 <- plyr::mapvalues(empdatat02a$price, from = levels(empdatat02a$price), 
                                        to = c("124999", "149999", "24999", "199999", "249999", "34999", "499999", "49999", "74999", 
                                               "500000", "99999", "14999"))

empdatat02a$bracket1 <- plyr::mapvalues(empdatat02a$bracket2, from = levels(empdatat02a$bracket2), 
                                        to = c("99999", "124999", "14999", "149999", "199999", "24999", "249999", "34999", "49999",
                                               "499999", "74999", "0"))

empdatat02a$bracket1 <- as.numeric(as.character(empdatat02a$bracket1))
empdatat02a$bracket2 <- as.numeric(as.character(empdatat02a$bracket2))

df.dplyr01t0a <-  empdatat02a %>% inner_join(df_region, by = c("region" = "RegionIncomeClar"))
df.dplyr02t0 <- z.na.keep(df.dplyr01t0a, col = 8 ) ## uses helper na function to remove income rows not matched to CAR price stats

#### Start Prior Year Price Revision Code ####

IntRateT0 <- as.numeric(df_rates_zoo[length(df_rates$HAIrate)-1,][[3]])
### XXX add checker to make sure interest rates are present
### CODE_CUT1
## replace the previous code (which reads in the original data from previous weeks)

data_priceT0 <- data_priceT0 %>% filter(data_priceT0[[eval(Y_Qc0)]] != "NA")

data_priceT0ftb <- data_priceT0 

data_priceT0[[eval(Y_Qc0)]] <- as.numeric(as.character(data_priceT0[[eval(Y_Qc0)]]))
data_priceT0$principal <-  .8 * data_priceT0[[eval(Y_Qc0)]]
data_priceT0$pmt <- tvm(i=IntRateT0 / 12, n = Term , pv= - I(data_priceT0[[eval(Y_Qc0)]] * DP), pmt = NA, pyr = 1, cyr = 1)[,5]
data_priceT0$t_i <- (data_priceT0[[eval(Y_Qc0)]] *.0138)/12 ## tax rate
data_priceT0$pti <- (data_priceT0$t_i + data_priceT0$pmt)
data_priceT0$min_inc <- (data_priceT0$pti  * 12)/ AffordRate

#### read and combine price and income data   ####
# depending on whether you need previous year Q1; or previous Quarter Q2, Q3, Q4 #

ifelse( CurrentPeriod == "Q1", 
        df.combinedT0 <- df.dplyr02t0 %>%  inner_join(data_priceT0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_Qc0) %>% arrange(RegionSalesCar, bracket2),
        df.combinedT0 <- df.dplyr02 %>%  inner_join(data_priceT0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_Qc0) %>% arrange(RegionSalesCar, bracket2))

## Affordability calculation ### 

df.HAIT0 <- df.combinedT0 %>%   
  mutate( HAIt = case_when (
    df.combinedT0$min_inc >  df.combinedT0$bracket2   
    ~  0,
    df.combinedT0$min_inc <= df.combinedT0$bracket2 & df.combinedT0$min_inc >  df.combinedT0$bracket1    
    ~  (df.combinedT0$bracket2 - df.combinedT0$min_inc) / (df.combinedT0$bracket2 - df.combinedT0$bracket1) * df.combinedT0$Pct,
    df.combinedT0$min_inc <= df.combinedT0$bracket2 & df.combinedT0$min_inc <= df.combinedT0$bracket1    
    ~  df.combinedT0$Pct )) %>% 
  group_by(RegionSalesCar) %>% 
  summarise( HAI = sum(HAIt))

df.TradAfford_t0 <-  df.combinedT0  %>% filter(bracket1 == 0) %>% select(RegionSalesCar, 12:15, principal,pmt, t_i, pti, min_inc, OutputOrder3) %>% 
  inner_join(df.HAIT0, by = "RegionSalesCar") %>% arrange(as.numeric(as.character(OutputOrder3))) %>% select(-OutputOrder3)

### FTB calculation
IntRateFTB_t0 <- as.numeric(df_rates_zoo[length(df_rates$HAIrate)-1,][[2]])
data_priceFTB_t0 <- data_priceT0ftb

data_priceFTB_t0[[eval(Y_Qc0)]] <- as.numeric(as.character(data_priceFTB_t0[[eval(Y_Qc0)]]))
data_priceFTB_t0[[eval(Y_Qc0)]] <- .85 * data_priceFTB_t0[[eval(Y_Qc0)]]
data_priceFTB_t0$principal <-  DPftb * data_priceFTB_t0[[eval(Y_Qc0)]]
data_priceFTB_t0$pmt <- tvm(i=IntRateFTB_t0 / 12, n = Term , pv= - I(data_priceFTB_t0[[eval(Y_Qc0)]] * DPftb), pmt = NA, pyr = 1, cyr = 1)[,5]
data_priceFTB_t0$t_i <- (data_priceFTB_t0[[eval(Y_Qc0)]] *.0138)/12 ## tax rate
data_priceFTB_t0$pti <- (data_priceFTB_t0$t_i + data_priceFTB_t0$pmt)
data_priceFTB_t0$min_inc <- (data_priceFTB_t0$pti  * 12)/ AffordRateFTB

ifelse( CurrentPeriod == "Q1", 
        df.combinedFTB_t0 <- df.dplyr02t0 %>%  inner_join(data_priceFTB_t0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_Qc0) %>% arrange(RegionSalesCar, bracket2),
        df.combinedFTB_t0 <- df.dplyr02 %>%  inner_join(data_priceFTB_t0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_Qc0) %>% arrange(RegionSalesCar, bracket2))

## FTB_t0 Affordability calculation

df.HAIFTB_t0 <- df.combinedFTB_t0 %>%   
  mutate( HAIt = case_when (
    df.combinedFTB_t0$min_inc >  df.combinedFTB_t0$bracket2                           
    ~  0,
    df.combinedFTB_t0$min_inc <= df.combinedFTB_t0$bracket2 & df.combinedFTB_t0$min_inc >  df.combinedFTB_t0$bracket1    
    ~  (df.combinedFTB_t0$bracket2 - df.combinedFTB_t0$min_inc) / (df.combinedFTB_t0$bracket2 - df.combinedFTB_t0$bracket1) * df.combinedFTB_t0$Pct,
    df.combinedFTB_t0$min_inc <= df.combinedFTB_t0$bracket2 & df.combinedFTB_t0$min_inc <= df.combinedFTB_t0$bracket1    
    ~  df.combinedFTB_t0$Pct )) %>% 
  group_by(RegionSalesCar) %>% 
  summarise( HAI = sum(HAIt))

df.FTBAfford_t0 <-  df.combinedFTB_t0  %>% filter(bracket1 == 0) %>% select(RegionSalesCar, 12:15, principal,pmt, t_i, pti, min_inc, OutputOrder3) %>% inner_join(df.HAIFTB_t0, by = "RegionSalesCar") %>% 
  arrange(as.numeric(as.character(OutputOrder3))) %>% select(-OutputOrder3)

### Previous Year Calculations ####
# CurrentYearT0 <- CurrentYear -1

### CODE_CUT_2

IntRateTY0 <- as.numeric(df_rates_zoo[length(df_rates$HAIrate)-4,][[3]])

data_priceTY0 <- data_priceTY0 %>% filter(data_priceTY0[[eval(Y_QcY0)]] != "NA")

data_priceTY0ftb <- data_priceTY0 

data_priceTY0[[eval(Y_QcY0)]] <- as.numeric(as.character(data_priceTY0[[eval(Y_QcY0)]]))
data_priceTY0$principal <-  .8 * data_priceTY0[[eval(Y_QcY0)]]
data_priceTY0$pmt <- tvm(i=IntRateTY0 / 12, n = Term , pv= - I(data_priceTY0[[eval(Y_QcY0)]] * DP), pmt = NA, pyr = 1, cyr = 1)[,5]
data_priceTY0$t_i <- (data_priceTY0[[eval(Y_QcY0)]] *.0138)/12 ## tax rate
data_priceTY0$pti <- (data_priceTY0$t_i + data_priceTY0$pmt)
data_priceTY0$min_inc <- (data_priceTY0$pti  * 12)/ AffordRate

#### read and combine price and income data   ####
# depending on whether you need previous year Q1; or previous Quarter Q2, Q3, Q4 #
df.combinedTY0 <- df.dplyr02t0 %>%  inner_join(data_priceTY0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_QcY0) %>% arrange(RegionSalesCar, bracket2)

## Affordability calculation ### 

df.HAITY0 <- df.combinedTY0 %>%   
  mutate( HAIt = case_when (
    df.combinedTY0$min_inc >  df.combinedTY0$bracket2   
    ~  0,
    df.combinedTY0$min_inc <= df.combinedTY0$bracket2 & df.combinedTY0$min_inc >  df.combinedTY0$bracket1    
    ~  (df.combinedTY0$bracket2 - df.combinedTY0$min_inc) / (df.combinedTY0$bracket2 - df.combinedTY0$bracket1) * df.combinedTY0$Pct,
    df.combinedTY0$min_inc <= df.combinedTY0$bracket2 & df.combinedTY0$min_inc <= df.combinedTY0$bracket1    
    ~  df.combinedTY0$Pct )) %>% 
  group_by(RegionSalesCar) %>% 
  summarise( HAI = sum(HAIt))

df.TradAfford_ty0 <-  df.combinedTY0  %>% filter(bracket1 == 0) %>% select(RegionSalesCar, 12:15, principal,pmt, t_i, pti, min_inc, OutputOrder3) %>% 
  inner_join(df.HAITY0, by = "RegionSalesCar") %>% arrange(as.numeric(as.character(OutputOrder3))) %>% select(-OutputOrder3)

### FTB calculation
IntRateFTB_tY0 <- as.numeric(df_rates_zoo[length(df_rates$HAIrate)-4,][[2]])

data_priceFTB_tY0 <- data_priceTY0ftb

### XXX turn into a function
data_priceFTB_tY0[[eval(Y_QcY0)]] <- as.numeric(as.character(data_priceFTB_tY0[[eval(Y_QcY0)]]))
data_priceFTB_tY0[[eval(Y_QcY0)]] <- .85 * data_priceFTB_tY0[[eval(Y_QcY0)]]
data_priceFTB_tY0$principal <-  DPftb * data_priceFTB_tY0[[eval(Y_QcY0)]]
data_priceFTB_tY0$pmt <- tvm(i=IntRateFTB_tY0 / 12, n = Term , pv= - I(data_priceFTB_tY0[[eval(Y_QcY0)]] * DPftb), pmt = NA, pyr = 1, cyr = 1)[,5]
data_priceFTB_tY0$t_i <- (data_priceFTB_tY0[[eval(Y_QcY0)]] *.0138)/12 ## tax rate
data_priceFTB_tY0$pti <- (data_priceFTB_tY0$t_i + data_priceFTB_tY0$pmt)
data_priceFTB_tY0$min_inc <- (data_priceFTB_tY0$pti  * 12)/ AffordRateFTB

# ifelse( CurrentPeriod == "Q1", 
#2015 income data #         df.combinedFTB_tY0 <- df.dplyr02t0 %>%  inner_join(data_priceFTB_tY0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_QcY0) %>% arrange(RegionSalesCar, bracket2),
#2016 income data #         df.combinedFTB_tY0 <- df.dplyr02 %>%  inner_join(data_priceFTB_tY0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_QcY0) %>% arrange(RegionSalesCar, bracket2))

df.combinedFTB_tY0 <- df.dplyr02t0 %>%  inner_join(data_priceFTB_tY0, by = c("RegionSalesCar" = "region" )) %>% filter(yearQtr2 == Y_QcY0) %>% arrange(RegionSalesCar, bracket2)

## FTB_t0 Affordability calculation
# df.combinedFTB_tY0 <- df.combinedFTB_tY0 %>% filter(yearQtr2 == Y_QcY0) %>% arrange(RegionSalesCar, bracket2)

df.HAIFTB_tY0 <- df.combinedFTB_tY0 %>%   
  mutate( HAIt = case_when (
    df.combinedFTB_tY0$min_inc >  df.combinedFTB_tY0$bracket2                           
    ~  0,
    df.combinedFTB_tY0$min_inc <= df.combinedFTB_tY0$bracket2 & df.combinedFTB_tY0$min_inc >  df.combinedFTB_tY0$bracket1    
    ~  (df.combinedFTB_tY0$bracket2 - df.combinedFTB_tY0$min_inc) / (df.combinedFTB_tY0$bracket2 - df.combinedFTB_tY0$bracket1) * df.combinedFTB_tY0$Pct,
    df.combinedFTB_tY0$min_inc <= df.combinedFTB_tY0$bracket2 & df.combinedFTB_tY0$min_inc <= df.combinedFTB_tY0$bracket1    
    ~  df.combinedFTB_tY0$Pct )) %>% 
  group_by(RegionSalesCar) %>% 
  summarise( HAI = sum(HAIt))

df.FTBAfford_ty0 <-  df.combinedFTB_tY0  %>% filter(bracket1 == 0) %>% select(RegionSalesCar, 12:15, principal,pmt, t_i, pti, min_inc, OutputOrder3) %>% inner_join(df.HAIFTB_tY0, by = "RegionSalesCar") %>% 
  arrange(as.numeric(as.character(OutputOrder3))) %>% select(-OutputOrder3)

#### Check for Revisions #### 
# Reads back in data from previous quarter / previous year
# outputs to df.TradAffordOut, df.FTBAffordOut
mainDir <- paste0(fileloc, "/Output_Data")
subDirQY_prevQ <- paste0("HAI",Y_Qc0)
subDirQY_prevY <- paste0("HAI",Y_QcY0)

df.TradAfford_t0_original <- read.csv(file = paste0( file.path( mainDir, subDirQY_prevQ),"/",subDirQY_prevQ,".csv"), stringsAsFactors = FALSE, na.strings= "#N/A" )
df.FTBAfford_t0_original  <- read.csv(file = paste0( file.path(mainDir, subDirQY_prevQ),"/",subDirQY_prevQ,"FTB.csv"), stringsAsFactors = FALSE, na.strings= "#N/A")

df.TradAfford_ty0_original <- read.csv(file = paste0(file.path(mainDir, subDirQY_prevY),"/",subDirQY_prevY,".csv"), stringsAsFactors = FALSE, na.strings= "#N/A")
df.FTBAfford_ty0_original  <- read.csv(file = paste0(file.path(mainDir, subDirQY_prevY),"/",subDirQY_prevY,"FTB.csv"), stringsAsFactors = FALSE, na.strings= "#N/A")

df.TradAfford_t0 <-  df.TradAfford_t0 %>% inner_join( (df.TradAfford_t0_original %>%  select(RegionSalesCar, HAI)), by = c("RegionSalesCar"))
df.TradAfford_t0$Quarter_Rev <- ifelse(round( as.numeric( df.TradAfford_t0$HAI.x), 2) == round(as.numeric(df.TradAfford_t0$HAI.y), 2), "", "R")

df.TradAfford_ty0 <-  df.TradAfford_ty0 %>% inner_join( (df.TradAfford_ty0_original %>%  select(RegionSalesCar, HAI)), by = c("RegionSalesCar"))
df.TradAfford_ty0$Year_Rev   <- ifelse(round( as.numeric(df.TradAfford_ty0$HAI.x), 2) == round(as.numeric( df.TradAfford_ty0$HAI.y), 2), "", "R")

df.TradAffordOut_1 <-  (df.TradAfford_t0 %>% select(RegionSalesCar, HAI.x, Quarter_Rev)) %>% 
  left_join( (df.TradAfford_ty0 %>% select(RegionSalesCar, HAI.x, Year_Rev)), by = "RegionSalesCar")

df.TradAffordOut <- (df.TradAfford %>% select(-Quarter_Rev)) %>% left_join(df.TradAffordOut_1, by = "RegionSalesCar")

## FTB
df.FTBAfford_t0a <-  df.FTBAfford_t0 %>% inner_join( (df.FTBAfford_t0_original %>%  select(RegionSalesCar, HAI)), by = c("RegionSalesCar"))
df.FTBAfford_t0a$Quarter_Rev <- ifelse( round(as.numeric( df.FTBAfford_t0a$HAI.x), 2) == round( as.numeric( df.FTBAfford_t0a$HAI.y), 2), "", "R")

df.FTBAfford_ty0a <-  df.FTBAfford_ty0 %>% inner_join( (df.FTBAfford_ty0_original %>%  select(RegionSalesCar, HAI)), by = c("RegionSalesCar"))
df.FTBAfford_ty0a$Year_Rev   <- ifelse( round(as.numeric( df.FTBAfford_ty0a$HAI.x), 2) == round(as.numeric( df.FTBAfford_ty0a$HAI.y), 2), "", "R")

df.FTBAffordOut_1 <-  (df.FTBAfford_t0a %>% select(RegionSalesCar, HAI.x, Quarter_Rev)) %>%  
  left_join(df.FTBAfford_ty0a %>% select(RegionSalesCar, HAI.x, Year_Rev), by = "RegionSalesCar")
df.FTBAffordOut <- (df.FTBAfford %>% select(-Quarter_Rev)) %>% left_join(df.FTBAffordOut_1, by = "RegionSalesCar")

## output files

subDir <- paste0("HAI",Y_Q)

ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
write.csv(df.TradAffordOut, file = paste0(file.path( mainDir, subDir),"/",subDir,".csv") )
write.csv(df.FTBAffordOut,  file = paste0(file.path( mainDir, subDir),"/",subDir,"FTB.csv") )

subDirQY <- paste0("HAI",Y_Qc0)
write.csv(df.TradAfford_t0, file = paste0(file.path( mainDir, subDir),"/",subDirQY,".csv") )
write.csv(df.FTBAfford_t0a, file = paste0(file.path( mainDir, subDir),"/",subDirQY,"FTB.csv") )

subDirQY0 <- paste0("HAI",Y_QcY0)
write.csv(df.TradAfford_ty0,  file = paste0(file.path(mainDir, subDir),"/",subDirQY0,".csv") )
write.csv(df.FTBAfford_ty0a,  file = paste0(file.path(mainDir, subDir),"/",subDirQY0,"FTB.csv") )

## Historical HAI file reorder and write out to be manually pasted
df_region_hist <- read.csv("./Raw_Data/Region/region_tomNEW.csv") 
names(df_region_hist) <- c("Reorder_TOM", "order_org", "order_out")
df.TradAffordHist <- df.TradAffordOut %>% right_join(df_region_hist, by = c("RegionSalesCar" = "Reorder_TOM")) %>% arrange(order_out) %>% select(10,11,13)
names(df.TradAffordHist) <- c(Y_Qc, Y_Qc0, Y_QcY0 )

df_TradAffordHist <- as.data.frame(t(df.TradAffordHist)) 
names(df_TradAffordHist) <- (df.TradAffordOut %>% right_join(df_region_hist, by = c("RegionSalesCar" = "Reorder_TOM")) %>% arrange(order_out))[[1]]
df_TradAffordHist <- cbind(`Month/QTR` = rownames(df_TradAffordHist), df_TradAffordHist)

## FTB out
df.FTBAffordHist <- df.FTBAffordOut %>% right_join(df_region_hist, by = c("RegionSalesCar" = "Reorder_TOM")) %>% arrange(order_out) %>% select(10,11,13)
names(df.FTBAffordHist) <- c(Y_Qc, Y_Qc0, Y_QcY0 )

df.FTBAffordHist <- as.data.frame(t( df.FTBAffordHist )) 
names(df.FTBAffordHist) <- (df.FTBAffordOut %>% right_join(df_region_hist, by = c("RegionSalesCar" = "Reorder_TOM")) %>% arrange(order_out))[[1]]
df.FTBAffordHist <- cbind(`Month/QTR` = rownames(df.FTBAffordHist), df.FTBAffordHist)

#write out Historical Paste File
write.csv(df_TradAffordHist, file = paste0(file.path(mainDir, subDir),"/_hist_",subDir,"_paste.csv"), row.names = FALSE)
write.csv(df.FTBAffordHist,  file = paste0(file.path(mainDir, subDir),"/_hist_",subDir,"FTB_paste.csv"), row.names = FALSE)

# source(".\\Programs\\code_templates.r")

## Historical HAI file reorder and write out to be manually pasted
df_region_template <- read.csv("./Raw_Data/Region/region_template.csv") 
names(df_region_template) <- c("Region_Sales_Out", "Order", "Region_Sales_Car")

df.FTBAffordOut_template <- df.FTBAffordOut %>% right_join(df_region_template, by = c("RegionSalesCar" = "Region_Sales_Car")) %>% arrange(Order) %>% select(15, 10,11, 12, 13, 14, 2, 8,9) %>% 
  mutate(HAI = round(HAI ,2)  * 100, HAI.x.x = round(HAI.x.x,2 ) * 100, HAI.x.y = round(HAI.x.y,2) * 100,  pti = round(pti, -1), min_inc  = round(min_inc, -1)) %>% 
  replace(is.na(.), '') 
df.TradAffordOut_template <- df.TradAffordOut %>% right_join(df_region_template, by = c("RegionSalesCar" = "Region_Sales_Car")) %>% arrange(Order) %>% select(15, 10,11, 12, 13, 14, 2, 8,9) %>% 
  mutate(HAI = round(HAI ,2)  * 100, HAI.x.x = round(HAI.x.x,2 ) * 100, HAI.x.y = round(HAI.x.y,2) * 100,  pti = round(pti, -1), min_inc  = round(min_inc, -1)) %>% 
  replace(is.na(.), '') 

# figure out how to call the current quarter to round it within a dplyr call
names(df.FTBAffordOut_template) <- c("STATE/REGION/COUNTY", Y_Qdir2, Y_Q0dir2, " ", Y_Qy0dir2, "  ", "Median Home Price", "Monthly Payment Including Taxes & Insurance", "Minimum Qualifying Income")
names(df.TradAffordOut_template) <- c("STATE/REGION/COUNTY", Y_Qdir2, Y_Q0dir2, " ", Y_Qy0dir2, "  ", "Median Home Price", "Monthly Payment Including Taxes & Insurance", "Minimum Qualifying Income")

df.FTBAffordOut_template$`Median Home Price`  <- round( as.numeric(df.FTBAffordOut_template$`Median Home Price`), -1 )
df.TradAffordOut_template$`Median Home Price` <- round( as.numeric(df.TradAffordOut_template$`Median Home Price`), -1 )

df.FTBAffordOut_template <- df.FTBAffordOut_template %>% mutate_at(vars(2:3,5,7:9), funs(as.numeric(.)))
df.TradAffordOut_template<- df.TradAffordOut_template %>% mutate_at(vars(2:3,5,7:9), funs(as.numeric(.)))


### copy template files ####
list.of.files = c( ".\\Raw_Data\\Templates\\XXXX First Time Buyer HAI.xlsx",
                   # ".\\Raw_Data\\Templates\\XXXX Historical First Time Buyer HAI.xlsx",
                   # ".\\Raw_Data\\Templates\\XXXX Historical Traditional HAI.xlsx",
                   ".\\Raw_Data\\Templates\\XXXX Traditional HAI.xlsx")

#Use gsub to edit destination paths

ifelse(!dir.exists( paste0( ."\\Release_Data\\", CurrentYear)),
        dir.create( paste0( ."\\Release_Data\\", CurrentYear)), FALSE)
ifelse(!dir.exists( paste0( ."\\Release_Data\\", CurrentYear, "\\", Y_Qdir)),
        dir.create( paste0( ."\\Release_Data\\", CurrentYear, "\\", Y_Qdir)), FALSE)

list.of.dest.file0 = gsub("XXXX", Y_Qdir ,list.of.files)
list.of.dest.file1 = gsub(".\\Raw_Data\\Templates\\", paste0( dirname(getwd()),"\\", CurrentYear, "\\", Y_Qdir, "\\"), list.of.dest.file0, fixed = TRUE)

## this will only write out if the files DONT exist... for safety. add overwrite = TRUE, delete prior to writing new.
for(i in 1:length(list.of.files)){
  cat("Copying file:",list.of.files[i],"\n")
  file.copy(list.of.files[i],list.of.dest.file1[i])
}

# library(openxlsx)
wb <- openxlsx::loadWorkbook(list.of.dest.file1[1])
openxlsx::writeDataTable( wb, x = df.FTBAffordOut_template, sheet = "Sheet1", startRow = 6, colNames = TRUE,  withFilter = FALSE, stack = FALSE)
openxlsx::saveWorkbook(wb, list.of.dest.file1[1], overwrite = TRUE)

wb2 <- openxlsx::loadWorkbook(list.of.dest.file1[2])
openxlsx::writeDataTable( wb2, x = df.TradAffordOut_template, sheet = "Sheet1", startRow = 6, colNames = TRUE,  withFilter = FALSE, stack = FALSE)
openxlsx::saveWorkbook(wb2, list.of.dest.file1[2], overwrite = TRUE)

save.image( file= paste0( dirname(getwd()), "\\!Product\\Output_Data\\_sourcedata\\", Y_Qdir, ".RData" ))


