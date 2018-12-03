

#### Don't Change These! 
Term <- 360
DP <- .8
AffordRate <- .3

# FTB Values
DPftb <- .9
AffordRateFTB<- .4
#



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




# name2018 <- list.files("*", path = ".\\Raw_Data\\Income\\Place Data\\2018")
# tbl = list.files(path=paste0(fileloc,"/Raw_Data/Income/Place Data/2018/"), pattern="*.xlsx")
# tbl1 <- paste0(fileloc,"/Raw_Data/Income/Place Data/2018/",tbl)
# out <- vector("list", length(tbl1))

## change spotlight place income data to share drive, but super duper slow if not local...
name2018 <- list.files("*", path = "C:\\Users\\azada\\OneDrive\\Work\\@_Inbox\\_Affordability\\Place Data\\2018")
tbl = list.files(path=paste0("C:\\Users\\azada\\OneDrive\\Work\\@_Inbox\\_Affordability\\Place Data\\2018\\"), pattern="*.xlsx")
tbl1 <- paste0("C:\\Users\\azada\\OneDrive\\Work\\@_Inbox\\_Affordability\\Place Data\\2018\\",tbl)
out <- vector("list", length(tbl1))


for(i in seq_along(tbl1)) {
  out[[i]] <- read.xlsx(tbl1[i], sheetIndex =1, startRow =7, endRow = 18, header = FALSE, region = tbl[i])
  out[[i]]$X3 <- as.numeric(as.character(out[[i]]$X3)) / 100
  out[[i]]$X5 <- as.numeric(as.character(out[[i]]$X5)) / 100
  out[[i]]$X7 <- as.numeric(as.character(out[[i]]$X7)) / 100
  print(tbl1[i])
}
city_inc_datat <- data.table::rbindlist(out)
names(city_inc_datat) <- c("price","Pop2010Num","Pop2010Pct","Pop2016Num","Pop2016Pct",
                    "Pop2021Num","Pop2021Pct", "region")

names_df  <- as.data.frame(t(as.data.frame(stringi::stri_split_fixed(city_inc_datat$region, ",", n = 2))))

# trends_place_name <- (stringr::str_split( city_empdata2a$AreaName, ","))
# trends_name <- sapply(trends_place_name, "[", 1)
# trends_type <- sapply(trends_place_name, "[", 2)
# city_empdata2a$place <- trends_name

city_inc_datat$region <- NULL
city_inc_datat$region <- names_df$V1
city_inc_datat <- city_inc_datat %>% filter( !grepl("County", region) )

names(city_inc_datat) <- c("price","Pop2010Num","Pop2010Pct","Pop2016Num","Pop2016Pct",
                    "Pop2021Num","Pop2021Pct", "region")

empdata2 <- city_inc_datat %>% select(price, region, Pop2016Pct, Pop2021Pct)
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

empdataY <- empdata2a

#redo for previous year
# name2018 <- list.files("*", path = "C:\\Users\\azada\\OneDrive\\Work\\@_Inbox\\_Affordability\\Place Data\\2018")
tbl = list.files(path=paste0("C:\\Users\\azada\\OneDrive\\Work\\@_Inbox\\_Affordability\\Place Data\\2017\\"), pattern="*.xlsx")
tbl1 <- paste0("C:\\Users\\azada\\OneDrive\\Work\\@_Inbox\\_Affordability\\Place Data\\2017\\",tbl)

# tbl = list.files(path=paste0(fileloc,"/Raw_Data/Income/Place Data/2017/"), pattern="*.xlsx")
# tbl1 <- paste0(fileloc,"/Raw_Data/Income/Place Data/2017/",tbl)
out <- vector("list", length(tbl1)) 

for(i in seq_along(tbl1)) {
  out[[i]] <- read.xlsx(tbl1[i], sheetIndex =1, startRow =7, endRow = 18, header = FALSE, region = tbl[i])
  out[[i]]$X3 <- as.numeric(as.character(out[[i]]$X3)) / 100
  out[[i]]$X5 <- as.numeric(as.character(out[[i]]$X5)) / 100
  out[[i]]$X7 <- as.numeric(as.character(out[[i]]$X7)) / 100
  print(tbl1[i])
}
city_inc_datat <- data.table::rbindlist(out)
names(city_inc_datat) <- c("price","Pop2010Num","Pop2010Pct","Pop2016Num","Pop2016Pct",
                           "Pop2021Num","Pop2021Pct", "region")

names_df  <- as.data.frame(t(as.data.frame(stringi::stri_split_fixed(city_inc_datat$region, ",", n = 2))))

# trends_place_name <- (stringr::str_split( city_empdata2a$AreaName, ","))
# trends_name <- sapply(trends_place_name, "[", 1)
# trends_type <- sapply(trends_place_name, "[", 2)
# city_empdata2a$place <- trends_name

city_inc_datat$region <- NULL
city_inc_datat$region <- names_df$V1
city_inc_datat <- city_inc_datat %>% filter( !grepl("County",region) )

names(city_inc_datat) <- c("price","Pop2010Num","Pop2010Pct","Pop2016Num","Pop2016Pct",
                           "Pop2021Num","Pop2021Pct", "region")

empdata2 <- city_inc_datat %>% select(price, region, Pop2016Pct, Pop2021Pct)
names(empdata2) <-  c("price" , "region" , as.character(CurrentYearT0), as.character(CurrentYearT0 +5))

empdata2 <- as.data.frame(empdata2)
empdata2[as.character(CurrentYearT0+1)]   <- empdata2[as.character(CurrentYearT0)] + ( empdata2[ as.character( CurrentYearT0 + 5 ) ] - empdata2[ as.character( CurrentYearT0) ] ) / 5
empdata2[as.character( paste0( CurrentYearT0," Q1" ))] <- empdata2[as.character(CurrentYearT0)] + ( empdata2[ as.character( CurrentYearT0 + 1 ) ] - empdata2[ as.character( CurrentYearT0) ] ) / 4 
empdata2[as.character( paste0( CurrentYearT0," Q2" ))] <- empdata2[as.character(CurrentYearT0)] + ( empdata2[ as.character( CurrentYearT0 + 1 ) ] - empdata2[ as.character( CurrentYearT0) ] ) / 4 * 2 
empdata2[as.character( paste0( CurrentYearT0," Q3" ))] <- empdata2[as.character(CurrentYearT0)] + ( empdata2[ as.character( CurrentYearT0 + 1 ) ] - empdata2[ as.character( CurrentYearT0) ] ) / 4 * 3
empdata2[as.character( paste0( CurrentYearT0," Q4" ))] <- empdata2[as.character(CurrentYearT0)] + ( empdata2[ as.character( CurrentYearT0 + 1 ) ] - empdata2[ as.character( CurrentYearT0) ] ) / 4 * 4

empdata2a <- empdata2 %>% gather( Year, Pct, -region, -price )
empdata2a <- empdata2a %>% filter( Year != as.character(CurrentYearT0), Year != as.character(CurrentYearT0 +1 ), Year != as.character(CurrentYearT0 +5) )

empdata2a$yearQtr2 <- as.yearqtr(as.character(empdata2a$Year))

empdata2a$bracket2 <- plyr::mapvalues(empdata2a$price, from = levels(empdata2a$price), 
                                      to = c("124999", "149999", "24999", "199999", "249999", "34999", "499999", "49999", "74999", 
                                             "500000", "99999", "14999"))

empdata2a$bracket1 <- plyr::mapvalues(empdata2a$bracket2, from = levels(empdata2a$bracket2), 
                                      to = c("99999", "124999", "14999", "149999", "199999", "24999", "249999", "34999", "49999",
                                             "499999", "74999", "0"))

empdata2a$bracket1 <- as.numeric(as.character(empdata2a$bracket1))
empdata2a$bracket2 <- as.numeric(as.character(empdata2a$bracket2))

empdataY0 <- empdata2a

empdataY_Y0 <- rbind(empdataY, empdataY0)
# empdataY_Y0 %>% filter(Year == Y_Qc)
