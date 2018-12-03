## helper.r

#' keep rows that have a certain number of NAs anywhere/somewhere and delete others
#' @param df a data frame
#' @param col restrict to the columns where you would like to search for NA; eg, 3, c(3), 2:5, "place", c("place","age")
#' \cr default is NULL, search for all columns
#' @param n integer or vector, 0, c(3,5), number/range of NAs allowed.
#' \cr Range includes both ends 3<=n<=5
#' \cr Range could be -Inf, Inf
#' @return returns a new df with rows that have NA(s) removed
#' @export
z.na.keep = function(df, col=NULL, n=0){
  if (!is.null(col)) {
    df.temp = df[,col]
  } else {
    df.temp = df
  }
  
  if (length(n)==1){
    if (n==0) {
      # simply call complete.cases which might be faster
      result = df[complete.cases(df.temp),]
    } else {
      # credit: http://stackoverflow.com/a/30461945/2292993
      log <- apply(df.temp, 2, is.na)
      logindex <- apply(log, 1, function(x) sum(x) == n)
      result = df[logindex, ]
    }
  }
  
  if (length(n)==2){
    min = n[1]; max = n[2]
    log <- apply(df.temp, 2, is.na)
    logindex <- apply(log, 1, function(x) {sum(x) >= min && sum(x) <= max})
    result = df[logindex, ]
  }
  
  return(result)
}

is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    library(package_name,character.only=TRUE,quietly=FALSE,verbose=FALSE)
  }
}

na.zero <- function (x) {
  x <-  as.character(x)
  x[(x=="NA")] <- ""
  return(as.character(x))
}

na.zero1 <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}


setdiff2 <- function(x,y) {
  
  #returns a list of the elements of x that are not in y 
   #and the elements of y that are not in x (not the same thing...)
 
  Xdiff = setdiff(x,y)
  
  Ydiff = setdiff(y,x)
  
  list(X_not_in_Y=Xdiff, Y_not_in_X=Ydiff)
  
}


numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()