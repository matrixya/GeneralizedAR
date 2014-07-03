#-----------------------------------------------------------------------------------------------#
# reading the all camp files of the folder
  
  library(ggplot2)
  library(car)
  library(dplyr)

all.the.files <- list.files("data",full=T)
all.the.data <- lapply(all.the.files, read.delim, header=T, sep="\t", check.names=F)
camp_data <- do.call('rbind',all.the.data)
       
names(camp_data)[6:9] <- c("survey_ts","no_imp","netID_last_imp","ts_last_imp")

#-----------------------------------------------------------------------------------------------#
# transforming of the calender week from UNIX timestamp and saving as a new column
  
ts_to_week <- function(x) {strftime(as.POSIXct(x, origin="1970-01-01"),format="%W")}
camp_data <-transform(camp_data,week=ts_to_week(ts_last_imp))
  
#-----------------------------------------------------------------------------------------------#
# defining a variable of campaign IDs
unq_campID <- unique(camp_data$campaignID)
unq_week <- unique(camp_data$week)
unq_netID <- unique(camp_data$netID_last_imp)
  
#-----------------------------------------------------------------------------------------------#
# selecting values of S3

S3 <- function(x){
  
  r = regexpr("S3=[0-9];",camp_data$answers)
  
  innerfun <- function(y){
    x <- ifelse(y==-1,NA,y)
    return(x)
  }
  
  s <- sapply(r,innerfun)
  
  result = substr(camp_data$answers, start=s+3, stop=s+3)
  
  return(result)
}

camp_data$S3 <- S3(camp_data$answers)

camp_s3 <- subset(camp_data,!(is.na(camp_data$S3)|camp_data$S3==0))
camp_s3$S3 <- recode(camp_s3$S3, recodes="1='male';2='female'",as.factor.result=T,levels=c("male","female"))
  

#-----------------------------------------------------------------------------------------------#
# selecting values of S4n
  
S4n <- function(x){
    
  r = regexpr("S4n=[0-9][0-9];",camp_data$answers)
    
  innerfun <- function(y){
      x <- ifelse(y==-1,NA,y)
      return(x)
    }
    
    s <- sapply(r,innerfun)
    
    result = substr(camp_data$answers, start=s+4, stop=s+5)
    
    return(result)
  }
  
camp_data$S4n <- S4n(camp_data$answers)
  
camp_s4n <- camp_data[((camp_data$S4n>13)&(camp_data$S4n<99)),]
camp_s4n <- subset(camp_data,!is.na(camp_data$S4n))
camp_s4n$S4n_kk <- recode(camp_s4n$S4n, recodes="10:19='14-19';20:29='20-29';30:39='30-39';40:49='40-49';50:59='50-59';60:hi='60+'",as.factor.result=T)

#-----------------------------------------------------------------------------------------------#
# selecting values of S5
  
S5 <- function(x){
    
  r = regexpr("S5=([0-9]+);",camp_data$answers, perl=TRUE)
  grex = gregexpr("S5=([0-9]+);",camp_data$answers)
    
  matched_len <- function(x){attr(x,"match.length")}
  len_char <- sapply(grex,matched_len)
    
  innerfun <- function(y){
    x <- ifelse(y==-1,NA,y)
    return(x)
  }
    
  len_char <- sapply(grex,matched_len)
  s <- sapply(r,innerfun)
    
  s+len_char
    
  result = substr(camp_data$answers, start=s+len_char-3, stop=s+len_char-2)
  result <- gsub("=","",result)
    
  return(result)
}
  
camp_data$S5 <- S5(camp_data$answers)
camp_data$S5 <- as.integer(camp_data$S5)
  
camp_s5 <- subset(camp_data,(!is.na(camp_data$S5)) & camp_data$S5>0)
camp_s5$S5_kk <- recode(camp_s5$S5, recodes="1='1';2='2';3='3';4='4';5:hi='5+'",as.factor.result=T,levels=c("1","2","3","4","5+"))
  
#-----------------------------------------------------------------------------------------------#
# selecting values of S12
  
S12 <- function(x){
    
  r = regexpr("S12=([0-9]+);",camp_data$answers, perl=TRUE)
  grex = gregexpr("S12=([0-9]+);",camp_data$answers)
    
  matched_len <- function(x){attr(x,"match.length")}
  len_char <- sapply(grex,matched_len)
    
  innerfun <- function(y){
  x <- ifelse(y==-1,NA,y)
    return(x)
  }
    
  len_char <- sapply(grex,matched_len)
  s <- sapply(r,innerfun)
    
  s+len_char
    
  result = substr(camp_data$answers, start=s+len_char-3, stop=s+len_char-2)
  result <- gsub("=","",result)
    
  return(result)
}
  
camp_data$S12 <- S12(camp_data$answers)
camp_data$S12 <- as.integer(camp_data$S12)

camp_s12 <- subset(camp_data,!is.na(camp_data$S12))
camp_s12 <- subset(camp_data,(camp_data$S12<13)&(camp_data$S12>0))
camp_s12$S12_kk <- recode(camp_s12$S12, recodes="c(1,2,3,12)='low';4:7='medium';8:11='high'",as.factor.result=T,levels=c("low","medium","high"))
  