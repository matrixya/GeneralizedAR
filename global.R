#-----------------------------------------------------------------------------------------------#
# reading the all camp files of the folder
  
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(car))
  suppressPackageStartupMessages(library(dplyr))

# camp_data <- read.delim("extra/original.csv", header=T,stringsAsFactors=F, check.names=F, sep="\t")
# 
# country_net <- read.delim("extra/network_country.csv", header=T,stringsAsFactors=F, check.names=F, sep="\t")
# country_net <- country_net[,-2]
#   
# merged <- merge(camp_data,country_net,by.x="netID_last_imp",by.y="id",all.x=T)
# merged <- subset(merged, !(is.na(merged$country)))
# 
# write.table(merged,"camp_data.csv", row.names=F, col.names=T, sep="\t", na="", quote=F)

  camp_data <- read.delim("camp_data.csv", header=T,stringsAsFactors=F, check.names=F, sep="\t")  
  
#-----------------------------------------------------------------------------------------------#
# defining a variable of campaign IDs
  unq_country <- unique(camp_data$country)
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
# selecting values of S13
  
S13 <- function(x){
    
  r = regexpr("S13=([0-9]+);",camp_data$answers, perl=TRUE)
  grex = gregexpr("S13=([0-9]+);",camp_data$answers)
    
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
  
camp_data$S13 <- S13(camp_data$answers)
camp_data$S13 <- as.integer(camp_data$S13)

camp_s13 <- subset(camp_data,!is.na(camp_data$S13))
camp_s13 <- subset(camp_data,(camp_data$S13<14)&(camp_data$S13>0))
camp_s13$S13_de <- recode(camp_s13$S13, recodes="1:2='<1.000???';3:4='1.000???-1.999???';5:6='2.000???-2.999???';7:8='3.000???-3.999???';9:11='>4.000???' ",as.factor.result=T,levels=c("<1.000???","1.000???-1.999???","2.000???-2.999???","3.000???-3.999???",">4.000???"))
