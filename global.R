#-----------------------------------------------------------------------------------------------#
# reading the all camp files of the folder
  
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(car))
  suppressPackageStartupMessages(library(plyr))

#   camp_data <- read.delim("camp_data.csv", header=T, stringsAsFactors=F, check.names=F, sep="\t", nrow=1000)  
#   classes <- sapply(camp_data,class)
#   #classes[c("netID_last_imp","campaignID","netID_survey","surveyID","survey_ts","no_imp","ts_last_imp","week")] <- "integer"
#   classes["userID"] <- "character"
#   #classes[c("country","answers")] <- "character"
  
  camp_data <- read.delim("camp_data.csv", header=T, stringsAsFactors=F, check.names=F, sep="\t")  
  camp_data <- subset(camp_data, !is.na(camp_data$netID_survey))
  
#-----------------------------------------------------------------------------------------------#
# defining a variable of campaign IDs
  unq_country <- unique(camp_data$country)
  unq_campID <- unique(camp_data$campaignID)
  unq_week <- unique(camp_data$week)
  unq_netID <- unique(camp_data$netID_last_imp)
  
#----------------------------------------------------------------------------------####  
# creating the reg_ex function extracting only the values of each variable
  
  reg_ex <- function(var,campdat){
    
    rec <- regexec(pattern=var, text=campdat$answers)   
    reg_match <- regmatches(campdat$answers, rec)

    innerfun <- function(y){
      x <- ifelse(length(y)==0,NA,y[2])
      return(x)
    }
    
    values <- sapply(reg_match,innerfun)
    
    return(values)
  }
  
#-----------------------------------------------------------------------------------------------#  
# S3
  
  camp_data$S3 <- reg_ex("S3=([0-9]);",camp_data)
  
  camp_s3 <- subset(camp_data, S3>0)
  camp_s3$S3 <- recode(camp_s3$S3, 
                     recodes="0=NA;1='male';2='female'",
                     as.factor.result=T,levels=c("male","female"))

  camp_s3 <- subset(camp_s3,!(is.na(camp_s3$S3)))
  
#-----------------------------------------------------------------------------------------------#  
# S4n
  
  camp_data$S4n <- reg_ex("S4n=([0-9][0-9]);",camp_data)
  
  camp_s4n <- subset(camp_data,((camp_data$S4n>13)&(camp_data$S4n<99)))
  camp_s4n$S4n_kk <- recode(camp_s4n$S4n, 
                            recodes="14:17='14-17';18:19='18-19';20:29='20-29';30:39='30-39';40:49='40-49';50:59='50-59';60:98='60+'",
                            as.factor.result=T,
                            levels=c("14-17","18-19","20-29","30-39","40-49","50-59","60+"))
  camp_s4n <- subset(camp_s4n,!is.na(camp_s4n$S4n))
  
  
#-----------------------------------------------------------------------------------------------#  
# S5
  
  camp_data$S5 <- reg_ex("S5=([0-9]+);",camp_data)
  
  camp_s5 <- subset(camp_data,(!is.na(camp_data$S5)) & camp_data$S5>0)
  camp_s5$S5_kk <- recode(camp_s5$S5, 
                          recodes="1='1';2='2';3='3';4='4';5:hi='5+'",
                          as.factor.result=T,levels=c("1","2","3","4","5+"))
  camp_s5 <- subset(camp_s5,!is.na(camp_s5$S5_kk))
  
  
#-----------------------------------------------------------------------------------------------#
# S13

  camp_data$S13 <- reg_ex("S13=([0-9]+);",camp_data)

  camp_data$S13 <- as.integer(camp_data$S13)
  camp_s13 <- subset(camp_data,(camp_data$S13<14)&(camp_data$S13>0))

  
  ## de & at
  camp_s13$S13_kk[camp_s13$country %in% c("de","at") & camp_s13$S13 %in% c(1:2)] <- "very low"
  camp_s13$S13_kk[camp_s13$country %in% c("de","at") & camp_s13$S13 %in% c(3:4)] <- "low"
  camp_s13$S13_kk[camp_s13$country %in% c("de","at") & camp_s13$S13 %in% c(5:6)] <- "average"
  camp_s13$S13_kk[camp_s13$country %in% c("de","at") & camp_s13$S13 %in% c(7:8)] <- "high"  
  camp_s13$S13_kk[camp_s13$country %in% c("de","at") & camp_s13$S13 %in% c(9:11)] <- "very high"
  camp_s13$S13_kk[camp_s13$country %in% c("de","at") & camp_s13$S13 == 12 ] <- NA
  
  #fr and lpm
  camp_s13$S13_kk[camp_s13$country %in% c("fr","lpm") & camp_s13$S13 == 1 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country %in% c("fr","lpm") & camp_s13$S13 == 3 ] <- "low"
  camp_s13$S13_kk[camp_s13$country %in% c("fr","lpm") & camp_s13$S13 == 5 ] <- "average"
  camp_s13$S13_kk[camp_s13$country %in% c("fr","lpm") & camp_s13$S13 == 7 ] <- "high"
  camp_s13$S13_kk[camp_s13$country %in% c("fr","lpm") & camp_s13$S13 == 8 ] <- "very high"
  
  #ch
  camp_s13$S13_kk[camp_s13$country=="ch" &  camp_s13$S13 == 1 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country=="ch" &  camp_s13$S13 %in% 2:4 ] <- "low"
  camp_s13$S13_kk[camp_s13$country=="ch" &  camp_s13$S13 %in% 5:8 ] <- "average"
  camp_s13$S13_kk[camp_s13$country=="ch" &  camp_s13$S13 %in% 9:12 ] <- "high" 
  camp_s13$S13_kk[camp_s13$country=="ch" &  camp_s13$S13 == 13 ] <- "very high"
    
  #dk
  camp_s13$S13_kk[camp_s13$country=="dk" & camp_s13$S13 %in% 1:2 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country=="dk" & camp_s13$S13 %in% 3:4 ] <- "low"
  camp_s13$S13_kk[camp_s13$country=="dk" & camp_s13$S13 %in% 5:6 ] <- "average"
  camp_s13$S13_kk[camp_s13$country=="dk" & camp_s13$S13 %in% 7:8 ] <- "high"
  camp_s13$S13_kk[camp_s13$country=="dk" & camp_s13$S13 %in% 9:11 ] <- "very high"
  camp_s13$S13_kk[camp_s13$country=="dk" & camp_s13$S13 == 12 ] <- NA
  
  #no
  camp_s13$S13_kk[camp_s13$country=="no" & camp_s13$S13 %in% 1:3 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country=="no" & camp_s13$S13 %in% 4:5 ] <- "low"
  camp_s13$S13_kk[camp_s13$country=="no" & camp_s13$S13 %in% 6:7 ] <- "average"
  camp_s13$S13_kk[camp_s13$country=="no" & camp_s13$S13 %in% 8:9 ] <- "high"
  camp_s13$S13_kk[camp_s13$country=="no" & camp_s13$S13 %in% 10:11 ] <- "very high"
  
  #se
  camp_s13$S13_kk[camp_s13$country=="se" & camp_s13$S13 == 1 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country=="se" & camp_s13$S13 %in% 2:3 ] <- "low"
  camp_s13$S13_kk[camp_s13$country=="se" & camp_s13$S13 %in% 4:5 ] <- "average" 
  camp_s13$S13_kk[camp_s13$country=="se" & camp_s13$S13 %in% 6:7 ] <- "high"  
  camp_s13$S13_kk[camp_s13$country=="se" & camp_s13$S13 %in% 8:11 ] <- "very high"
         
  #gr
  camp_s13$S13_kk[camp_s13$country=="gr" & camp_s13$S13 == 1 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country=="gr" & camp_s13$S13 == 2 ] <- "low"
  camp_s13$S13_kk[camp_s13$country=="gr" & camp_s13$S13 %in% 3:4 ] <- "average" 
  camp_s13$S13_kk[camp_s13$country=="gr" & camp_s13$S13 == 5 ] <- "high" 
  camp_s13$S13_kk[camp_s13$country=="gr" & camp_s13$S13 %in% 6:11 ] <- "very high"
        
  #it
  camp_s13$S13_kk[camp_s13$country=="it" & camp_s13$S13 == 1] <- "very low"
  camp_s13$S13_kk[camp_s13$country=="it" & camp_s13$S13 %in% 2:3 ] <- "low" 
  camp_s13$S13_kk[camp_s13$country=="it" & camp_s13$S13 %in% 4:6 ] <- "average"   
  camp_s13$S13_kk[camp_s13$country=="it" & camp_s13$S13 %in% 7:8 ] <- "high"
  camp_s13$S13_kk[camp_s13$country=="it" & camp_s13$S13 %in% 9:11 ] <- "very high"
     
  #pl
  camp_s13$S13_kk[camp_s13$country=="pl" & camp_s13$S13 %in% 1:5 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country=="pl" & camp_s13$S13 %in% 6:7 ] <- "low" 
  camp_s13$S13_kk[camp_s13$country=="pl" & camp_s13$S13 %in% 8:9 ] <- "average" 
  camp_s13$S13_kk[camp_s13$country=="pl" & camp_s13$S13 == 10 ] <- "high"
  camp_s13$S13_kk[camp_s13$country=="pl" & camp_s13$S13 == 11 ] <- "very high"
    
  #ro, bg, ba
  camp_s13$S13_kk[camp_s13$country %in% c("ro","bg","ba","sr","ua","cz") & camp_s13$S13 %in% 1:2 ] <- "very low"
  camp_s13$S13_kk[camp_s13$country %in% c("ro","bg","ba","sr","ua","cz") & camp_s13$S13 %in% 3:4 ] <- "low" 
  camp_s13$S13_kk[camp_s13$country %in% c("ro","bg","ba","sr","ua","cz") & camp_s13$S13 %in% 5:6 ] <- "average" 
  camp_s13$S13_kk[camp_s13$country %in% c("ro","bg","ba","sr","ua","cz") & camp_s13$S13 %in% 7:8 ] <- "high"
  camp_s13$S13_kk[camp_s13$country %in% c("ro","bg","ba","sr","ua","cz") & camp_s13$S13 %in% 9:11 ] <- "very high"
    
  #sr, cz, ua ?????????????????????????
      
  camp_s13$S13_kk <- factor(camp_s13$S13_kk, levels=c("very low","low","average","high","very high"))
  camp_s13 <- subset(camp_s13,!is.na(camp_s13$S13_kk)) 
  
  