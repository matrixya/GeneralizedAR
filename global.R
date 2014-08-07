#-----------------------------------------------------------------------------------------------#
# reading the all camp files of the folder
  
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(car))
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(dplyr))
  

# reading in the data
  
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
  
  reg_ex <- function(var,data){
    
    rec <- regexec(pattern=var, text=data)   
    reg_match <- regmatches(data, rec)

    innerfun <- function(y){
      x <- ifelse(length(y)==0,NA,y[2])
      return(x)
    }
    
    values <- sapply(reg_match,innerfun)
    
    return(values)
  }

#----------------------------------------------------------------------------------####  
# reading the surveys varibles of all countries
  
  varlist <- read.delim("SurveyVarList.txt", header=T, stringsAsFactors=F, check.names=F, sep="\t")  
  var<-paste0(varlist$de,"=([0-9]+);")
  
  for(i in seq_along(varlist$de)){
    varname <- varlist$de[i]
    camp_data[,11+i] <- reg_ex(var[i],camp_data$answers)
    names(camp_data)[(11+i)] <- varname
  }
  
  write.table(camp_data,"test.csv",sep="\t",row.names=F,col.names=T,quote=F,na="")  
  
  camp_data <- subset(camp_data,!is.na(camp_data$S4n) & camp_data$S4n!=99)
  
  
  
#-----------------------------------------------------------------------------------------------#  
# S3
  
  #camp_data$S3 <- reg_ex("S3=([0-9]+);",camp_data$answers)
  camp_s3 <- subset(camp_data, S3>0)
  camp_s3$S3 <- recode(camp_s3$S3, 
                     recodes="0=NA;1='male';2='female'",
                     as.factor.result=T,levels=c("male","female"))

  camp_s3 <- subset(camp_s3,!(is.na(camp_s3$S3)))
  
#-----------------------------------------------------------------------------------------------#  
# S4n
  
  #camp_data$S4n <- reg_ex("S4n=([0-9]+);",camp_data$answers)
  
  camp_s4n <- filter(camp_data, S4n > 13, S4n <99)
  camp_s4n$S4n_kk <- recode(camp_s4n$S4n, 
                            recodes="14:17='14-17';18:19='18-19';20:29='20-29';30:39='30-39';40:49='40-49';50:59='50-59';60:98='60+'",
                            as.factor.result=T,
                            levels=c("14-17","18-19","20-29","30-39","40-49","50-59","60+"))
  camp_s4n <- subset(camp_s4n,!is.na(camp_s4n$S4n))
  
  
#-----------------------------------------------------------------------------------------------#  
# S5
  
  #camp_data$S5 <- reg_ex("S5=([0-9]+);",camp_data$answers)
  
  camp_s5 <- subset(camp_data,(!is.na(camp_data$S5)) & camp_data$S5>0)
  camp_s5$S5_kk <- recode(camp_s5$S5, 
                          recodes="1='1';2='2';3='3';4='4';5:hi='5+'",
                          as.factor.result=T,levels=c("1","2","3","4","5+"))
  camp_s5 <- subset(camp_s5,!is.na(camp_s5$S5_kk))
  
  
#-----------------------------------------------------------------------------------------------#
# S13

  #camp_data$S13 <- reg_ex("S13=([0-9]+);",camp_data$answers)

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
  
  
#-----------------------------------------------------------------------------------------------#
# recoding varibles
  
  namy<-names(camp_data)
    
  camp_data$K1_11<-recode(camp_data$K1_11,"0=NA;1=0;2=1")
  
  camp_data$K2_24<-recode(camp_data$K2_24,"1=0;2=1")
  
  camp_data$K2_25<-recode(camp_data$K2_25,"1=0;2=1")
  
  camp_data[,namy[grep("P1",namy)]]<-apply(camp_data[,namy[grep("P1",namy)]],2,function(x) recode(x,"0=NA;1=0;2=1;3=2;4=3"))
  
  camp_data[,namy[grep("P2",namy)]]<-apply(camp_data[,namy[grep("P2",namy)]],2,function(x) recode(x,"0=NA;1=0;2=1;3=2;4=3"))
  
  camp_data[,namy[grep("P3",namy)]]<-apply(camp_data[,namy[grep("P3",namy)]],2,function(x) recode(x,"0=NA;1:2=0;3:4=1"))
  
  camp_data[,namy[grep("P4",namy)]]<-apply(camp_data[,namy[grep("P4",namy)]],2,function(x) recode(x,"0=NA;1:2=0;3:4=1"))
  
  #recoding s3
  camp_data$S3<-recode(camp_data$S3,"0=NA;1=0;2=1")
  
  #recode S4n to S4n_kk
  camp_data$S4n_kk<-recode(camp_data$S4n,"14:19=0;20:29=1;30:39=2;40:49=3;50:59=4;60:98=5")
  
  #recode S4n to S4n_kkv
  camp_data$S4n_kkv<-recode(camp_data$S4n,"14:17=0;18:19=1;19:hi=NA")
  
  
  #recode S5
  camp_data$S5_kk<-recode(camp_data$S5,"0=NA;1=0;2=1;3=2;4=3;5:hi=4")
  
  #recode S6 variableS
  camp_data[,namy[grep("S6",namy)]]<-apply(camp_data[,namy[grep("S6",namy)]],2,function(x) recode(x,"0=NA;1=0;2=1"))
  
  #recode S7
  camp_data$S7<-recode(camp_data$S7,"0=NA;1=0;2=1")
  
  #recode S8
  camp_data$S8<-recode(camp_data$S8,"0=NA;1=0;2=1")
  
  #recode S9_kk
  camp_data$S9_kk<-recode(camp_data$S9,"0=NA;1=0;2:3=1;4:5=2;6=3")
    
  #recode S10_neu
  camp_data$S10_neu<-recode(camp_data$S10,"0=NA;1=0;2=1;3=2;4=3;5=4")
    
  #recode S12_kk
  camp_data$S12_kk<-recode(camp_data$S12,"0=NA;1:2=1;3:4=2;5:6=3;7:8=4;9:11=5;12=0")
  
  #recode S13_kk
  #camp_data$S13_kk<-recode(camp_data$S13,"0=NA;1:2=0;3:4=1;5:6=2;7:8=3;9:11=4;12=NA")
  
  