suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))

shinyServer(function(input, output) {
  
  ##----------------------------------------------------------------------------##
  # interactive inputs
  
  output$campIDcontrol <- renderUI({
        
    seleted_ctry <- subset(camp_data, country==input$countries)
    camp_choice <- unique(seleted_ctry$campaignID)
    
    selectInput(inputId="campID",
                label=h5("Campaign"),
                choices =camp_choice,
                selected=NULL)
    
  })
    
  output$netIDcontrol <- renderUI({
    
    if (is.null(input$campID))
      return()
    
    selected_camp <- subset(camp_data, country==input$countries& campaignID==input$campID)
    netID_choice <- unique(selected_camp$netID_last_imp)
    netID_choice <- c("all",netID_choice)
             
    checkboxGroupInput(inputId="checknetID", 
                      label = h5("Network"), 
                      choices = netID_choice,
                      selected = "all")
  })
  
  output$weekcontrol <- renderUI({
    
    if (is.null(input$campID))
      return()
    
    selected_camp <- subset(camp_data, country==input$countries & campaignID==input$campID)
    week_choice <- levels(ordered(selected_camp$week))
    week_choice <- c("all",week_choice)
    
    checkboxGroupInput(inputId="checkWeek", 
                              label = h5("Week"), 
                              choices = week_choice,
                              selected = "all")    
  })

  ##----------------------------------------------------------------------------##
  #S3  
  dataset1 <- function(){
    
    if(!is.null(input$countries) & !is.null(input$campID) & !is.null(input$checknetID) & !is.null(input$checkWeek)){
    
      ## ALL Networks --------------------------------------------------------------------------------------------------##
      if(any(input$checknetID=="all")){
        if(any(input$checkWeek=="all")){
          data1 <- subset(camp_s3, country==input$countries & campaignID==input$campID)  
        }else{
          data1 <- subset(camp_s3, country==input$countries & campaignID==input$campID & week %in% c(input$checkWeek))
        }
      }else{
        if(any(input$checkWeek=="all")){
          data1 <- subset(camp_s3, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID))
        }else{
          data1 <- subset(camp_s3, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID) & week %in% c(input$checkWeek))
        }
      }
      
  }else{
    data1 <- subset(camp_s3)
  }
  }

  ##----------------------------------------------------------------------------##
  #S4n
  dataset2 <- function(){
    
    if(!is.null(input$countries) & !is.null(input$campID) & !is.null(input$checknetID) & !is.null(input$checkWeek)){
      
      ## ALL Networks --------------------------------------------------------------------------------------------------##
      if(any(input$checknetID=="all")){
        if(any(input$checkWeek=="all")){
          data2 <- subset(camp_s4n, country==input$countries & campaignID==input$campID)  
        }else{
          data2 <- subset(camp_s4n, country==input$countries & campaignID==input$campID & week %in% c(input$checkWeek))
        }
      }else{
        if(any(input$checkWeek=="all")){
          data2 <- subset(camp_s4n, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID))
        }else{
          data2 <- subset(camp_s4n, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID) & week %in% c(input$checkWeek))
        }
      }
      
    }else{
      data2 <- subset(camp_s4n)
    }
  }
  
  # -------------------------------------------------------
  perbar<-function(dataset,xx){
    q <- ggplot(data= dataset, aes_string(x=xx, fill=xx))+
      geom_bar(aes(y =..count..))+
      geom_text(aes(y = ..count..,
                    label = ifelse((..count..)==0,"",
                    scales::percent((..count..)/sum(..count..)))),
                    stat="bin",colour="darkgreen") 
    q
  }
  
  ##----------------------------------------------------------------------------##
  #S5
  dataset3 <- function(){
    
    if(!is.null(input$countries) & !is.null(input$campID) & !is.null(input$checknetID) & !is.null(input$checkWeek)){
      
      ## ALL Networks --------------------------------------------------------------------------------------------------##
      if(any(input$checknetID=="all")){
        if(any(input$checkWeek=="all")){
          data3 <- subset(camp_s5, country==input$countries & campaignID==input$campID)  
        }else{
          data3 <- subset(camp_s5, country==input$countries & campaignID==input$campID & week %in% c(input$checkWeek))
        }
      }else{
        if(any(input$checkWeek=="all")){
          data3 <- subset(camp_s5, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID))
        }else{
          data3 <- subset(camp_s5, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID) & week %in% c(input$checkWeek))
        }
      }
      
    }else{
      data3 <- subset(camp_s5)
    }
  }
  
  # -------------------------------------------------------
  # rendering the plots
  output$all <- renderPlot({
    
    input$go
    isolate(graph1 <- perbar(dataset1(),'S3'))
    isolate(graph2 <- perbar(dataset2(),'S4n_kk'))
    isolate(graph3 <- perbar(dataset3(),'S5_kk'))
  
    grid.arrange(graph1,graph2, graph3, nrow = 2, ncol = 2)
    
  })
  
})