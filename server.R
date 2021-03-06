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
    
    selected_camp <- filter(camp_data, country==input$countries, campaignID==input$campID)
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
    
    selected_camp <- filter(camp_data, country==input$countries,campaignID==input$campID)
    week_choice <- levels(ordered(selected_camp$week))
    week_choice <- c("all",week_choice)
    
    checkboxGroupInput(inputId="checkWeek", 
                              label = h5("Week"), 
                              choices = week_choice,
                              selected = "all")    
  })
  
  
  ##----------------------------------------------------------------------------##
 
  ## creating non null conditions
  
    not_null <- function() {
      cond1 <- ifelse(is.null(input$countries),0,1)
      cond2 <- ifelse(is.null(input$campID),0,1)
      cond3 <- ifelse(is.null(input$checknetID),0,1)
      cond4 <- ifelse(is.null(input$checkWeek),0,1)
      test <- cond1 * cond2 * cond3 * cond4
      test
    }
  
  # create a function acting according to condition of interactive inputs
  
  dataset <- function(y){
    
    if(not_null() != 0){
      
      if(any(input$checknetID=="all")){
        
        if(any(input$checkWeek=="all")){
          x <- subset(y, country==input$countries & campaignID==input$campID)
          
        }else{
          x <- subset(y, country==input$countries & campaignID==input$campID & week %in% c(input$checkWeek))
          
        }
                    
        
      } else{
        if(any(input$checkWeek=="all")){
          x <- subset(y, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID))
          
        }else{
          x <- subset(y, country==input$countries & campaignID==input$campID & netID_last_imp %in% c(input$checknetID) & week %in% c(input$checkWeek))
       
        }
            }
      
    }else{
          x <- y
    }
    
    x[!duplicated(x$userID),]
  }

  ##----------------------------------------------------------------------------##
  # reactive campaign dataset of each varible
    dataset1 <- reactive({dataset(camp_s3)})
    dataset2 <- reactive({dataset(camp_s4n)})
    dataset3 <- reactive({dataset(camp_s5)})
    dataset4 <- reactive({dataset(camp_s13)})
  
    dataset5 <- reactive({dataset(camp_data)})
  

  ##----------------------------------------------------------------------------##
  # control of variable drop-down list
  
  output$valuecontrol1 <- renderUI({
    
    if(nchar(input$var1)<1){
      return()
      
    }else{
   
    value_choice <- unique(camp_data[,input$var1])
    value_choice <- sort(value_choice)
    
    checkboxGroupInput(inputId="value1",
                        label="Values",
                        choices =setdiff(value_choice,'NA'))
    }
  })
    
  output$valuecontrol2 <- renderUI({
    
    if(nchar(input$var2)<1){
      return()
      
    }else{
      
    value_choice <- unique(camp_data[,input$var2])
    value_choice <- sort(value_choice)
    
    checkboxGroupInput(inputId="value2",
                       label="Values",
                       choices =setdiff(value_choice,'NA')
    )
    }  
  })
  
  output$valuecontrol3 <- renderUI({
    
    if(nchar(input$var3)<1){
      return()
      
    }else{
    
    value_choice <- unique(camp_data[,input$var3])
    value_choice <- sort(value_choice)
    
    checkboxGroupInput(inputId="value3",
                       label="Values",
                       choices =setdiff(value_choice,'NA')
    )
    }
  })  
  
  
  ##----------------------------------------------------------------------------##
  # target group as a combination of variables
  combi <- function(){
      
    camp <- dataset5()
      
      if(nchar(input$var1)>1 & !is.null(input$value1)){
        filtered <- subset(camp_data,(!is.na(input$var1)))
        
        #unselected <- setdiff(unique(filtered[,input$var1]),c(input$value1))
        
        
      }
    
      
#           
#       camp$v2 <- reg_ex(input$var2,camp$answers)
#       value2 <- input$value2
#       
#       #camp$v3 <- reg_ex(input$var3,camp$answers)
#   
#       filtered1 <- subset(camp_data,(!is.na(input$var1) & input$var1!=0))
#       filtered2 <- subset(filtered1,(!is.na(input$var2) & input$var2!=0))
#       #filtered3 <- subset(filtered2,(!is.na(filtered2$v3) & filtered2$v3!=0))
   }
    
  ##----------------------------------------------------------------------------##
  
  output$view <- renderPrint({
   
    fil <- combi()
#     selected <- input$value1
#     unselected <- setdiff(uni,selected)
#     
#     print(uni)
#     print(selected)
#     print(unselected)
     #print(fil)
    table(fil$target)

#       if(input$var1==Choose & input$var2==''){
#          print("Please choose variables")
#        }else if(input$var1!="" & input$var2==""){
#         
#          Variable1<-fil[,input$var1]
#          xtabs(~Variable1) 
#        }
#       else{
#         Variable1<-fil[,input$var1]
#         Variable2<-fil[,input$var2]
#           xtabs(~Variable1+Variable2)
#        }
  })
  
  ##----------------------------------------------------------------------------##
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
  # rendering the plots
  output$all <- renderPlot({
    
    input$go
    isolate(graph1 <- perbar(dataset1(),'S3'))
    isolate(graph2 <- perbar(dataset2(),'S4n_kk'))
    isolate(graph3 <- perbar(dataset3(),'S5_kk'))
    isolate(graph4 <- perbar(dataset4(),'S13_kk'))
  
    grid.arrange(graph1,graph2, graph3, graph4, nrow = 2, ncol = 2)
  })
  
})