library(shiny)
library(car)
library(ggplot2)
library(gridExtra)

# -------------------------------------------------------
# 2. create Server with the function fluidPage
# -------------------------------------------------------
shinyServer(function(input,output){
  
  output$text <- renderText({
    
    form <- reactive({paste("Campaign", input$campID, sep=" ID ")})
    form()
  })
   
#   output$netcheck <- renderUI({
#     
#     checkboxInput(inputId="all_networks",label="All Networks",value=T)
#     
#   })
    
  output$netIDcontrol <- renderUI({
    
    input$go
    isonet <- isolate({     
    selected_camp <- subset(camp_data,campaignID==input$campID)
    netID_choice <- unique(selected_camp$netID_last_imp)
    
    #switch(input$campID,
       
    checkboxGroupInput(inputId="checknetID", 
                         label = h4("Network"), 
                         choices = netID_choice)
    })
    #)
  })
      
    output$weekcheck <- renderUI({
      
      #if (is.null(input$campID))
      #  return()

      switch(input$campID,
             
             checkboxInput(inputId="all_weeks",label="All Weeks",value=T)
      )
    })

    output$weekcontrol <- renderUI({
    
    if (is.null(input$campID))
      return()
    
    selected_camp <- subset(camp_data,campaignID==input$campID)
    week_choice <- levels(ordered(selected_camp$week))
    
    switch(input$campID,
            
         checkboxGroupInput(inputId="checkWeek", 
                            label = h4("Week"),
                            choices = week_choice)
    )
  })

   
  # -------------------------------------------------------
  # creating the function for adjusting dataset
  
  #S3  ------------------------------------------------------------
  dataset1 <- function(){
    
    if(is.null(input$all_networks)){ print("error")}
      
    if(input$all_networks){
      
      data1 <- subset(camp_s3, campaignID==input$campID)
      
        if(input$all_weeks) {
        
        data1 <- subset(camp_s3, campaignID==input$campID)
        
        } else {
        
          data1 <- subset(camp_s3, week %in% c(input$checkWeek) & campaignID==input$campID)
        }
  }
    else{
      
      if(input$all_weeks) {
        
        data1 <- subset(camp_s3,netID_last_imp %in% c(input$checknetID) & campaignID==input$campID)
        
      } else {
        
        data1 <- subset(camp_s3, week %in% c(input$checkWeek) & netID_last_imp %in% c(input$checknetID) & campaignID==input$campID)
      }
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
    

  # -------------------------------------------------------
  # rendering the plots
  output$all <- renderPlot({
    
    input$go
    isolate(  
    graph1 <- perbar(dataset1(),'S3'))
    #graph1 <- ggplot(data= dataset1(),aes_string(x=S3,fill=S3)) + geom_bar()
    
    #graph2 <- perbar(dataset2(),'S4n_kk')
    #graph2 <- qplot(data= dataset2(),x=S4n_kk,fill=S4n_kk) + geom_bar()
      
    grid.arrange(graph1, nrow = 2, ncol = 2)
    
    })
})