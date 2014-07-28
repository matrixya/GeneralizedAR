shinyUI(fluidPage(
  
  titlePanel("Audience Report"),
  
  column(3, wellPanel(
    
    actionButton(inputId="go",label="Update"),
    
    selectInput(inputId="countries",
                label=h5("Country"),
                choices =unq_country,
                selected="dummy"),
            
    conditionalPanel("input.countries!='dummy'",
                     
    uiOutput("campIDcontrol"),
  
    uiOutput("netIDcontrol"),
    uiOutput("weekcontrol")
  )
    
  )),
  
  column(7,
         "When selecting countries, please make sure to have checked *all* for both network and week.",
         
         conditionalPanel("input.countries!='dummy'",
         plotOutput("all", width=800,height=700)
         ))
  
))