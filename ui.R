shinyUI(fluidPage(
  
  titlePanel("Audience Report"),
  fluidRow(
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
                          plotOutput("all", width=800,height=700))
         
  )),
  
  fluidRow(
    
    column(3, wellPanel(
         textInput("var1","Variable1","..."),
         textInput("value1","Value1","..."))),
    
    column(3, wellPanel(
          textInput("var2","Variable2","..."),
          textInput("value2","Value2","..."))),
    
    column(3, wellPanel(
      textInput("var3","Variable3","..."),
      textInput("value3","Value3","...")))
  ),
  
  fluidRow(
    column(9, wellPanel(
      verbatimTextOutput("view")))
  )
))