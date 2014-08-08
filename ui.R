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
      selectInput(inputId="var1",
                  label="Variable 1",
                  choices = c("",vari_list)
                  ),
      conditionalPanel("input.var1!=''",uiOutput("valuecontrol1"))
    )),
    
    column(3, wellPanel(
      selectInput(inputId="var2",
                  label="Variable 2",
                  choices = c("",vari_list)
                  ),
      uiOutput("valuecontrol2")
    )),
      
    column(3, wellPanel(
      selectInput(inputId="var3",
                  label="Variable 3",
                  choices = c("",vari_list)
                  ),
      uiOutput("valuecontrol3")
    ))
  ),
  
  fluidRow(
    column(9, wellPanel(
      verbatimTextOutput("view")))
  )
))