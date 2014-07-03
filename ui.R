# -------------------------------------------------------
# 1. create a User Interface with the function fluidPage
# -------------------------------------------------------

shinyUI(fluidPage(

  # -----------------------------------------------------
  # 3.1. Title 
  # -----------------------------------------------------
  titlePanel("Generalized AR"),
  
  # -----------------------------------------------------
  fluidRow(
     
    column(3, wellPanel(
      selectInput(inputId="campID",
                  label=h4("Choose a Campaign ID"),
                  choices =unq_campID,
                  selected=7330),
      checkboxInput(inputId="all_networks",label="All Networks",value=T),
      checkboxInput(inputId="all_weeks",label="All Weeks",value=T),
      actionButton("go","Go")      
      
    )),
    
    column(3, wellPanel(
      #uiOutput("netcheck"),
      uiOutput("netIDcontrol")
    )),
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("weekcontrol")
    ))),
  
  mainPanel(  
  h3(textOutput("text")),
  plotOutput(outputId="all",width=800,height=700)
)
))