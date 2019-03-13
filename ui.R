library(shiny)

source("dataUtils.R")

symbolData <- getAllSymbols()

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  
  tags$head(tags$style(HTML("
                            .multicol .shiny-options-group{
                            -webkit-column-count: 2; /* Chrome, Safari, Opera */
                            -moz-column-count: 2;    /* Firefox */
                            column-count: 2;
                            -moz-column-fill: balanced;
                            -column-fill: balanced;
                            }
                            "))),
  # App title ----
  headerPanel <- NULL,
  
  # Sidebar panel for inputs ----
  sidebarLayout( 
    sidebarPanel <- NULL,
    
    mainPanel(
      plotOutput("pair"),
      
      plotOutput("corr"),
      
      plotOutput("individualSymbols"),
      
      verbatimTextOutput("value"),
      
      tableOutput('table'),
      
      uiOutput("test")
    )
  )
  
)
