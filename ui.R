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
      conditionalPanel(
        condition = "new URLSearchParams(window.location.search).get('destination') == 'home'",
        plotOutput("homePair")
      ),
      conditionalPanel(
        condition = "new URLSearchParams(window.location.search).get('destination') == 'home'",
        plotOutput("homeCorr")
      ),
      conditionalPanel(
        condition = "new URLSearchParams(window.location.search).get('destination') == 'home'",
        plotOutput("homeIndividualSymbols")
      ),

      verbatimTextOutput("value"),
      
      # dataTableOutput('dataTable'),
      # tableOutput('table'),
      
      conditionalPanel(
        condition = "new URLSearchParams(window.location.search).get('destination') == 'prediction'",
        uiOutput("predictionResult")
      ),
      
      conditionalPanel(
        condition = "new URLSearchParams(window.location.search).get('destination') == 'prediction'",
        plotOutput("predictionUserInput")
      ),
      
      width = 12)
  )
  
)
