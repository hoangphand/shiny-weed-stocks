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
  headerPanel("Weed stocks"),
  
  # Sidebar panel for inputs ----
  sidebarLayout( 
    sidebarPanel(
      tags$div(
        class = 'multicol',
        checkboxGroupInput("symbols", "Choose stocks:", 
                           choiceNames = symbolData$symbol, 
                           choiceValues = symbolData$id,
                           selected = c(symbolData[1,1], symbolData[2,1])
                           )
        ),
      tags$label("Choose duration:"),
      selectInput("periodSelection", label = NULL,
                  c("Custom date range" = -1,
                    "Last 1 week" = 7,
                    "Last 2 week" = 14,
                    "Last 3 week" = 21,
                    "Last 1 month" = 30,
                    "Last 2 month" = 60,
                    "Last 3 month" = 90,
                    "Last 6 month" = 180,
                    "Last 1 year" = 365)
                  ),
      conditionalPanel(
        condition = "input.periodSelection == -1",
        dateRangeInput("dateRange", label = NULL, start = Sys.Date() - 365, end = Sys.Date(), 
                       min = NULL, max = Sys.Date(), format = "yyyy-mm-dd", 
                       startview = "month", weekstart = 0, language = "en", 
                       separator = " to ", width = NULL)
      )
    ),
    
    mainPanel(
      plotOutput("corr"),
      
      uiOutput("individualSymbols"),
      
      verbatimTextOutput("value"),
      
      uiOutput("test")
    )
  )
  
)