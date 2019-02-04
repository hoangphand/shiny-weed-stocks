library(shiny)

source("dataUtils.R")

symbolData <- getAllSymbols()

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Weed stocks"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    checkboxGroupInput("symbols", "Choose stocks:", 
                       choiceNames = symbolData$symbol, 
                       choiceValues = symbolData$id,
                       selected = c(symbolData[1,1], symbolData[2,1])
    ),
    
    selectInput("duration", "Choose duration:",
                c("Last 1 week" = 7,
                  "Last 2 week" = 14,
                  "Last 3 week" = 21,
                  "Last 1 month" = 30,
                  "Last 2 month" = 60,
                  "Last 3 month" = 90,
                  "Last 6 month" = 180,
                  "Last 1 year" = 365))
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("corr"),
    verbatimTextOutput("value")
  )
)