library(shiny)
library(corrplot)

source("dataUtils.R")
source("plotUtils.R")

duration = NULL

server <- function(input, output, session) {
  selectedSymbolsFromURL <- reactive({
    rawQuerySymbols <- parseQueryString(session$clientData$url_search)$selectedSymbols
    unlist(strsplit(rawQuerySymbols, split=","))
  })
  
  periodSelectionFromURL <- reactive({
    rawPeriodSelection <- parseQueryString(session$clientData$url_search)$periodSelection
  })
  
  dateRangeFromURL <- reactive({
    fromDate <- parseQueryString(session$clientData$url_search)$fromDate
    toDate <- parseQueryString(session$clientData$url_search)$toDate
    c(fromDate, toDate)
  })
  
  dataOfSelectedSymbols <- reactive({
    if (is.null(periodSelectionFromURL())) {
      dateRange <- dateRangeFromURL()
      fromDate <- as.character.Date(dateRange[1])
      toDate <- as.character.Date(dateRange[2])
      readSymbolListDataByDateRange(selectedSymbolsFromURL(), fromDate, toDate)
    } else {
      readSymbolListDataByPeriod(selectedSymbolsFromURL(), periodSelectionFromURL())
    }
  })
  
  output$pair <- renderPlot({
    validate(
      need(length(selectedSymbolsFromURL()) >= 2, "Please select at least 2 symbols")
    )
    
    pairs(dataOfSelectedSymbols()[, -1], pch = 16)
  })
  
  output$corr <- renderPlot({
    validate(
      need(length(selectedSymbolsFromURL()) >= 2, "Please select at least 2 symbols")
    )
    corMatrix = cor(dataOfSelectedSymbols()[,-1], use='pair')
    corrplot(corMatrix, method = "number")
  })
  
  output$individualSymbols <- renderPlot({
    validate(
      need(length(selectedSymbolsFromURL()) >= 2, "Please select at least 2 symbols")
    )
    
    plotMultilinesSameGraph(dataOfSelectedSymbols())
  })
  
  output$value <- renderText({
  })
}
