library(shiny)
library(corrplot)

source("dataUtils.R")
source("plotUtils.R")
source("predictionUtils.R")

duration = NULL

server <- function(input, output, session) {
  destinationFromURL <- reactive({
    destination <- parseQueryString(session$clientData$url_search)$destination
  })
  
  selectedSymbolsFromURL <- reactive({
    rawQuerySymbols <- parseQueryString(session$clientData$url_search)$selectedSymbols
    as.integer(unlist(strsplit(rawQuerySymbols, split=",")))
  })
  
  periodSelectionFromURL <- reactive({
    rawPeriodSelection <- parseQueryString(session$clientData$url_search)$periodSelection
  })
  
  dateRangeFromURL <- reactive({
    fromDate <- parseQueryString(session$clientData$url_search)$fromDate
    toDate <- parseQueryString(session$clientData$url_search)$toDate
    c(fromDate, toDate)
  })
  
  userPredictionFromURL <- reactive({
    rawUserPrediction <- strtoi(parseQueryString(session$clientData$url_search)$userPrediction, base = 0L)
  })
  
  investmentDurationFromURL <- reactive({
    rawNoOfDays <- strtoi(parseQueryString(session$clientData$url_search)$investmentDuration, base = 0L)
  })
  
  investmentAmountFromURL <- reactive({
    rawInitialAmount <- strtoi(parseQueryString(session$clientData$url_search)$investmentAmount, base = 0L)
  })
  
  symbolWeightsFromURL <- reactive({
    rawQuerySymbolWeights <- parseQueryString(session$clientData$url_search)$symbolWeights
    as.integer(unlist(strsplit(rawQuerySymbolWeights, split=",")))
  })
  
  dataOfSelectedSymbols <- reactive({
    if (is.null(periodSelectionFromURL())) {
      dateRange <- dateRangeFromURL()
      fromDate <- as.character.Date(dateRange[1])
      toDate <- as.character.Date(dateRange[2])
      readSymbolListPriceByDateRange(selectedSymbolsFromURL(), fromDate, toDate)
      # readSymbolListDataByDateRange(selectedSymbolsFromURL(), fromDate, toDate)
    } else {
      readSymbolListDataByPeriod(selectedSymbolsFromURL(), periodSelectionFromURL())
    }
  })
  
  output$homePair <- renderPlot({
    if (!is.null(destinationFromURL()) & length(destinationFromURL()) > 0) {
      if (destinationFromURL() == 'home') {
        validate(
          need(length(selectedSymbolsFromURL()) >= 2, "Please select at least 2 symbols")
        )
  
        pairs(dataOfSelectedSymbols()[, -1], pch = 16)
      }
    }
  })

  output$homeCorr <- renderPlot({
    if (!is.null(destinationFromURL()) & length(destinationFromURL()) > 0) {
      if (destinationFromURL() == 'home') {
        validate(
          need(length(selectedSymbolsFromURL()) >= 2, "Please select at least 2 symbols")
        )
        corMatrix = cor(dataOfSelectedSymbols()[,-1], use='pair')
        corrplot(corMatrix, method = "number")
      }
    }
  })

  output$homeIndividualSymbols <- renderPlot({
    if (!is.null(destinationFromURL()) & length(destinationFromURL()) > 0) {
      if (destinationFromURL() == 'home') {
        validate(
          need(length(selectedSymbolsFromURL()) >= 2, "Please select at least 2 symbols")
        )
  
        plotMultilinesSameGraph(dataOfSelectedSymbols())
      }
    }
  })
  
  output$predictionUserInput <- renderPlot({
    if (!is.null(destinationFromURL()) & length(destinationFromURL()) > 0) {
      if (destinationFromURL() == 'prediction') {
        userPrediction = userPredictionFromURL()
        investmentDuration = investmentDurationFromURL()
        investmentAmount = investmentAmountFromURL()
        selectedSymbols = selectedSymbolsFromURL()
        symbolWeights = symbolWeightsFromURL()
        
        portfolioValues = getPortfolioValues(userPrediction, selectedSymbols, 
                                             symbolWeights, investmentDuration, investmentAmount)
        
        portfolioValuesToDisplay = calculatePortfolioValues(portfolioValues, investmentDuration)
        
        output$predictionResult <- renderUI({
          tagList(
            h4(paste("The original investment was $", format(round(investmentAmount), big.mark = ",", 
                      scientific = F), "on", as.character(as.Date(Sys.Date() + investmentDuration)))),
            h4(paste("You have a", sprintf(" %.1f %%", 100 * portfolioValuesToDisplay[4]), 
                     "chance of having less than $", portfolioValuesToDisplay[1])),
            h4(paste("You also have a", sprintf(" %.1f %%", 100 * portfolioValuesToDisplay[4]), 
                         "chance of having more than $", portfolioValuesToDisplay[3])),
            h4(paste("Our best guess is that you will have about $", portfolioValuesToDisplay[2])))
          
        })
        
        plotPortfolioValues(portfolioValues, investmentAmount, investmentDuration)
      }
    }
  })
  
  # output$dataTable <- renderDataTable(dataOfSelectedSymbols())
  # output$table <- renderTable(dataOfSelectedSymbols()[, -1])
  # output$dataTable <- renderDataTable(dateRangeFromURL())
  # output$table <- renderTable(dateRangeFromURL())
  
  output$value <- renderText({
  })
}
