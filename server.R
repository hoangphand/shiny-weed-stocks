library(shiny)
library(corrplot)

source("dataUtils.R")
source("plotUtils.R")

duration = NULL

server <- function(input, output, session) {
  dataOfSelectedSymbols <- reactive({
    if (input$periodSelection == -1) {
      fromDate <- as.character.Date(input$dateRange[1])
      toDate <- as.character.Date(input$dateRange[2])
      readSymbolListDataByDateRange(input$symbols, fromDate, toDate)
    } else {
      readSymbolListDataByPeriod(input$symbols, input$periodSelection)
    }
  })
  
  output$pair <- renderPlot({
    validate(
      need(length(input$symbols) >= 2, "Please select at least 2 symbols")
    )
    
    pairs(dataOfSelectedSymbols()[, -1], pch = 16)
  })
  
  output$corr <- renderPlot({
    validate(
      need(length(input$symbols) >= 2, "Please select at least 2 symbols")
    )
    corMatrix = cor(dataOfSelectedSymbols()[,-1], use='pair')
    corrplot(corMatrix, method = "number")
  })
  
  output$individualSymbols <- renderPlot({
    validate(
      need(length(input$symbols) >= 2, "Please select at least 2 symbols")
    )
    
    plotMultilinesSameGraph(dataOfSelectedSymbols())
  })
  
  # output$value <- renderText({ length(data()[,1]) })
  # output$value <- renderText({ 
  #   cat(dataOfSelectedSymbols())
  # })
  # output$value <- renderText({ input$dateRange[1] })
  # output$value <- renderText({ as.character.Date(input$dateRange[1]) })
  # output$value <- renderText({ data() })
  # output$value <- renderText({ class(data()) })
  # output$value <- renderText({ length(data()[,1]) })
  # output$value <- renderText({ class(data()) })
}
