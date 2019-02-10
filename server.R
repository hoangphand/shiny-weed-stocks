library(shiny)
library(corrplot)

source("dataUtils.R")

duration = NULL

server <- function(input, output, session) {
  data <- reactive({
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
    
    pairs(data()[, -1])
  })
  
  output$corr <- renderPlot({
    validate(
      need(length(input$symbols) >= 2, "Please select at least 2 symbols")
    )
    corMatrix = cor(data()[,-1])
    corrplot(corMatrix, method = "number")
  })

  output$individualSymbols <- renderUI({
    lapply(input$symbols, function(symbol) {
      data <- NULL
      
      if (input$periodSelection == -1) {
        fromDate <- as.character.Date(input$dateRange[1])
        toDate <- as.character.Date(input$dateRange[2])
        data <- readSymbolDataByDateRange(symbol, fromDate, toDate)
      } else {
        data <- readSymbolDataByPeriod(symbol, input$periodSelection)
      }
      
      data$normed_high = scale(data$high)
      data$date <- factor(data$date)

      output <- tagList()

      output[[1]] <- renderText({ data[1, 1] })
      output[[2]] <- renderPlot({ plot(data$date, data$normed_high) })

      output
    })
  })
  
  # output$value <- renderText({ length(data()[,1]) })
  output$value <- renderText({ 
    corMatrix = cor(data()[,-1])
    })
  # output$value <- renderText({ input$dateRange[1] })
  # output$value <- renderText({ as.character.Date(input$dateRange[1]) })
  # output$value <- renderText({ data() })
  # output$value <- renderText({ class(data()) })
  # output$value <- renderText({ length(data()[,1]) })
  # output$value <- renderText({ class(data()) })
}
