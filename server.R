library(shiny)

source("dataUtils.R")

server <- function(input, output) {
  data <- reactive({
    readSymbolListData(input$symbols, input$duration)
  })
  
  output$corr <- renderPlot({
    validate(
      need(length(input$symbols) >= 2, "Please select at least 2 symbols")
    )
    pairs(data()[, -1])
    # round(cor(data()[,-1], use='pair'), 2)
  })
  
  output$individualSymbols <- renderUI({
    lapply(input$symbols, function(symbol) {
      data <- readSymbolData(symbol, input$duration)
      data$normed_high = scale(data$high)
      data$date <- factor(data$date)
      renderText({ data[1,1]})
      renderPlot({ plot(data$date, data$normed_high) })
    })
  })
  
  # output$value <- renderText({ length(data()[,1]) })
  output$value <- renderText({ data()[4,1] })
  # output$value <- renderText({ data() })
  # output$value <- renderText({ class(data()) })
  # output$value <- renderText({ length(data()[,1]) })
  # output$value <- renderText({ class(data()) })
}
