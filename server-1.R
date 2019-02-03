library(shiny)
library(dplyr)
library(pool)
library(RMySQL)
library(DBI)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = "cuocdoideplam1"
))

databaseName <- "symbol_prices"
symbol_table <- "symbol"
symbol_daily_table <- "symbol_daily"
symbol_five_mins_table <- "symbol_five_mins"

db <- dbConnect(MySQL(), dbname = databaseName, 
                host = options()$mysql$host, 
                port = options()$mysql$port, 
                user = options()$mysql$user, 
                password = options()$mysql$password
)

query <- sprintf("SELECT * FROM %s", symbol_table)

# Submit the fetch query and disconnect
symbol_data <- dbGetQuery(db, query)
# dbDisconnect(db)

read_symbol_data <- function(symbol_ids, duration) {
  symbol_ids_arr_str <- paste("(", paste(symbol_ids, collapse = ","), ")")
  select_date = Sys.Date() - duration
  
  query <- sprintf("SELECT symbol, date, high FROM %s JOIN %s ON %s.symbol_id=%s.id WHERE symbol_id IN %s",
                   symbol_daily_table,
                   symbol_table,
                   symbol_daily_table,
                   symbol_table,
                   symbol_ids_arr_str)
  
  list_of_symbol_df = list()
  
  data <- dbGetQuery(db, query)
  data = data[data$date >= select_date]
  
  symbols <- levels(factor(data$symbol))
  
  count <= 1
  for (sym in symbols) {
    list_of_symbol_df[[sym]] <- data[data$symbol == sym, c('date', 'high')]
    colnames(list_of_symbol_df[[sym]]) <- c('date', sym)
    count <- count + 1
  }
  
  merged_data <- NULL
  
  for (sym in symbols) {
    merged_data <- merge(list_of_symbol_df[[sym]], merged_data, all = TRUE, by = 'date')
  }
  
  merged_data
}

server <- function(input, output) {
  data <- reactive({ 
    read_symbol_data(input$symbols, input$duration)
  })
  # symbol_ids <- input$symbols
  # symbol_ids_arr_str <- paste("(", paste(symbols, collapse = ","), ")")
  # duration <- input$duration
  # select_date = Sys.Date() - duration
  # 
  # query <- sprintf("SELECT symbol, date, high FROM %s JOIN %s ON %s.symbol_id=%s.id WHERE symbol_id IN %s", 
  #                  symbol_daily_table,
  #                  symbol_table,
  #                  symbol_daily_table,
  #                  symbol_table,
  #                  symbol_ids_arr_str)
  # 
  # list_of_symbol_df = list()
  # 
  # data <- dbGetQuery(db, query)
  # data = data[data$date >= select_date]
  # 
  # symbols <- levels(factor(data$symbol))
  # 
  # count <= 1
  # for (sym in symbols) {
  #   list_of_symbol_df[[sym]] <- data[data$symbol == sym, c('date', 'high')]
  #   colnames(list_of_symbol_df[[sym]]) <- c('date', sym)
  #   count <- count + 1
  # }
  # 
  # merged_data <- NULL
  # 
  # for (sym in symbols) {
  #   merged_data <- merge(list_of_symbol_df[[sym]], merged_data, all = TRUE, by = 'date')
  # }
  
  # output$corr <- round(cor(merged_data[,-1], use='pair'), 2)
  output$value <- renderText({ input$symbols })
}
