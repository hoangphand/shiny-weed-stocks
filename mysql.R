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

# query <- sprintf("SELECT * FROM %s", symbol_table)
# data <- dbGetQuery(db, query)

num_arr <- c(9, 10)
num_arr_str <- paste("(", paste(num_arr, collapse = ","), ")")
# 
query <- sprintf("SELECT symbol, date, high FROM %s 
                 JOIN %s ON %s.symbol_id=%s.id 
                 WHERE symbol_id IN %s", 
                 symbol_daily_table,
                 symbol_table,
                 symbol_daily_table,
                 symbol_table,
                 num_arr_str)

data <- dbGetQuery(db, query)
dbDisconnect(db)
data

select_date <- as.character.Date(Sys.Date() - 7)
data = data[data$date >= select_date,]

symbols <- levels(factor(data$symbol))
df <- list()
count <- 1

for (sym in symbols) {
  df[[sym]] <- data[data$symbol == sym, c('date', 'high')]
  colnames(df[[sym]]) <- c('date', sym)
  count <- count + 1
}
df

merged_data <- Reduce(function(x, y) merge(x, y, all = TRUE, by = 'date'), df)
merged_data
