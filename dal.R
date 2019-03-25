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
symbolTable <- "Symbol"
symbolDailyTable <- "Symbol_daily"
symbolFiveMinsTable <- "Symbol_five_mins"
symbolCorrelationsTable <- "3_months_correlation"

getDBConnect <- function() {
  db = dbConnect(MySQL(), dbname = databaseName, 
                 host = options()$mysql$host, 
                 port = options()$mysql$port, 
                 user = options()$mysql$user, 
                 password = options()$mysql$password)
  db
}

closeDBConnect <- function(db) {
  dbDisconnect(db)
}