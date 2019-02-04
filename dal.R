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
symbolTable <- "symbol"
symbolDailyTable <- "symbol_daily"
symbolFiveMinsTable <- "symbol_five_mins"

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