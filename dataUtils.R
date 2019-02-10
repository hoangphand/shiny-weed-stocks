library(dplyr)
library(pool)
library(RMySQL)
library(DBI)

source("dal.R")

getAllSymbols <- function() {
  db = getDBConnect()
  
  query <- sprintf("SELECT * FROM %s", symbolTable)
  symbolData <- dbGetQuery(db, query)
  
  closeDBConnect(db)
  
  symbolData
}

readSymbolDataByPeriod <- function(symbolId, duration) {
  selectDate = as.character.Date(Sys.Date() - as.numeric(duration))
  
  query <- sprintf("SELECT symbol, date, high 
                   FROM %s JOIN %s 
                   ON %s.symbol_id=%s.id 
                   WHERE symbol_id = %s",
                   symbolDailyTable,
                   symbolTable,
                   symbolDailyTable,
                   symbolTable,
                   symbolId)
  
  db <- getDBConnect()
  
  data <- dbGetQuery(db, query)
  
  closeDBConnect(db)
  
  data = data[data$date >= selectDate,]
}

readSymbolDataByDateRange <- function(symbolId, fromDate, toDate) {
  # selectDate = as.character.Date(Sys.Date() - as.numeric(duration))
  
  query <- sprintf("SELECT symbol, date, high 
                   FROM %s JOIN %s 
                   ON %s.symbol_id=%s.id 
                   WHERE symbol_id = %s",
                   symbolDailyTable,
                   symbolTable,
                   symbolDailyTable,
                   symbolTable,
                   symbolId)
  
  db <- getDBConnect()
  
  data <- dbGetQuery(db, query)
  
  closeDBConnect(db)
  
  data = data[data$date >= fromDate & data$date <= toDate,]
}

readSymbolListDataByPeriod <- function(symbolIds, duration) {
  if (length(symbolIds) > 0) {
    symbolIdsArrStr <- paste("(", paste(symbolIds, collapse = ","), ")")
    
    selectDate = as.character.Date(Sys.Date() - as.numeric(duration))
    
    query <- sprintf("SELECT symbol, date, high 
                     FROM %s JOIN %s 
                     ON %s.symbol_id=%s.id 
                     WHERE symbol_id IN %s",
                     symbolDailyTable,
                     symbolTable,
                     symbolDailyTable,
                     symbolTable,
                     symbolIdsArrStr)
    
    listOfSymbolDF = list()
    
    db <- getDBConnect()
    
    data <- dbGetQuery(db, query)
    
    closeDBConnect(db)
    
    data = data[data$date >= selectDate,]
    
    symbols <- levels(factor(data$symbol))
    
    for (symbol in symbols) {
      listOfSymbolDF[[symbol]] <- data[data$symbol == symbol, c('date', 'high')]
      colnames(listOfSymbolDF[[symbol]]) <- c('date', symbol)
    }
    
    mergedData <- Reduce(function(x, y) merge(x, y, all = TRUE, by = 'date'), listOfSymbolDF)
    
    mergedData
  }
  
}

readSymbolListDataByDateRange <- function(symbolIds, fromDate, toDate) {
  if (length(symbolIds) > 0) {
    symbolIdsArrStr <- paste("(", paste(symbolIds, collapse = ","), ")")
    
    query <- sprintf("SELECT symbol, date, high 
                     FROM %s JOIN %s 
                     ON %s.symbol_id=%s.id 
                     WHERE symbol_id IN %s",
                     symbolDailyTable,
                     symbolTable,
                     symbolDailyTable,
                     symbolTable,
                     symbolIdsArrStr)
    
    listOfSymbolDF = list()
    
    db <- getDBConnect()
    
    data <- dbGetQuery(db, query)
    
    closeDBConnect(db)
    
    data = data[data$date >= fromDate & data$date <= toDate,]
    
    symbols <- levels(factor(data$symbol))
    
    for (symbol in symbols) {
      listOfSymbolDF[[symbol]] <- data[data$symbol == symbol, c('date', 'high')]
      colnames(listOfSymbolDF[[symbol]]) <- c('date', symbol)
    }
    
    mergedData <- Reduce(function(x, y) merge(x, y, all = TRUE, by = 'date'), listOfSymbolDF)
    
    mergedData
  }
  
}