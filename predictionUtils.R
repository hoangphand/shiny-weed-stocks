source('dataUtils.R')

readSymbolData3Months <- function(symbolIds) {
  data <- readSymbolListDataByPeriod(symbolIds, 90)
}

readSymbolCorrelationsLast3Months <- function(symbolIds) {
  if (length(symbolIds) > 0) {
    symbolIdsArrStr <- paste("(", paste(symbolIds, collapse = ","), ")")
    
    query <- sprintf("SELECT symbol1, symbol2, corr 
                     FROM %s  
                     WHERE symbol1 IN %s AND symbol2 IN %s",
                     symbolCorrelationsTable,
                     symbolIdsArrStr,
                     symbolIdsArrStr)
    db <- getDBConnect()
    
    data <- dbGetQuery(db, query)
    
    closeDBConnect(db)
    
    data
  }
}

getFractionalChangeForEachDay = function(x) {
  return(x[-1] / x[-length(x)])
}

calculateDataFractionalChanges <- function(data) {
  fractionallyTransformedData = data[-nrow(data), -1] # Take out the last observation and the date
  noOfRowsOfInputData = ncol(data)
  for(i in 2: noOfRowsOfInputData){
    fractionalChangeCurrDay = getFractionalChangeForEachDay(data[, i])
    fractionallyTransformedData[, i - 1] = fractionalChangeCurrDay;
    ###### Fill up for missing data
    fractionalChangeCurrDay = fractionalChangeCurrDay[is.finite(fractionalChangeCurrDay)]
    vecOfInvalidData = !is.finite(fractionallyTransformedData[, i - 1]) | fractionallyTransformedData[, i - 1] == 0
    fractionallyTransformedData[vecOfInvalidData, i - 1] = sample(fractionalChangeCurrDay, sum(vecOfInvalidData), replace = T)
  }
  
  fractionallyTransformedData
}

findHeavyHitters <- function(rawData, correlations) {
  # Find the HH
  # Get the mean volume for each of our chosen stocks
  meanVolume = tapply(rawData$volume, rawData$symbol, mean)
  
  # Get the product of the correlations for each of our chosen stocks
  productOfCorrelations = tapply(correlations$corr, correlations$symbol1, function(x){
    tmp = cumprod(abs(x))
    return(tmp[length(tmp)])
  })
  # sort data in the alphabet order of symbol names
  productOfCorrelations = productOfCorrelations[order(names(productOfCorrelations))]
  meanVolume = meanVolume[order(names(meanVolume))]
  # calculate production of meanVolume and product correlations
  tmpHeavyHitters = meanVolume * productOfCorrelations
  # Find the HH
  HH = tmpHeavyHitters[which(tmpHeavyHitters == max(tmpHeavyHitters))]
}

getPortfolioValues = function(userPrediction, symbolIds, symbolWeights, noOfDays, initialAmount, 
                              noOfRuns = 10, userConfidence = 9, historyConfidence = 6, tendency = 9) {
  # Rescale
  userPrediction = userPrediction / 100
  symbolWeights = initialAmount * symbolWeights / sum(symbolWeights)
  
  # Prepare data
  correlations = readSymbolCorrelationsLast3Months(symbolIds)
  rawData = readSymbolData3Months(symbolIds)
  priceData = extractSymbolPriceFromData(rawData)
  HH = findHeavyHitters(rawData, correlations)
  fractionallyTransformedData = calculateDataFractionalChanges(priceData)
  
  # Get constants from our matrix fractionallyTransformedData
  declineVector = fractionallyTransformedData[, colnames(fractionallyTransformedData) == names(HH)] < 1
  daysOfReturns = nrow(fractionallyTransformedData)
  noOfDeclineDaysOverN = sum(fractionallyTransformedData[, colnames(fractionallyTransformedData) == names(HH)] < 1)
  # Parameters of beta distribution
  alpha = historyConfidence * (daysOfReturns - noOfDeclineDaysOverN) / daysOfReturns + userConfidence * userPrediction + tendency + 1
  beta = historyConfidence * noOfDeclineDaysOverN / daysOfReturns + userConfidence * (1 - userPrediction) + tendency
  # We'll generate 100 different time frames for each user. Choose 100 values of p for binomial distributions
  # noOfRuns=10
  probabilities = rbeta(noOfRuns, alpha, beta)
  portfolioValue = matrix(NA, nrow = noOfDays, ncol = noOfRuns)
  for(i in 1:noOfRuns) {
    advances = rbinom(1, noOfDays, probabilities[i])
    # Choose this many of the advancing noOfDays
    advancingDays = sample(which(!declineVector), advances, replace = T)
    decliningDays=sample(which(declineVector), noOfDays - advances, replace = T)
    currentSample = sample(c(advancingDays, decliningDays))
    currentFractionallyTransformedData = fractionallyTransformedData[currentSample, ]
    currentFractionallyTransformedData = cumprod(currentFractionallyTransformedData)
    for(j in 1:ncol(currentFractionallyTransformedData)){
      currentFractionallyTransformedData[, j] = symbolWeights[j] * currentFractionallyTransformedData[, j]
    }
    portfolioValue[, i] = round(apply(currentFractionallyTransformedData, 1, sum))
  }
  portfolioValue
}

calculatePortfolioValues = function(portfolioValue, noOfDays, noOfRuns = 10) {
  medianAtDay = round(apply(portfolioValue, 1, median))
  last.pv = sort(portfolioValue[noOfDays, ])
  alpha = 0.10
  lowerIndex = max(1, floor(alpha * noOfRuns))
  upperIndex = min(noOfRuns, ceiling((1 - alpha) * noOfRuns))
  
  c(last.pv[lowerIndex], medianAtDay[noOfDays], last.pv[upperIndex], alpha)
}

plotPortfolioValues = function(portfolioValue, initialAmount, noOfDays, noOfRuns = 10) {
  medianAtDay = round(apply(portfolioValue, 1, median))
  xdays = seq(Sys.Date(), Sys.Date() + noOfDays - 1, by = 1)
  par(mar = c(3, 3, 1, 1))
  plot(xdays, medianAtDay, lwd = 2, col = "skyblue", yaxt = 'n')
  abline(h = initialAmount, lwd = 2, col = 'seagreen')
  tmp = pretty(medianAtDay)
  axis(2, at = tmp, labels = format(tmp, big.mark = ",", scientific = FALSE))
  for(i in 1:noOfRuns) lines(xdays, portfolioValue[, i], col = "grey")
  lines(xdays, medianAtDay, lwd = 4, col = "skyblue")
}