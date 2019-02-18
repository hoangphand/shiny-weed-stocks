plotMultilinesSameGraph <- function(data) {
  # Plot all four of these on the same plot. Normalize and look at percent changes 
  ncol.now = ncol(data)
  data[, (ncol.now + 1):(2 * ncol.now - 1)] = NA
  # For each column of stock prices, use the first non-missing price to normalize
  # the other prices
  scale.by.first.not.zero = function(x) {
    min.not.missing = min(which(!(is.na(x) | x == 0)))
    first.non.missing = x[min.not.missing]
    return(100 * (x - first.non.missing) / first.non.missing)
  }
  
  for(i in (ncol.now + 1):(2 * ncol.now - 1)) data[, i] = scale.by.first.not.zero(data[, i - ncol.now + 1])
  
  colnames(data)[(ncol.now + 1):(2 * ncol.now - 1)] = paste(colnames(data)[2:ncol.now], ".2", sep='')
  min.y = min(data[, (ncol.now + 1):(2 * ncol.now - 1)], na.rm = T)
  max.y = max(data[, (ncol.now + 1):(2 * ncol.now - 1)], na.rm = T)
  par(mar = c(5, 5, 2, 1))
  plot(factor(data$date), data[, ncol.now + 1], pch = 16, col = 1, cex = 1.6, xlab = "Date",
       ylab = "% Change", main = "", ylim = c(min.y, max.y))
  lines(factor(data$date), data[, ncol.now + 1], col = 1, lwd = 2)
  for(i in (ncol.now + 2):(2 * ncol.now - 1)) {
    points(factor(data$date), data[, i], pch=16, col = i - ncol.now)
    lines(factor(data$date), data[, i], lwd = 2,col = i - ncol.now)
  }
  # legend(data[round(0.8 * nrow(data)), 1], 0.1 * min.y + 0.9 * max.y, colnames(data)[2:ncol.now],
  #        pch = 16, col = 1:ncol.now, bg = "ivory2", cex = 1.)
  legend("topright", colnames(data)[2:ncol.now],
         pch = 16, col = 1:ncol.now, bg = "ivory2", cex = 1.)
}