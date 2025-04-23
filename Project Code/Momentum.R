
install.packages("quantmod")
install.packages("zoo")
install.packages("dplyr")
library(quantmod)
library(zoo)
library(dplyr)

#load the data
data <- read.csv("/Users/santi/PycharmProjects/PortfolioConstructionFinalProject/Project\ Code/returns.csv")

#First we look to rank the assets based on momentum. Then we will long the

data <- data[-1, ]
rownames(data) <- data$Date
data <- data[, -1]

library(xts)
returns_xts <- xts(data, order.by = as.Date(rownames(data)))

monthly_ends <- endpoints(returns_xts, on = "months")


momentum_signal <- lapply(monthly_ends, function(end_idx) {
  if (end_idx < 252) return(rep(NA, ncol(returns_xts)))

  window_returns <- returns_xts[(end_idx - 252 + 1):(end_idx - 21), ]
  cum_ret <- apply(window_returns, 2, function(x) {
    if (sum(!is.na(x)) < 150) return(NA)
    prod(1 + x, na.rm = TRUE) - 1
  })
  return(cum_ret)
})

momentum_signal <- do.call(rbind, momentum_signal)
rownames(momentum_signal) <- index(returns_xts)[monthly_ends]



