# AIM :
# Predict Return of a Stock Portfolio using Monte Carlo Simulation on individual stocks

rm(list = ls())
library(httr)
library(jsonlite)
library(car)

# Generates a path through Geometric Brownian Motion
# Requires : 
# iter : Number of Iterations / Length of Path
# mean : drift
# sigma : volatility
# interval : time interval between discrete steps
# initial : Initial Value of path
GeomBrownian = function(iter, mean, sigma, interval, initial){
  path = vector(length=iter)
  path[1] = initial
  for(i in 2:iter){
    path[i] = path[i-1] * exp(interval*mean + (interval^0.5)*sigma*rnorm(1))
  }
  
  return(path)
}

# Get Expected Path by Monte Carlo Simulation
# Requires :
# iter : Number of Iterations / Length of Path
# npaths : Number of Path to consider
# prices : Price vector
# lratio : log ratio of prices
GetExpectedPath = function(iter, npaths, prices){
  price_len = length(prices)
  interval = 1
  
  # Log ratios
  lratio = vector(length=price_len-1)
  for (i in 1:price_len-1){
    lratio[i] = log(prices[i+1]) - log(prices[i])
  }
  
  # UMVUE's of drift and variance 
  # as each lratio follows N(drift, variance)
  drift = mean(lratio)
  diffusion = sd(lratio)
  
  brownian = matrix(0, nrow = iter, ncol = npaths)
  for (i in 1:npaths){
    brownian[,i] = GeomBrownian(iter, drift, diffusion, interval, prices[price_len])
  }
  
  # Mean of all Paths will estimate the true path as per Monte Carlo Simulation
  exp_path = rowMeans(brownian)
  return(exp_path)
}


# Get Stock Price Data through RapidAPI + Yahoo Finance API 
# Requires :
# symbol : Stock ticker as listed on Yahoo Finance
PriceData = function(symbol){
  url = sprintf("https://apidojo-yahoo-finance-v1.p.rapidapi.com/stock/v3/get-historical-data?symbol=%s&region=IND", symbol)
  
  headers = add_headers(
    "x-rapidapi-key" = RAPIDAPI_KEY, 
    "x-rapidapi-host" = "apidojo-yahoo-finance-v1.p.rapidapi.com"
  )
  response = GET(url, headers)
  
  parsed = fromJSON(content(response, "text", encoding="UTF-8"), simplifyVector = FALSE)
  price_len = length(parsed$prices)

  data = vector(length=252)
  for (i in 1:252){
    # Handle Null Price Data
    if (i > 1 && 
        (is.null(parsed$prices[i][[1]]$close) || 
        is.na(parsed$prices[i][[1]]$close))) {
      data[i] = data[i-1]
      next
    }
    data[i] = as.numeric(parsed$prices[i][[1]]$close)
  }
  return(rev(data))
}


# Get Portfolio
portfolio = read.csv("portfolio.csv", header=TRUE, sep=",")
View(portfolio)
portfolio_size = nrow(portfolio)

stocks = portfolio$Symbol
qty = as.integer(portfolio$Quantity.Available)

# Store Daily Price Data of last 1 year for each stock 
prices = matrix(0, nrow=252, ncol=portfolio_size)

for (i in 1:portfolio_size){
  prices[, i] = PriceData(stocks[i])
}
View(prices)

# Plot last 1 year return of All stocks individually
matplot(prices, main="All Stocks", xlab="Time", ylab="Value", type="l")
legend("topleft", legend = stocks , col = 1:portfolio_size, lty = 1:portfolio_size, cex=0.5)

# Calculate Portfolio Returns
portfolio_returns = vector(length = nrow(prices))
for (i in 1:portfolio_size){
  portfolio_returns = portfolio_returns + qty[i]*prices[, i]
}

# Plot last 1 year value of Portfolio
matplot(portfolio_returns, main="Portfolio", xlab="Time", ylab="Value", type="l")
cat("Current Value : ", portfolio_returns[length(portfolio_returns)])


# Get Nifty Index Price Data
nifty = PriceData("^NSEI")

# Compare Portfolio with Nifty for last 1 year
cmp_matrix = matrix(0, nrow=252, ncol=2)
cmp_matrix[,1] = portfolio_returns*100/portfolio_returns[1]
cmp_matrix[,2] = nifty*100/nifty[1]

matplot(cmp_matrix, main="Portfolio v/s Nifty (Initial Amount : 100)", xlab="Time", ylab="Value", type="l")
legend("topleft", legend = c("Portfolio", "Nifty") , col = 1:2, lty = 1:2, cex=0.75)

cat("If Amount 100 were invested in the Portfolio and Nifty it will be ",
    portfolio_returns[252]*100/portfolio_returns[1], " in Portfolio and ",
    nifty[252]*100/nifty[1], " in Nifty")


## Prediction
# Testing Assumptions of Geometric Brownian Motion : 
# 1. log ratios are normally distributed with constant mean (drift) and 
#     constant variance (diffusion)
# 2. log ratios are independent 

# log ratio matrix
prices.lratio = matrix(0, nrow = nrow(prices) - 1, ncol = ncol(prices))
for (i in 1:nrow(prices.lratio)){
  prices.lratio[i,] = log(prices[i+1,]) - log(prices[i,])
}

par(mfrow = c(2,2))
for (i in 1:length(stocks)){
  temp_plot.name =stocks[i]
  plot(prices.lratio[,i], xlab = temp_plot.name, ylab = "Log Ratio", 
       col = "blue", type = "l")
}
for (i in 1:length(stocks)){
  temp_plot.name =stocks[i]
  hist(prices.lratio[,i], xlab = temp_plot.name, ylab = "Log Ratio", 
       col = "blue")
}
for (i in 1:length(stocks)){
  temp_plot.name =stocks[i]
  qqnorm(y = prices.lratio[,i], pch = 1, frame=FALSE, main = temp_plot.name)
  qqline(prices.lratio[,i], col = "steelblue", lwd = 2)
}
# From the QQ plots it is clear to see log ratios are roughly normal

prices.shapiroW = c()
for (i in 1:length(stocks)){
  prices.shapiroW = append(prices.shapiroW, shapiro.test(prices.lratio[,i])$p.value)
}
prices.shapiroW

# Testing Serial Independence in log ratios 
prices.durbinW = c()
for (i in 1:length(stocks)){
  prices.durbinW = append(prices.durbinW, durbinWatsonTest(prices.lratio[,i]))
}
prices.durbinW
# As all test values are close to 2, we can conclude there is no significant 
# serial correlation in log ratios


# Using Monte Carlo Simulation to get Expected paths of individual stocks
predicted_prices = matrix(0, nrow=20, ncol=portfolio_size)

for (i in 1:portfolio_size){
  predicted_prices[, i] = GetExpectedPath(20, 10, prices[,i])
}
View(predicted_prices)

# Plot Predicted Prices for Individual Stocks
par(mfrow = c(1,1))
matplot(predicted_prices, main="All Stocks prediction", xlab="Time", ylab="Value", type="l")
legend("topleft", legend = stocks , col = 1:portfolio_size, lty = 1:portfolio_size, cex=0.5)

# Calculate Portfolio Returns
portfolio_predicted_returns = vector(length = nrow(predicted_prices))
for (i in 1:portfolio_size){
  portfolio_predicted_returns = portfolio_predicted_returns + qty[i]*predicted_prices[, i]
}

# Plot predicted value of Portfolio
matplot(portfolio_predicted_returns, main="Portfolio prediction", xlab="Time", ylab="Value", type="l")
cat("Predicted Value after 20days : ", portfolio_predicted_returns[20])

# Expected Path for Nifty
predicted_nifty_prices = GetExpectedPath(20, 10, nifty)

# Compare prediction of Portfolio with Nifty
pred_cmp_matrix = matrix(0, nrow=20, ncol=2)
pred_cmp_matrix[,1] = portfolio_predicted_returns*100/portfolio_predicted_returns[1]
pred_cmp_matrix[,2] = predicted_nifty_prices*100/predicted_nifty_prices[1]

matplot(pred_cmp_matrix, main="Predicted Portfolio v/s Nifty (Initial Amount : 100)", 
        xlab="Time", ylab="Value", type="l")
legend("topleft", legend = c("Portfolio", "Nifty") , col = 1:2, lty = 1:2, cex=0.75)

cat("If Amount 100 is invested in the Portfolio and Nifty it will be ",
    portfolio_predicted_returns[20]*100/portfolio_predicted_returns[1], " in Portfolio and ",
    predicted_nifty_prices[20]*100/predicted_nifty_prices[1], " in Nifty in 20days")

