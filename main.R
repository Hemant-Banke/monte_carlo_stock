# AIM :
# Predict Return of a Stock Portfolio using Monte Carlo Simulation on individual stocks

# Libraries (httr)
install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

# Setup Environment
setwd("/media/hyena0/G Vol/rproj/monte_carlo")


# Generates a path through Geometric Brownian Motion
# Requires : 
# iter : Number of Iterations / Length of Path
# mean : drift
# sigma : volatility
# initial : Initial Value of path
GeomBrownian = function(iter, mean, sigma, initial){
  interval = 1
  
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
GetExpectedPath = function(iter, npaths, prices){
  price_len = length(prices)
  # Percent Change
  percent_change = vector(length=price_len-1)
  for (i in 1:price_len-1){
    percent_change[i] = (prices[i+1] - prices[i])/prices[i]
  }
  
  # Stats of Price
  price_mean = mean(percent_change)
  price_var = var(percent_change)
  price_std = sd(percent_change)
  drift = price_mean - (0.5*price_var)
  
  brownian = matrix(0, nrow = iter, ncol = npaths)
  for (i in 1:npaths){
    brownian[,i] = GeomBrownian(iter, drift, price_std, prices[price_len])
  }
  
  # Geometric Mean of all Paths
  exp_path <- apply(brownian, 1, function(x) (prod(x[x!=0]))^(1/sum(x!=0)))
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
    if (i > 0 && 
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
portfolio_size = length(portfolio)

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


# Using Monte Carlo Simulation to get Expected paths of individual stocks
predicted_prices = matrix(0, nrow=20, ncol=portfolio_size)

for (i in 1:portfolio_size){
  predicted_prices[, i] = GetExpectedPath(20, 10, prices[,i])
}
View(predicted_prices)

# Plot Predicted Prices for Individual Stocks
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

