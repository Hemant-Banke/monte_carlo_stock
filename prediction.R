# Setup Environment
rm(list = ls())
setwd("/media/hyena0/G Vol/rproj/monte_carlo")

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


# ADANIENT Data (7 Aug 2019 to 7 Aug 2021)
price_data = read.csv("ADANIENT.csv", header=TRUE, sep=",")

View(head(price_data))
price_len = nrow(price_data)

close = rev(as.numeric(gsub(",", "", price_data$close.)))
dates = rev(as.Date(price_data$Date., "%d-%b-%Y"))

# Plot stock close data
plot(dates, close, 'l', col=3, main = "ADANIENT")


## Prediction
# log ratios
prices.lratio = vector(length = price_len-1)
for (i in 1:price_len-1){
  prices.lratio[i] = log(close[i+1]) - log(close[i])
}

plot(prices.lratio, xlab = "Closing Price", ylab = "Log Ratio", 
     col = "blue", type = "l")

hist(prices.lratio, xlab = "Closing Price", ylab = "Log Ratio", 
     col = "blue")

qqnorm(y = prices.lratio, pch = 1, frame=FALSE)
qqline(prices.lratio, col = "steelblue", lwd = 2)
# From the QQ plots it is clear to see log ratios are roughly normal

shapiro.test(prices.lratio)
durbinWatsonTest(prices.lratio)
# As test value is close to 2, we can conclude there is no significant 
# serial correlation in log ratios

# UMVUE's of drift and variance 
# as each lratio follows N(drift, variance)
drift = mean(prices.lratio)
diffusion = sd(prices.lratio)


# Prediction of Price for next 20 days
interval = 1
iter = 20
npaths = 10
brownian = matrix(0, nrow = iter, ncol = npaths)
for (i in 1:npaths){
  brownian[,i] = GeomBrownian(iter, drift, diffusion, interval, close[price_len])
}

cat("E[W(10)] = ", mean(brownian[10,]), "\n")
cat("E[W(20)] = ", mean(brownian[20,]), "\n")
matplot(brownian, main="ADANIENT +20", xlab="Time", ylab="Path", type="l")

# Expected Path
exp_path = rowMeans(brownian)
matplot(exp_path, main="ADANIENT +20 Expected", xlab="Time", ylab="Path", type="l")

