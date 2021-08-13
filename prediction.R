# Setup Environment
rm(list = ls())
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


# ADANIENT Data (7 Aug 2019 to 7 Aug 2021)
price_data = read.csv("ADANIENT.csv", header=TRUE, sep=",")

View(head(price_data))
price_len = nrow(price_data)

close = rev(as.numeric(gsub(",", "", price_data$close.)))
dates = rev(as.Date(price_data$Date., "%d-%b-%Y"))

# Plot stock close data
plot(dates, close, 'l', col=3)

# Plot Percentage change
percent_change = vector(length=price_len-1)
for (i in 1:price_len-1){
  percent_change[i] = (close[i] - close[i+1])/close[i+1]
}
percent_change = rev(percent_change)
plot(dates[2:price_len], percent_change*100, 'l', col=3, xlab="Time", ylab="Percent Change")

# Stats of Percentage Change
price_mean = mean(percent_change)
price_var = var(percent_change)
price_std = sd(percent_change)
cat("Mean : ", price_mean, ", Var : ", price_var, ", Std : ", price_std)

# Drift 
drift = price_mean - (0.5*price_var)
cat("Drift : ", drift)

# Prediction of Price for next 20 days
iter = 20
npaths = 10
brownian = matrix(0, nrow = iter, ncol = npaths)
for (i in 1:npaths){
  brownian[,i] = GeomBrownian(iter, drift, price_std, close[price_len])
}

cat("E[W(10)] = ", mean(brownian[10,]), "\n")
cat("E[W(20)] = ", mean(brownian[20,]), "\n")
matplot(brownian, main="ADANIENT +20", xlab="Time", ylab="Path", type="l")


