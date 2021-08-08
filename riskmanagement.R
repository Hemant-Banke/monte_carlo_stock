# Setup Environment
rm(list = ls())
setwd("/media/hyena0/G Vol/rproj/monte_carlo")

# Calculate Return from Risky and Fixed Assets
# Requires : 
# alpha : fractional allocation to risk free asset , range = (0, 1)
# risk_free_rate : Risk free return from Fixed asset in the time period
# risk_mean : Mean return of risky asset in the time period
# risk_sd : volatility of risky asset in the time period
calc_Return = function(alpha, risk_free_rate, risk_mean, risk_sd){
  return(alpha*risk_free_rate + (1 - alpha)*(rnorm(1)*risk_sd + risk_mean))
}

# Generates a return path for a given alpha
# Requires : 
# iter : Number of Iterations / Length of Path
# alpha : fractional allocation to risk free asset , range = (0, 1)
# risk_free_rate : Risk free return from Fixed asset in the time period
# risk_mean : Mean return of risky asset in the time period
# risk_sd : volatility of risky asset in the time period
ReturnPath = function(iter, alpha, risk_free_rate, risk_mean, risk_sd){
  path = vector(length=iter)
  path[1] = 1
  for(i in 2:iter){
    path[i] = path[i-1] * calc_Return(alpha, risk_free_rate, risk_mean, risk_sd)
  }
  
  return(path)
}



# Predicting Allocation for the following case
# Risk Free Asset : An FD giving fixed 5% annually
# Risky Asset : Nifty 50 Index (Mean annual return : 11.30% + 1.18% (Dividend) , Std Deviation : 0.03)
risk_free_rate = 1.05
risk_mean = 1.1248
risk_sd = 0.03

iter = 20
npaths = 20

# Simulating Return for a given alpha
alpha = 0.2

paths = matrix(0, nrow = iter, ncol = npaths)
for (i in 1:npaths){
  paths[,i] = ReturnPath(iter, alpha, risk_free_rate, risk_mean, risk_sd)
}

cat("E[W(10)] = ", mean(paths[10,]), "\n")
cat("E[W(20)] = ", mean(paths[20,]), "\n")
matplot(paths, main=sprintf("Simulation for alpha = %s", alpha), xlab="Time", ylab="Path", type="l")

# Simulating Returns for random alpha
paths_alpha = matrix(0, nrow = iter, ncol = npaths)
for (i in 1:npaths){
  paths_alpha[,i] = ReturnPath(iter, runif(1), risk_free_rate, risk_mean, risk_sd)
}

cat("E[W(10)] = ", mean(paths[10,]), "\n")
cat("E[W(20)] = ", mean(paths[20,]), "\n")
matplot(paths_alpha, main="Simulation for random alpha", xlab="Time", ylab="Path", type="l")


