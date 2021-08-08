# Setup Environment
rm(list = ls())
setwd("/media/hyena0/G Vol/rproj/monte_carlo")

# Generate Uniform(0,1) Random Variables using Linear Congruential Generators
# Requires : 
# iter : Number of Iterations
# x0 : seed
# a, c, m : constants for LCG
LCG = function(iter, x0, a, d, m){
  u<-vector(length=iter)
  u[1]<-x0/m
  for (i in 1:iter)
  {
    x1<-(a*x0)%%m
    x0<-x1
    u[i+1]<-x1/m		
  }
  return(u)
}

# Generates Normal Random Variables
# Requires : 
# u1, u2 : Independent Uniform(0, 1) Random Variables with length len (can be fed by LCG)
# len : Length of Resultant Vector
# mean, variance of resultant vector
Normal = function(u1, u2, len, mean, variance){
  r = -2*log(u1)
  th = 2*pi*u2
  z1 = sqrt(r)*cos(th)
  z2 = sqrt(r)*sin(th)
  z = c(z1, z2)
  
  z = z*variance + mean
  return(z)
}

# Generates a path through Standard Brownian Motion (drift: 0, volatility: 1, initial : 0)
# Requires : 
# iter : Number of Iterations / Length of Path
StdBrownian = function(iter){
  interval = 5/iter
  
  path = vector(length=iter+1)
  path[1] = 0
  for(i in 2:(iter+1)){
    path[i] = path[i-1] + (interval^0.5)*rnorm(1)
  }
  
  return(path)
}

# Generates a path through Geometric Brownian Motion
# Requires : 
# iter : Number of Iterations / Length of Path
# mean : drift
# sigma : volatility
# initial : Initial Value of path
GeomBrownian = function(iter, mean, sigma, initial){
  interval = 5/iter
  
  path = vector(length=iter)
  path[1] = initial
  for(i in 2:iter){
    path[i] = path[i-1] * exp(interval*(mean - (sigma^2)/2) + (interval^0.5)*sigma*rnorm(1))
  }
  
  return(path)
}

iter = 5000
# u1 = LCG(iter/2, 1, 16807, 0, 2^31-1)
# u2 = LCG(iter/2, 200, 16807, 0, 2^31-1)
# z = Normal(u1, u2, iter, 0, 1)

npaths = 10
brownian = matrix(0, nrow = iter, ncol = npaths)
for (i in 1:npaths){
  brownian[,i] = GeomBrownian(iter, 0.06, 0.3, 100)
}

cat("E[W(2)] = ", mean(brownian[2000,]), "\n")
cat("E[W(5)] = ", mean(brownian[5000,]), "\n")
matplot(brownian, main="Geometric Brownian", xlab="Time", ylab="Path", type="l")

