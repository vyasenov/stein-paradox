# Part of this code was produced by chatGPT in January 2024.

rm(list=ls())
set.seed(1988)

##############
############## SIMULATION PARAMETERS
##############

p <- 5  # Number of means
n <- 1000  # Number of simulations
sigma <- 1
mu <- rnorm(p, mean = 5, sd = 2)  # True means

# results storage
mse_mle <- numeric(n)  
mse_js <- numeric(n)

##############
############## SIMULATIONS
##############

for (sim in 1:n) {
  # Simulate observations
  X <- rnorm(p, mean = mu, sd = sigma)

  # MLE estimator
  mle <- X
  mse_mle[sim] <- sum((mle - mu)^2)

  # James-Stein estimator
  shrinkage <- max(0, 1 - ((p - 2) * sigma^2) / sum(X^2))
  js <- shrinkage * X
  mse_js[sim] <- sum((js - mu)^2)
}

##############
############## PRINT RESULTS
##############

cat("Average mse of MLE:", mean(mse_mle), "\n")
cat("Average mse of James-Stein:", mean(mse_js), "\n")