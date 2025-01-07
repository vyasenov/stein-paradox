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
risk_mle <- numeric(n)  
risk_js <- numeric(n)

##############
############## SIMULATIONS
##############

for (sim in 1:n) {
  # Simulate observations
  X <- rnorm(p, mean = mu, sd = sigma)

  # MLE estimator
  mle <- X
  risk_mle[sim] <- sum((mle - mu)^2)

  # James-Stein estimator
  shrinkage <- max(0, 1 - ((p - 2) * sigma^2) / sum(X^2))
  js <- shrinkage * X
  risk_js[sim] <- sum((js - mu)^2)
}

##############
############## PRINT RESULTS
##############

cat("Average Risk of MLE:", mean(risk_mle), "\n")
cat("Average Risk of James-Stein:", mean(risk_js), "\n")