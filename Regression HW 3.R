# Regression HW 3
# Setting the HW Directory
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')

set.seed(100)
b_zero <- 1
b_one <- 0.5
sigma_squared <- 0.25
n <- 50
N <- 1000
# yi = b_zero + b_one*xi + epsilon(i), 1 <= i <= n

aaa = matrix(0, nrow = 1000, ncol = 3)
for(i in 1:N){
  x_i <- rnorm(n, 0, 1)
  epsilon <- rnorm(n, 0, sqrt(sigma_squared))
  y_i <- b_zero + b_one*x_i + epsilon
  
  out = lm(y_i ~ x_i)
  out$coef
  aaa[i,1:2] <- out$coef
  aaa[i,3] <- ((summary(out)$sigma)^2)
}
histogram_beta_zero_hat <- hist(aaa[,1], xlab = 'Beta_zero_hat', ylab = 'Frequency',
                                xlim = c(0.7, 1.3), ylim = c(0,300),
                                main = 'Histogram of Beta_zero_hat', col = 'blue')
variance_beta_zero_hat <- (sd(aaa[,1])^2)
variance_beta_zero_hat # Sample Variance of beta_zero_hat is 0.005148661.

histogram_beta_one_hat <- hist(aaa[,2], xlab = 'Beta_one_hat', ylab = 'Frequency',
                               xlim = c(0.2, 0.8), ylim = c(0,300),
                               main = 'Histogram of Beta_one_hat', col = 'green')
variance_beta_one_hat <- (sd(aaa[,3])^2)
variance_beta_one_hat # Sample Variance of beta_one_hat is 0.002603959.

histogram_sigma_squared <- hist(aaa[,3], xlab = 'Sigma_squared', ylab = 'Frequency',
                                ylim = c(0,400),
                                main = 'Histogram of Sigma_squared', col = 'red')
