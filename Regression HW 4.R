# Regression HW 4
# Setting the HW Directory
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
install.packages("faraway")
library(faraway)
data(prostate)
head(prostate)

# 1(a)
plot(prostate$lcavol, prostate$lpsa, xlab = "lcavol", ylab = "lpsa",
     main = "Relationship between lcavol and lpsa")
# 1(b)
mean(prostate$lcavol) # 1.35001
mean(prostate$lpsa)   # 2.478387
sd(prostate$lcavol)   # 1.178625
sd(prostate$lpsa)     # 1.154329
cor(prostate$lcavol, prostate$lpsa) # 0.7344603

# 1(c)
out = lm(prostate$lpsa ~ prostate$lcavol)
out$coef
summary.lm(out)
(summary(out)$sigma)^2
# The estimates of beta_zero_hat and beta_one_hat is 1.5072979 and 0.7193201.
# The estimate of Se^2 is 0.6201553.
abline(lm(prostate$lpsa ~ prostate$lcavol), col = "blue")

# 1(d)
# The slope means for every 1 lcavol increase there is an increase in 0.72 lpsa.
# 1(e)
# The intercept means at 0 lcavol there is about 1.51 lpsa.

# 1(f)
summary.lm(out)$r.squared
# R squared is about 0.54

# 1(g)
mean(prostate$lcavol) - sd(prostate$lcavol)
mean(prostate$lpsa)
# yi = beta_zero_hat + beta_one_hat*xi
# xi is the value of one sx lower than xbar.
# yi = 1.5072979 + (0.7193201)*(0.1713847)
1.5072979 + (0.7193201)*(0.1713847)
# yi = 1.630578
2.478387 - 1.630578
# His lpsa is expected to 0.847809 lower than the mean of lpsa.

# 1(h)
n <- 97
se_beta_zero_hat <- 0.12194
beta_zero_hat <- 1.5072979
se_beta_one_hat <- 0.06819
beta_one_hat <- 0.7193201
qt(.90, 95)
CI_Lower_beta_zero <- beta_zero_hat - (qt(.95, n-2)*se_beta_zero_hat)
CI_Upper_beta_zero <- beta_zero_hat + (qt(.95, n-2)*se_beta_zero_hat)
# 90% CI for beta_zero is [1.304749, 1.709847].
CI_Lower_beta_one <- beta_one_hat - (qt(.95, n-2)*se_beta_one_hat)
CI_Upper_beta_one <- beta_one_hat + (qt(.95, n-2)*se_beta_one_hat)
# 90% CI for beta_one is [0.606053, 0.8325872].

# 1(i)
C_alpha <- qt(.95, n-2) # 1.6661052
abs(beta_one_hat/se_beta_one_hat) #10.54876
# 10.54876 is greater than 1.661052.
# Lpsa is linearly related.

# 1(j)
x_zero <- 2
# mu_zero = beta_zero + beta_one*x_zero
mu_zero <- beta_zero_hat + beta_one_hat*x_zero # 2.945938
# The expected lpsa mu_zero is 2.945938.

# 1(k)
new.lcavol = data.frame(lcavol = 2)
predict(out, newdata = new.lcavol, interval = "confidence", level = .95)
# 1(l)
predict(out, newdata = new.lcavol, interval = "prediction", level = .95)
