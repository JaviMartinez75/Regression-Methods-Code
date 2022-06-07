# Regression HW 8
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
install.packages("faraway")
library(faraway)
data(prostate)
head(prostate)
# 1(a) and 1(b)
# n = 97 and p = 9
lmod <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
           data=prostate)
summary(lmod)
x <- model.matrix(lmod)
x0 <- apply(x,2,median)
x0
y0 <- sum(x0*coef(lmod))
y0
predict(lmod, new = data.frame(t(x0)))
predict(lmod, new = data.frame(t(x0)), interval = "confidence")
# The 95% confidence interval is [2.172432, 2.605664].
predict(lmod, new = data.frame(t(x0)), interval = "prediction")
# The 95% prediction interval is [0.9646538, 3.813442].

# 1(c)
x0[4] <- 20.00000
x0
y0 <- sum(x0*coef(lmod))
y0
predict(lmod, new = data.frame(t(x0)), interval = "confidence")
# The 95% confidence interval is [2.26044, 4.285002].
predict(lmod, new = data.frame(t(x0)), interval = "prediction")
# The 95% prediction interval is [1.53874, 5.006702].
# The intervals are wider because x0' is now larger. Intervals center at y0.
# A larger x0' means when intervals are created the pivot around y0 is now longer 
# and thus creating wider intervals.

# 2(a)
data(sat)
head(sat)
lmod44 <- lm(total ~ expend + ratio + salary + takers, data = sat)
fakelmod <- lm(total ~ takers, data = sat)
summary(fakelmod)
summary(lmod44)
lmod44
# Coefficients:
# (Intercept)       expend        ratio       salary       takers  
#   1045.972        4.463       -3.624        1.638       -2.904

# 2(b)
plot(fitted(lmod44),residuals(lmod44),xlab="Fitted",ylab="Residuals",main = "Fitted vs Residuals")
abline(h=0)
# Comment: The constant variance assumption is valid. There is constant symmetrical variation
# in the vertical direction across the x-axis.

# 2(c)
plot(sat$expend,residuals(lmod44), xlab="expend",ylab="Residuals",main = "Expend vs Residuals")
abline(h=0)
# Comment: The constant variance assumption is valid. There is constant symmetrical variation
# in the vertical direction across the x-axis.
plot(sat$ratio,residuals(lmod44), xlab="ratio",ylab="Residuals",main = "Ratio vs Residuals")
abline(h=0)
# Comment: The constant variance assumption is not valid. There is no constant symmetrical variation
# in the vertical direction across the x-axis.
plot(sat$salary,residuals(lmod44), xlab="salary",ylab="Residuals",main = "Salary vs Residuals")
abline(h=0)
# Comment: The constant variance assumption is valid. There is constant symmetrical variation
# in the vertical direction across the x-axis.
plot(sat$takers,residuals(lmod44), xlab="takers",ylab="Residuals",main = "Takers vs Residuals")
abline(h=0)
# Comment: The constant variance assumption is valid. There is constant symmetrical variation
# in the vertical direction across the x-axis.
