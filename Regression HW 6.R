# Regression HW 6
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
install.packages("faraway")
library(faraway)
data(prostate)
head(prostate)
# 1(a)
out = lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
         data=prostate) ###Using predictors
summary(out)
# Beta_zero_hat is 0.669337.
# Beta_one_hat is 0.587022.
# Beta_two_hat is 0.454467.
# Beta_three_hat is -0.019637.
# Beta_four_hat is 0.107054.
# Beta_five_hat is 0.766157.
# Beta_six_hat is -0.105474.
# Beta_seven_hat is 0.045142.
# Beta_eight_hat is 0.004525.
# Sigma^2_hat is 0.5018525.

# 1(b)
out.s=summary(out)
names(out.s)
y.fit=out$fitted # Command line to extract vector of fitted values.
y.residual=out$residuals # Command line to extract residual vector.
# 1(c)
SSE <- sum(y.residual^2) # 44.16302
SSR <- var(y.fit)*(n-1) # 42.74972
SST <- SSR + SSE        # 86.91275
se2=sum(y.residual^2)/(97-8-1)
se=sqrt(se2)
se2
se
out.s$sigma^2 # 0.5018525

# 1(d)
r_squared <- SSR/SST
# R^2 is 0.4918694.
out.s$r.squared # 0.6547541

# 1(e)
X = model.matrix(~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data=prostate)
t(X)%*%y.residual
# Inner Product results:
#  5.773160e-15
# -1.776357e-14
#  5.329071e-15
#  5.826450e-13
# -4.662937e-15
# -4.440892e-16
# -1.687539e-14
#  8.171241e-14
# -9.237056e-14

# 1(f)
out1 = lm(lpsa ~ lcavol, data = prostate) ###Using predictors
summary(out1)
#lcavol is 0.71932 under SLR and lcavol is 0.587022 under MLR.
