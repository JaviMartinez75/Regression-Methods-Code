# Regression HW 5
# Setting the HW Directory
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
install.packages("faraway")
library(faraway)
data(prostate)
head(prostate)

# 1(a)
dim(prostate)
# n is 97 and and p is 8.
# 1(b)
X = model.matrix(~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
                 data=prostate)
# 1(c)
y = prostate$lpsa
XtX=t(X)%*%X
beta.hat=solve(XtX,t(X)%*%y)
beta.hat
# 1(d)
out = lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
         data=prostate) ###Using predictors
summary(out)
# 1(e)
# The results for Beta.hat is identical.
# 1(f)
# || y - Xb ||^(2)
sum((y-X%*%beta.hat)^2)
# The sum of squared vertical distances is 44.16302.