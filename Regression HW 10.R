# Regression HW 10
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
install.packages("faraway")
library(faraway)
data(sat)
head(sat)
dim(sat)
# 1
lmod <- lm(total ~ expend + ratio + salary + takers, data = sat)
summary(lmod)
lmod
# (a)
qqnorm(residuals(lmod), ylab="Residuals", main="Normal Q-Q Plot: SAT Data")
qqline(residuals(lmod))
shapiro.test(residuals(lmod))
# W = 0.97691, p-value = 0.4304
# The p-value of 0.4301 is greater than 0.05. Thus, we fail to reject the null hypothesis of the
# Shapiro-Wilk test. We can conclude that the data is normal.

# (c)
lmod <- lm(total ~ expend + ratio + salary + takers, data = sat)
hatv <- hatvalues(lmod)
head(hatv)
hatv
sum(hatv)
states <- row.names(sat)
halfnorm(hatv, labs = states, ylab = "Residuals") # Half-normal plot of the leverages.

rstandard((lmod))
qqnorm(rstandard(lmod))
abline(0,1)

# (b)
# hii >= [2(p+1)]/(n) where n = 50 and p = 4
# hii >= 10/50 = 0.2
hatv
# The hatv function in the prevous line gives the leverage (hii) of every state.
# The states that are leverage points are the ones where the leverage(hii) is greater than [2(p+1)]/(n).
# [2(p+1)]/(n) equates to 0.2. The following leverage states have leverages greater than 0.2:
# California, Connecticut, Iowa, New Jersey, and Utah.

# (d)
# Jacknife residuals
rstudent(lmod)
abs(rstudent(lmod))
# Bonferroni procedure
dim(sat)
lmod <- lm(total ~ expend + ratio + salary + takers, data = sat)
stud <- rstudent(lmod)
stud[which.max(abs(stud))]
qt(.05/(50*2),44) # -3.525801
# To decide which states are outliers, we compare each of the jackknife residuals to the
# cutoff value of C(alpha) which is -3.525801. If the jackknife residual is greater than C(alpha)
# than that state is an outlier. Upon further review, none of the states are outliers.

# (f)
lmod <- lm(total ~ expend + ratio + salary + takers, data = sat)
summary(lmod)
cook <- cooks.distance(lmod)
halfnorm(cook,3,labs=states,ylab="Cook's distances")

# (e)
cook <- cooks.distance(lmod)
cook # This line of code shows all of Cook's distances.
# To decide which states are influlential points, I need to compare each of Cook's distances 
# with the value of 1. If the distance is greater than 1, then the state is an influential point.
# Upon further review, all of the values are not greater than 1. 
# Thus, none of the states are influential points