# Regression HW 11
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
boston <- read.table("boston.txt",header=T,sep="\t")
head(boston)
library(boot)
install.packages('MPV')
library(MPV)
library(MASS)
library(leaps)

# Forwards method
# 1(a)
forw <- regsubsets(CMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
                     PTRATIO + B + LSTAT, method="forward", data = boston, nvmax=15)
forw.s <- summary(forw)
forw.s$which
forw.s$outmat
# Result:
lmod21 <- lm(CMEDV ~ LSTAT, boston)
summary(lmod21)
lmod22 <- lm(CMEDV ~ RM + LSTAT, boston)
summary(lmod22)
lmod23 <- lm(CMEDV ~ PTRATIO + RM + LSTAT, boston)
summary(lmod23)
lmod24 <- lm(CMEDV ~ DIS + PTRATIO + RM + LSTAT, boston)
summary(lmod24)
lmod25 <- lm(CMEDV ~ NOX + DIS + PTRATIO + RM + LSTAT, boston)
summary(lmod25)
lmod26 <- lm(CMEDV ~ CHAS + NOX + DIS + PTRATIO + RM + LSTAT, boston)
summary(lmod26)
lmod27 <- lm(CMEDV ~ B + CHAS + NOX + DIS + PTRATIO + RM + LSTAT, boston)
summary(lmod27)
lmod28 <- lm(CMEDV ~ ZN + B + CHAS + NOX + DIS + PTRATIO + RM + LSTAT, boston)
summary(lmod28)
# 1(b)
forw.s$bic
plot(forw.s$bic, main='BICs of Forward Search')
# The best model is the 11th model.
# 1(c)
forw.s$bic
# The model I would get is the 8th model. The BIC's continually decrease until the 8th model.
# The BIC increases for the 9th model.

# Backwards selection
# 1(d)
back <- regsubsets(CMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
                     PTRATIO + B + LSTAT, method="backward", data = boston, nvmax=15)
back.s <- summary(back) 
back.s$which
back.s$outmat
# Result:
lmod1 <- lm(CMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
              PTRATIO + B + LSTAT, data = boston)
summary(lmod1)
lmod2 <- update(lmod1, . ~ . - AGE)
summary(lmod2)
lmod3 <- update(lmod2, . ~ . - INDUS)
summary(lmod3)
lmod4 <- update(lmod3, . ~ . - CHAS)
summary(lmod4)
lmod5 <- update(lmod4, . ~ . - CRIM)
summary(lmod5)
lmod6 <- update(lmod5, . ~ . - ZN)
summary(lmod6)
lmod7 <- update(lmod6, . ~ . - TAX)
summary(lmod7)
lmod8 <- update(lmod7, . ~ . - RAD)
summary(lmod8)
lmod9 <- update(lmod8, . ~ . - B)
summary(lmod9)
lmod10 <- update(lmod9, . ~ . - NOX)
summary(lmod10)
lmod11 <- update(lmod10, . ~ . - DIS)
summary(lmod11)
lmod12 <- update(lmod11, . ~ . - PTRATIO)
summary(lmod12)
lmod13 <- update(lmod12, . ~ . - RM)
summary(lmod13)
lmod14 <- update(lmod13, . ~ . - LSTAT)
summary(lmod14)
# 1(e)
back.s$bic
plot(back.s$bic, main='BICs of Backward Search')
# The best model is the 11th model.
# 1(f)
back.s$bic
# The model I would get is the 6th model. The BIC's continually decrease until the 6th model.
# The BIC increases for the 7th model.

# 1(g)
all_press <- c(PRESS(lmod14),PRESS(lmod13),PRESS(lmod12),PRESS(lmod11),PRESS(lmod10),PRESS(lmod9),
               PRESS(lmod8),PRESS(lmod7),PRESS(lmod6),PRESS(lmod5),PRESS(lmod4),PRESS(lmod3),
               PRESS(lmod2),PRESS(lmod1))
# 42746.53 19414.19 15596.02 13961.45 13516.21 12771.28 12510.33 12448.90 12238.60 12042.84 11811.76
# 11693.74 11722.81 11799.56
plot(all_press, ylab = 'PRESS Values', main = 'PRESSs of the Backward Search')

# K-fold cross validation
# 1(h)
set.seed(123)
cv_one <- cv.glm(data=boston,glm(CMEDV ~ 1,data=boston),K=10)$delta[1]
cv_two <- cv.glm(data=boston,glm(CMEDV ~ LSTAT,data=boston),K=10)$delta[1]
cv_three <- cv.glm(data=boston,glm(CMEDV ~ RM + LSTAT,data=boston),K=10)$delta[1]
cv_four <- cv.glm(data=boston,glm(CMEDV ~ PTRATIO + RM + LSTAT,data=boston),K=10)$delta[1]
cv_five <- cv.glm(data=boston,glm(CMEDV ~ DIS + PTRATIO + RM + LSTAT,data=boston),K=10)$delta[1]
cv_six <- cv.glm(data=boston,glm(CMEDV ~ NOX + DIS + PTRATIO + RM + LSTAT,
                                 data=boston),K=10)$delta[1]
cv_seven <- cv.glm(data=boston,glm(CMEDV ~ B + NOX + DIS + PTRATIO + RM + LSTAT,
                                   data=boston),K=10)$delta[1]
cv_eight <- cv.glm(data=boston,glm(CMEDV ~ RAD + B + NOX + DIS + PTRATIO + RM + LSTAT,
                                   data=boston),K=10)$delta[1]
cv_nine <- cv.glm(data=boston,glm(CMEDV ~ TAX + RAD + B + NOX + DIS + PTRATIO + RM + LSTAT,
                                  data=boston),K=10)$delta[1]
cv_ten <- cv.glm(data=boston,glm(CMEDV ~ ZN + TAX + RAD + B + NOX + DIS + PTRATIO + RM + LSTAT,
                                 data=boston),K=10)$delta[1]
cv_eleven <- cv.glm(data=boston,glm(CMEDV ~ CRIM + ZN + TAX + RAD + B + NOX + DIS + PTRATIO + RM + LSTAT,
                                    data=boston),K=10)$delta[1]
cv_twelve <- cv.glm(data=boston,glm(CMEDV ~ CHAS + CRIM + ZN + TAX + RAD + B 
                                    + NOX + DIS + PTRATIO + RM + LSTAT,data=boston),K=10)$delta[1]
cv_thirteen <- cv.glm(data=boston,glm(CMEDV ~ INDUS + CHAS + CRIM + ZN + TAX + RAD + B 
                                      + NOX + DIS + PTRATIO + RM + LSTAT,data=boston),K=10)$delta[1]
cv_fourteen <- cv.glm(data=boston,glm(CMEDV ~ AGE + INDUS + CHAS + CRIM + ZN + TAX + RAD + B 
                                      + NOX + DIS + PTRATIO + RM + LSTAT,data=boston),K=10)$delta[1]

all_cvs <- c(cv_one, cv_two, cv_three, cv_four, cv_five, cv_six, cv_seven, cv_eight,
             cv_nine, cv_ten, cv_eleven, cv_twelve, cv_thirteen, cv_fourteen)
# 84.69478 38.56701 30.94194 27.55003 26.78518 25.38858 24.56027 24.88926 24.19370 23.85439 23.61851
# 23.43446 23.47274 23.57475
plot(all_cvs, ylab = 'CV Values',main = 'CVs of the Backward Search')

# 1(i)
## k= log(50) is using BIC. 
## k=2 is using AIC, which is the default
out.null <- lm(CMEDV ~ 1,data = boston)
full <- formula(lm(CMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
                     PTRATIO + B + LSTAT, data = boston))
out.full <- lm(CMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
                 PTRATIO + B + LSTAT,data = boston)

out.both <- step(out.full,scope=list(lower=~1,upper=full),direction="both" 
                 ,data = boston,trace=FALSE,k=2)
out.both$anova
out.both$call
# lm(formula = CMEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + 
#           TAX + PTRATIO + B + LSTAT, data = boston)

# 1(j)
out.null <- lm(CMEDV ~ 1,data = boston)
full <- formula(lm(CMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
                     PTRATIO + B + LSTAT, data = boston))
out.full <- lm(CMEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
                 PTRATIO + B + LSTAT,data = boston)

out.both <- step(out.full,scope=list(lower=~1,upper=full),direction="both" 
              ,data = boston,trace=FALSE,k=log(50))
out.both$anova
out.both$call
# lm(formula = CMEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + 
#           TAX + PTRATIO + B + LSTAT, data = boston)

# 1(k)
# The best model from parts (i) and (j) are the same. This means that using AIC or BIC
# usually yields the same result.
