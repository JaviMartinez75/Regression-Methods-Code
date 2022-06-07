# Regression HW 8
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
install.packages("faraway")
library(faraway)
data(prostate)
head(prostate)
# 1(a)
# Null: lweight is equal to 0. 
# Alt:  lweight is not equal to 0.
lmod <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
           data=prostate)
lmods <- lm(lpsa ~ lcavol + age + lbph + svi + lcp + gleason + pgg45, 
           data=prostate)
anova(lmods, lmod)
  # Source      DF        Sum of Squares        Mean Squares        F       
# SSElarge      88            44.163              0.50185     
# SSEsmall      89            47.749               
# Difference    1              3.586              3.586             7.1457
# The p-value is .008955 which is less than 0.05. Thus, the null hypothesis is rejected.
# We conclude that lweight is not equal to 0.
# 1(b)
# Null: lweight is equal to 0.5.
# Alt: lweight is not equal to 0.5.
out33 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
            data=prostate)
out333 <- lm(lpsa ~ lcavol + offset(0.5 * lweight) + age + lbph + svi + lcp + gleason + pgg45, 
             data=prostate)
summary(out33)
summary(out333)
T <- (0.454467 - 0.5)/(0.170012)
(1 - pt(abs(T), 88))*2
anova(out333, out33)
# The p-value is 0.78946 which is greater than 0.05. Thus, we fail to reject the null hypothesis.
# Thus, the better model where lweight is equal to 0.5.
# 1(c)
# Null: gleason and pgg45 is equal to 0. 
# Alt:  at least one of gleason or pgg45 is not equal to 0.
out10 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
            data=prostate)
out20 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp, 
            data=prostate)
anova(out20, out10)
  # Source      DF        Sum of Squares        Mean Squares        F       
# SSElarge      88            44.163              0.50185     
# SSEsmall      90            45.396               
# Difference    2             1.233               1.233             2.4569
# The p-value is 0.2976 which is greater than 0.05. Thus, we fail to reject the null hypothesis.
# We conclude that the better model is where gleason and pgg45 are equal to 0.
# 1(d)
# Null: lweight and lcavol are the same.
# Alt: lweight and lcavol are not the same.
out50 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
            data=prostate)
out500 <- lm(lpsa ~ I(lcavol + lweight) + age + lbph + svi + lcp + gleason + pgg45, 
             data=prostate)
anova(out500, out50)
  # Source      DF        Sum of Squares        Mean Squares        F       
# SSElarge      88            44.163              0.50185      
# SSEsmall      89            44.378              
# Difference    1              0.215              0.215             0.4286
# The p-value is 0.5144 which is greater than 0.05. Thus, we fail to reject the null hypothesis.
# We conclude that the better model is where lweight and lcavol are the same.
# 1(e)
lmod <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
           data=prostate)
confint(lmod, level = .95)
# 95% Confidence Intervals:
# lcavol:  [0.412298699, 0.761744954]
# lweight: [0.116603435, 0.792331414]
# age:     [-0.041840618, 0.002566267]
# lbph:    [-0.009101499, 0.223209561]
# svi:     [0.280644232, 1.251670420]
# lcp:     [-0.286344443, 0.075395916]
# gleason: [-0.267786053, 0.358069248]
# pgg45:   [-0.004260932, 0.013311395]
# 1(f)
lmod <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
           data=prostate)
set.seed(123)
B <- 5000
coefmat <- matrix(NA, B, 9)
resids <- residuals(lmod)
preds <- fitted(lmod)
for(i in 1:B) {
  booty = preds + sample(resids, rep=TRUE)
  bmod = update(lmod, booty ~ .)
  coefmat[i,] = coef(bmod)}
colnames(coefmat) <- c("Intercept",colnames(prostate[,2:9]))
coefmat <- data.frame(coefmat)
apply(coefmat,MARGIN=2,function(x) quantile(x,c(0.025,0.975)))

#       Intercept   lweight       age         lbph          svi       lcp     gleason
# 2.5%  -1.756013 0.4228544 0.1418059 -0.040506846 -0.001928083 0.3015895 -0.27204683
# 97.5%  3.056538 0.7458902 0.7867227  0.001345696  0.216017683 1.2064565  0.06886532

#           pgg45       lpsa
# 2.5%   -0.2444553 -0.003656029
# 97.5%   0.3388133  0.012836978
