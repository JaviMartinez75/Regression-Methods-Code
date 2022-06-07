# Regression HW 7
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
install.packages("faraway")
library(faraway)
data(prostate)
head(prostate)

# 1
n <- 50
p <- 3
# a)
  # Source      DF        Sum of Squares        Mean Squares        F       
# Regression    3           166.06                55.353          6.394
# Residual      46          398.22                 8.657
# Total         49          564.28

# b)
1-pf(6.394, 3, 46)
# P-value is 0.001031.The p-value is less than 0.05. Therefore, the null hypothesis rejected.

# 2
out20 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
            data=prostate)
out100 <- lm(lpsa ~ lcavol, data = prostate)
anova(out100, out20)

# a)
  # Source      DF        Sum of Squares        Mean Squares        F       
# Regression    7           14.752                 2.107          4.1992
# Residual      88          44.163                 0.502
# Total         95          58.915

# b)
1-pf(4.1992, 7, 88)
# P-value is 0.00049. The p-value is less than 0.05. Therefore, the null hypothesis rejected.
