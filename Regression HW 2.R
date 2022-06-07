# Regression Methods HW 2
# Setting the Working Directory and Uploading HW1
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
library(foreign)
kidiq=read.dta("kidiq.dta")
dim(kidiq)
head(kidiq)

# 1(a)
mom_hs_or_not <- kidiq$mom_hs
mom_hs_or_not = (mom_hs_or_not-.5)*2
mom_hs_or_not
# 1(b)
plot(mom_hs_or_not, kidiq$kid_score, xlab = 'Whether Mom attended HS or Not', 
     ylab = "Kid's Score", main = "Range of Scores Based on Mother's HS Education")
# 1(c)
lm(kid_score ~ mom_hs, data=kidiq)
abline(lm(kid_score ~ mom_hs, data=kidiq), col = 'blue')
# 1(d)
# The slope means that for every 1 point increase in mom's attendance of HS, 
# a kid's score increases 11.77 points. However, since there is a 2 point 
# difference from -1 to 1, in real-life context, if the mom attends HS, 
# the kid's score goes up about 23.54 points.
# 1(e)
# The intercept is 77.55. This means that if the mom does not attend HS, 
# the kid's score is about 77.55.

## 2 ##
mom_hs_zero_grade <- subset(kidiq,mom_hs==0,)
head(mom_hs_zero_grade)

mom_hs_one_grade <- subset(kidiq,mom_hs==1,)
head(mom_hs_one_grade)

# 2 mom_hs = 0
plot(mom_hs_zero_grade$mom_iq, mom_hs_zero_grade$kid_score, 
     xlab = "Mom's Score", ylab = "Kid's Score", 
     main = "Scatterplot of scores where Moms did not go to HS")
lm(kid_score ~ mom_iq, data=mom_hs_zero_grade)
abline(lm(kid_score ~ mom_iq, data=kidiq), col = 'blue')
# Within the group where mom's did not attend high school, the slope means that
# for every 1 point increase in mom's score, a kid's score increases 0.9689 points.

# mom_hs = 1
plot(mom_hs_one_grade$mom_iq, mom_hs_one_grade$kid_score, 
     xlab = "Mom's Score", ylab = "Kid's Score", 
     main = "Scatterplot of scores where Moms did go to HS")
lm(kid_score ~ mom_iq, data=mom_hs_one_grade)
abline(lm(kid_score ~ mom_iq, data=kidiq), col = 'blue')
# Within the group where mom's did attend high school, the slope means
# for every 1 point increase in mom's score, a kid's score increases 0.4846 points.