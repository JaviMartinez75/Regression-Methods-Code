# Regression Methods HW 1
# Setting the Working Directory and Uploading HW1
setwd('C:/Users/Owner/Downloads/Regression Methods Projects')
HW1 = read.table("hw1.txt",header = T)
HW1

# part a
plot(HW1$GDP, HW1$Satisfaction, xlab = 'GDP', ylab = 'Satisfaction',
     main = 'GDP vs Satisfaction')
# part b
abline(5.77,.0674, col = "blue")
# part c
france_expected_satisfaction = 5.77 + .0674*(27.4)
france_expected_satisfaction
# part d
points(27.4, (5.77 + .0674*27.4), pch=19, col = "red") # France
# part e
points(21.5, (5.77 + .0674*21.5), pch=19, col = "cyan") # Greece
points(10.1, (5.77 + .0674*10.1), pch=19, col = "yellow") # Turkey

