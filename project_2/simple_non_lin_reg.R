# W4479_Project2_Q1_Group60

# load required packages
library(tidyverse)

# i)
# We choose a dataset diamonds from https://www.kaggle.com/shivam2503/diamonds
# load data from CSV file
diamonds = read.csv("diamonds.csv")

# ii)
Y = diamonds$price
X = diamonds$carat

# create 1/X
inverse_X = 1/diamonds$carat

# 1: Y on X
model1 = lm(Y ~ X)
summary(model1)

# 2: Y on ln(X)
model2 = lm(Y ~ log(X))
summary(model2)

# 3: Y on 1/X
model3 = lm(Y ~ inverse_X)
summary(model3)

# 4: ln(Y) on X
model4 = lm(log(Y) ~ X)
summary(model4)

# 5: ln(Y) on ln(X)
model5 = lm(log(Y) ~ log(X))
summary(model5)

# 6: ln(Y) on 1/X
model6 = lm(log(Y) ~ inverse_X)
summary(model6)

# iii)
par(mfrow = c(3, 3)) # Create a 3 x 3 plotting matrix
# 1: Y on X
plot(X, Y, main = "Regression of Y on X",
     xlab = "Carat", ylab = "Price",
     pch = 19)
abline(model1, col = "red", lwd = 2)

# 2: Y on ln(X)
plot(X, Y, main = "Regression of Y on ln(X)",
     xlab = "Carat", ylab = "Price",
     pch = 19)
curve(model2$coef[1] + model2$coef[2] * log(x),
      from = 0, to = 5, add = TRUE, col = "red", lwd = 2)

# 3: Y on 1/X
plot(X, Y, main = "Regression of Y on 1/X",
     xlab = "Carat", ylab = "Price",
     pch = 19)
curve(model3$coef[1] + model3$coef[2] * (1/x),
      from = 0, to = 5, add = TRUE, col = "red", lwd = 2)

# 4: log(Y) on X
# Retransformed model: Y = exp(b_0 + b1*X)
plot(X, Y, main = "Regression of ln(Y) on X",
     xlab = "Carat", ylab = "Price",
     pch = 19)
curve(exp(model4$coef[1] + model4$coef[2] * x),
      from = 0, to = 5, add = TRUE, col = "red", lwd = 2)

# 5: ln(Y) on ln(X)
# Retransformed model: Y = exp(b_0 + b1*ln(X))
plot(X, Y, main = "Regression of ln(Y) on ln(X)",
     xlab = "Carat", ylab = "Price",
     pch = 19)
curve(exp(model5$coef[1] + model5$coef[2] * log(x)),
      from = 0, to = 5, add = TRUE, col = "red", lwd = 2)

# 6: ln(Y) on 1/X
# Retransformed model: Y = exp(b_0 + b1*(1/x))
plot(X, Y, main = "Regression of ln(Y) on 1/X",
     xlab = "Carat", ylab = "Price",
     pch = 19)
curve(exp(model6$coef[1] + model6$coef[2] * (1/x)),
      from = 0, to = 5, add = TRUE, col = "red", lwd = 2)


# iv)
# Stating the R^2 for the different models
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared
summary(model5)$r.squared
summary(model6)$r.squared

# Stating the RSS for the different models 
# RSS model 1
RSS1 <- sum((Y - (model1$coef[1] + model1$coef[2] * X))^2)
RSS1
# RSS model 2
RSS2 <- sum((Y - (model2$coef[1] + model2$coef[2] * log(X)))^2)
RSS2

# RSS model 3
RSS3 <- sum((Y - (model3$coef[1] + model3$coef[2] * inverse_X))^2)
RSS3

# RSS model 4
RSS4 <- sum((Y - exp(model4$coef[1] + model4$coef[2] * X))^2)
RSS4

# RSS model 5
RSS5 <- sum((Y - exp(model5$coef[1] + model5$coef[2] * log(X)))^2)
RSS5

# RSS model 6
RSS6 <- sum((Y - exp(model6$coef[1] + model6$coef[2] * inverse_X))^2)
RSS6

min(c(RSS1, RSS2, RSS3, RSS4, RSS5, RSS6))
which.min(c(RSS1, RSS2, RSS3, RSS4, RSS5, RSS6))

# The best model according to the RSS remains the simple linear regression (model 1) as it has the lowest RSS

# v)
# as indicated in part iv), the best model according to the RSS is the simple linear regression:
summary(model1)
# Y = -2256.36 + 7756.43 * X