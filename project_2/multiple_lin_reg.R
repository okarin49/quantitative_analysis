# W4479_Project2_Q2_Group60

# load required packages
library(tidyverse)

# load data from CSV file
housing <- read.csv("ameshousing.csv")

# look at structure
str(housing)

# i)
reg <- lm(SalePrice ~ ., data = housing)
summary(reg)
summary(reg)$fstatistic

# ii)
# We remove the variable 'YrSold'
reg2 <- lm(SalePrice ~ .-YrSold, data = housing)
summary(reg2)

# iii)
# Let us compare the adjusted R^2 of both models
summary(reg)$adj.r.squared
summary(reg2)$adj.r.squared

# iv)
# write in the data manually
housing_new <- data.frame(LotArea = rep(5000, 3), YearBuilt = c(2000, 1910, 2000),
                          BedroomAbvGr = c(1, 2, 2), GarageCars = c(2, 2, 2),
                          YrSold = c(2010, 2010, 2006))

predictions <- predict(reg2, housing_new)
predictions