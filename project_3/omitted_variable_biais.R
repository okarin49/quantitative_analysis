# Question 4 : Omitted Variable Bias

# Loading required packages
library(tidyverse)
library(stargazer) # regtables
library(lmtest) # durbinwatson

# Loading the dataset
bike <- read.csv("bikesharing.csv")

# Exploring the dataset
str(bike)

# Reviewing the summary of the dataset
summary(bike)

# a)
# Model1
m1 <- lm(count ~ temp + windspeed, data = bike)
stargazer(m1, type = "text")
confint(m1)

# b)
# Model2
m2 <- lm(count ~ temp + windspeed + factor(month), data = bike)
stargazer(m2, type = "text")
confint(m2)[2,]

# c)
# Model3
m3 <- lm(count ~ temp + windspeed + factor(month) + humidity, data = bike)
stargazer(m3, type = "text")
dwtest(m3)

## Additional check
cor(bike$windspeed, bike$humidity) # negative correlation between wind and rain

# d)
# Model4
m4 <- lm(count ~ temp + factor(month) + humidity, data = bike)
stargazer(m4, type = "text")
dwtest(m4)

# e)
# AIC and BIC calculation for all the models
tab <- data.frame(AIC = AIC(m1, m2, m3, m4),
                  BIC = BIC(m1, m2, m3, m4)
)
rownames(tab) <- 1:4
tab

# f)
# All models for comparison
stargazer(m1, m2, m3, m4, type="text")
