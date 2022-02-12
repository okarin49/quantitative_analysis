# Question 3 : Linear Regression with Heteroscedasticity

# Loading required packages
library(tidyverse)
library(stargazer) # regtables
library(Ecdat) # econ data
library(car) # ncv

# a)
# Loading the dataset
crime <- Ecdat::Crime %>% data.frame()
#write.csv(crime, "crime.csv")

# Exploring the dataset
str(crime)

# Reviewing the summary of the dataset
summary(crime)

# b)
# Linear regression model of crmrte on density
m_hetero <- lm(crmrte ~ density, data = crime)
stargazer(m_hetero, type = "text")

# Plot the data with the regression line
ggplot(crime, aes(y=crmrte, x=density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Crime rate and population density") +
  ylab("Crime committed per person") +
  xlab("hundreds of people per square mile")

# c)
# Test for Heteroscedasticity (nonconstantvariance test)
ncvTest(m_hetero)

# d)
# Residuals plots
data.frame(residuals_squared = residuals(m_hetero)^2, density = crime$density) %>%
  ggplot(., aes(y = residuals_squared, x = density)) +
  geom_point()
geom_smooth(method = "lm", formula = "y ~ x")

# WLS
m_wls <- lm(crmrte ~ density,
            weights = I(density + density^2),
            data = crime)

stargazer(m_hetero, m_wls, type = "text")