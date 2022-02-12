# Question 2 : Discussion of Multicollinearity Problem

# Loading required packages
library(datasets) # data for multicollinearity
library(tidyverse)
library(olsrr) # VIFtol
library(stargazer) # regtables

# Loading the dataset
data <- datasets::EuStockMarkets %>% data.frame()
#write.csv(data, "eustocks.csv")

# Exploring the dataset
str(data)

# Reviewing the summary of the dataset
summary(data)

# a)
# regression: dax ~ SMI + CAC + FTSE
fit <- lm(DAX ~ SMI + CAC + FTSE, data = data)
summary(fit)

# b)
# Calculate the VIFs and the TIFs for all independent variables
ols_vif_tol(fit)

# c)
# regression: dax ~ SMI + CAC + FTSE
fit2 <- lm(DAX ~ CAC + FTSE, data = data)
summary(fit2)

# Calculate the VIFs and the TIFs for all independent variables
ols_vif_tol(fit2)

# Display the two model together for coefficients and standard error comparison
stargazer(fit, fit2, type= "text")

# d)
# Calculate the AIC and the BIC for the two models
tab <- data.frame(AIC = AIC(fit, fit2),
                  BIC =  BIC(fit, fit2))

# Rename the rows and display the results
rownames(tab) <- c("Full model", "without SMI")
tab