# Question 4

# a)

# Loading packages
library(AER)
library(tidyverse)
library(ggpubr)
library(moments)

# Load the dataset
data(Guns)

# Explore the dataset
str(Guns)

# Review the summary of the dataset
summary(Guns)

# Filter data for the year 1980
Guns_1980 = Guns %>% filter(year == "1980")
Guns_1980

# Scatterplot of robbery on income for the year 1980
plot1 = ggplot(Guns_1980, aes(income, robbery)) + geom_point() + 
                 labs(title = "Robbery on Income")
plot1

# Highlight the outlier ("District of Columbia")
plot1 + geom_point(data = subset(Guns_1980, state == "District of Columbia"),
                   aes(income, robbery, color = "District of Columbia")) + 
  labs(color = "Country")

# b)

# Filter data without District of Columbia
Guns_1980_wo_DC = filter(Guns_1980, state != "District of Columbia")
Guns_1980_wo_DC

# Plot the data
plot2 = ggplot(Guns_1980_wo_DC, aes(income, robbery)) + geom_point() + 
  labs(title = "Robbery on Income",
       caption = "Data without the outlier Disctrict of Columbia")
plot2

# State the regression model of robbery on income
reg_wo_DC = lm(robbery ~ income, data = Guns_1980_wo_DC)
coef(reg_wo_DC)

# Report of the estimated model with its error term variance
summary(reg_wo_DC)
summary(reg_wo_DC)$sigma^2

# c)

# Run the t-test
beta2 = summary(reg_wo_DC)$coef[2, 1]
se_beta2 = summary(reg_wo_DC)$coef[2, 2]
t_0.025 = qt(0.975, 48)
t_beta2 = beta2 / se_beta2

# d)

# get r^2 from summary
r_sq = summary(reg_wo_DC)$r.squared

# calculate RSS from residuals of regression
RSS = sum(reg_wo_DC$residuals^2)    

# calculate ESS from RSS and r^2
ESS = r_sq/(1 - r_sq) * RSS

# calculate TSS from ESS and RSS
TSS = ESS + RSS # oder TSS = ESS / r_sq

# show all values
r_sq; RSS; ESS; TSS                 

# F-statistic
F = summary(reg_wo_DC)$fstatistic
F

# e)

# Point Prediction for a per capita income of 10000
X0 = data.frame(income = 10000)
predict(reg_wo_DC, X0)

# forecast confidence interval for the estimated mean
predict(reg_wo_DC, X0, interval = "confidence")

# forecast confidence interval for the individual prediction
predict(reg_wo_DC, X0, interval = "prediction")

# Point Prediction for a per capita income of 18000
X1 = data.frame(income = 18000)
predict(reg_wo_DC, X1)

# forecast confidence interval for the estimated mean
predict(reg_wo_DC, X1, interval = "confidence")

# forecast confidence interval for the individual prediction
predict(reg_wo_DC, X1, interval = "prediction")

# value of robbery in state of Alaska
robbery_alaska = Guns_1980_wo_DC %>% filter(state == "Alaska")
robbery_alaska$robbery

# f)

# set up data frame
residuals_df = data.frame(Residuals = reg_wo_DC$residuals,
                          Income = Guns_1980_wo_DC$income)

# Residual Plot
plot_res = ggplot(residuals_df, aes(Income, Residuals)) + 
  geom_point() +
  labs(title = "Residual Plot")
plot_res

# Histogram of residuals
plot_hist = ggplot(residuals_df, aes(Residuals)) + 
  geom_histogram(bins = 10) + 
  labs(title = "Histogram of Residuals")
plot_hist

# qq-plot of residuals
plot_qq = ggplot(residuals_df, aes(sample = Residuals)) + 
  geom_qq() + geom_qq_line() +
  labs(title = "QQ-Plot of Residuals")
plot_qq

ggarrange(plot_res, plot_hist, plot_qq)

# Jarque-Bera Test
c_res = reg_wo_DC$residuals - mean(reg_wo_DC$residuals)
n = length(c_res)

JB = (n/6) * (skewness(c_res)^2 + 0.25 * (kurtosis(c_res) - 3)^2)
p = 1 - pchisq(JB, 2)
p; JB
