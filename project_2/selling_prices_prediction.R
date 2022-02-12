# W4479_Project2_Q4_Group60

# load required packages
library(tidyverse)
library(stargazer)
library(ggpubr)

# i)
# load data from CSV file
insu_df = read.csv("insurance.csv")

# explore the data
str(insu_df)

# preview first couple of rows
head(insu_df)

# display all variables names for later copy-paste of variables names
names(insu_df)

# Let's focus on some easy to understand variables
insu_df = insu_df %>%
  select(charges, smoker, bmi, age)

# fix some data type issues
insu_df = insu_df %>%
  mutate(smoker = as.factor(smoker))

# Visualize the relationship between the responsive variable (charges) and the numerical explanatory variables
plot1 = ggplot(data = insu_df, aes(x = bmi, y = charges, color = smoker)) +
  geom_point() + labs(title = "Bmi on Charges")

plot2 = ggplot(insu_df, aes(x = age, y = charges, color = smoker)) +
  geom_point() + labs(title = "Age on Charges")

ggarrange(plot1, plot2)

# ii)
# fitting at least 5 linear regression using different combinations of dependent variables
## linear regression 1
reg1 = lm(charges ~ age, data = insu_df)
stargazer(reg1, intercept.bottom = FALSE, single.row = TRUE, type = "text")
summary(reg1)

## linear regression 2
reg2 = lm(charges ~ bmi, data = insu_df)
stargazer(reg2, intercept.bottom = FALSE, single.row = TRUE, type = "text")
summary(reg2)

## linear regression 3
reg3 = lm(charges ~ smoker, data = insu_df)
stargazer(reg3, intercept.bottom = FALSE, single.row = TRUE, type = "text")
summary(reg3)

## linear regression 4
reg4 = lm(charges ~ age + bmi, data = insu_df)
stargazer(reg4, intercept.bottom = FALSE, single.row = TRUE, type = "text")
summary(reg4)

## linear regression 5
reg5 = lm(charges ~ age + smoker, data = insu_df)
stargazer(reg5, intercept.bottom = FALSE, single.row = TRUE, type = "text")
summary(reg5)

## linear regression 6
reg6 = lm(charges ~ bmi + smoker, data = insu_df)
stargazer(reg6, intercept.bottom = FALSE, single.row = TRUE, type = "text")
summary(reg6)

## linear regression 7
reg7 = lm(charges ~ age + bmi + smoker, data = insu_df)
stargazer(reg7, intercept.bottom = FALSE, single.row = TRUE, type = "text")
summary(reg7)

# iii)
# calculating the adjusted R squared criterion for each model
summary(reg1)$adj.r.squared
summary(reg2)$adj.r.squared
summary(reg3)$adj.r.squared
summary(reg4)$adj.r.squared
summary(reg5)$adj.r.squared
summary(reg6)$adj.r.squared
summary(reg7)$adj.r.squared


# iv)
# calculating the 0.95 and the 0.99 confidence intervals for each parameter of the best model
beta_0 = coef(reg7)[1]
beta_1 = coef(reg7)[2]
beta_2 = coef(reg7)[3]
beta_3 = coef(reg7)[4]


t_0.025 = qt(0.975, df = 1334)
t_0.005 = qt(0.995, df = 1334)				

se_beta_0 = coef(summary(reg7))[1,"Std. Error"]
se_beta_1 = coef(summary(reg7))[2,"Std. Error"]
se_beta_2 = coef(summary(reg7))[3,"Std. Error"]
se_beta_3 = coef(summary(reg7))[4,"Std. Error"]

CI0_95 = c(beta_0 - t_0.025*se_beta_0, beta_0 + t_0.025*se_beta_0)
CI1_95 = c(beta_1 - t_0.025*se_beta_1, beta_1 + t_0.025*se_beta_1)
CI2_95 = c(beta_2 - t_0.025*se_beta_2, beta_2 + t_0.025*se_beta_2)
CI3_95 = c(beta_3 - t_0.025*se_beta_3, beta_3 + t_0.025*se_beta_3)

CI0_99 = c(beta_0 - t_0.005*se_beta_0, beta_0 + t_0.005*se_beta_0)
CI1_99 = c(beta_1 - t_0.005*se_beta_1, beta_1 + t_0.005*se_beta_1)
CI2_99 = c(beta_2 - t_0.005*se_beta_2, beta_2 + t_0.005*se_beta_2)
CI3_99 = c(beta_3 - t_0.005*se_beta_3, beta_3 + t_0.005*se_beta_3)

names(CI0_95) = c("lower bound", "upper bound")
names(CI1_95) = c("lower bound", "upper bound")
names(CI2_95) = c("lower bound", "upper bound")
names(CI3_95) = c("lower bound", "upper bound")
names(CI0_99) = c("lower bound", "upper bound")
names(CI1_99) = c("lower bound", "upper bound")
names(CI2_99) = c("lower bound", "upper bound")
names(CI3_99) = c("lower bound", "upper bound")

CI0_95
CI1_95
CI2_95
CI3_95

CI0_99
CI1_99
CI2_99
CI3_99
