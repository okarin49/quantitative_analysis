# Question 1 : Application of Dummy Variables in Regression

# Loading required packages
library(AER)
library(tidyverse)

# Loading the dataset
data("GSOEP9402", package = "AER")

# Applying some convenience data transformation
GSOEP = GSOEP9402

# Exploring the dataset
str(GSOEP)

# Reviewing the summary of the dataset
summary(GSOEP)

# Expected relationship between variables meducation and school
ggplot(GSOEP, aes(meducation, y = income, color = school)) + geom_point() + labs(title = "Income on Years of Education")

# a)
# Encoding categorical variable "school" using suitable dummy variables
school = as.numeric((GSOEP[,1]))
                    Hauptschule = as.numeric(school == 1)
                    Realschule = as.numeric(school == 2)
                    Gymnasium = as.numeric(school == 3)
# b)
# Regression of income on meducation and the dummy variables school
reg_b = lm(income ~ meducation + Realschule + Gymnasium, data = GSOEP)
summary(reg_b)

# c)
# Regression in the levels and in the slopes
reg_c = lm(income ~ meducation + Realschule + Gymnasium + I(Realschule * meducation)
          + I(Gymnasium * meducation), data = GSOEP)
summary(reg_c)

# d)
# Encoding the categorical variable "gender" using suitable dummy variables
gender = as.numeric((GSOEP[,3]))
            male = as.numeric(gender == 1)
            female = as.numeric(gender == 2)

# Regression of income on meducation and school and gender as dummy variables
reg_d = lm(income ~ meducation + female + Realschule + Gymnasium + I(female * Realschule) + I(female * Gymnasium), data = GSOEP)
summary(reg_d)
coef(reg_d)