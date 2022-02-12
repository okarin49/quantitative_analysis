# W4479_Project2_Q3_Group60

# i) Input the data and display the scatter plot matrix
library(ISLR)
data(Auto)
str(Auto)  ## to show the structure of this data set

# We will use the log-transformed data of the 1., and 3. to 5. columns 
pairs(log(Auto[,c(1, 3, 4,5)])) #### display the scatter matrix of those variables 

Y = log(Auto[, 1]) #### mpg, Disp
X2 = log(Auto[, 3]) #### displacement
X3 = log(Auto[, 4]) #### horsepower
X4 = log(Auto[, 5]) #### weight

# Originally Y was in column 1 of Auto and was the variable mpg, meaning how many miles per gallon the car could drive on average
# X2 was in column 3 of Auto and is the engine displacement (cu. inches)
# X3 was in column 4 of Auto and is the engine horspower
# X4 was in column 5 of Auto and is the vehicle weight in lbs.

# To judge weather the log transformation was a suitable choice,
# we should compare the scatter matrix above with a scatter matrix of the original non-log-transformed variables
pairs(Auto[,c(1, 3, 4,5)])
# I would say that a log-transformation is a suitable choice here as it makes the relationships between the dependent variable (mpg) and independent variables more linear than before.
# A linear relationship is important here as we want to fit a linear regression model later on. 


#ii) Fit the model based on the complete data set and basic analysis
M.data = lm(Y ~ X2 + X3 + X4)

# report fitted model
summary(M.data)

# estimated variance-covariance matrix of the estimated coefficients
cov_mat <- vcov(summary(M.data))
cov_mat

# All three coefficients of the X-variables are significantly different from 0 at the 0.05-level and therefore important
# we look at the correlation matrix of the estimated coefficients to rule out multicolinearity
cov2cor(cov_mat)
# We see that X4 has a high correlation with the intercept. This means that the intercept will have a high variability.
# However, since the intercept has no interpretation in our case (the case X1 = X2 = X3 = X4 = 0 does not exist), we can simply ignore the high correlation between X4 and the intercept.
# We conclude there is not a multicolinearity problem in the fitted model and all variables are important.




#### Part 3: The bootstrap study
set.seed(6866552)  #### Replace this with one of your student number!

n = length(Y)
M = 1000
coeff.boot = matrix(0, M, 4)

for(m in 1:M)
{
  Indm = sample(1:n, n, replace  =  TRUE) 
  Yb = log(Auto[Indm, 1]) #### mpg, Disp
  X2b = log(Auto[Indm, 3]) #### displacement
  X3b = log(Auto[Indm, 4]) #### horsepower
  X4b = log(Auto[Indm, 5]) #### weight
  M.boot = lm(Yb ~ X2b + X3b + X4b)
  coeff.boot[m, ] = M.boot$coefficients
}

colnames(coeff.boot) <- c("Intercept", "X2", "X3", "X4")
coeff.boot <- as.data.frame(coeff.boot)

library(ggplot2)
# histogram for Intercept
ggplot(coeff.boot, aes(x = Intercept)) +
  geom_histogram(color="black", fill="lightblue") + 
  ggtitle("Histogram of Intercept")
# histogram for coefficients of X2 (displacement)
ggplot(coeff.boot, aes(x = X2)) +
  geom_histogram(color="black", fill="lightblue") + 
  ggtitle("Histogram of Coefficients of X2")
# histogram for coefficients of X3 (horsepower)
ggplot(coeff.boot, aes(x = X3)) +
  geom_histogram(color="black", fill="lightblue") + 
  ggtitle("Histogram of Coefficients of X3")
# histogram for coefficients of X4 (weight)
ggplot(coeff.boot, aes(x = X4)) +
  geom_histogram(color="black", fill="lightblue") + 
  ggtitle("Histogram of Coefficients of X4")

# We can see that the peak of the distributions of the coefficients are always very close to the coefficients we have obtained in ii)
# The peak of the distribution of the bootstrap result for the intercept matches exactly the 9.2 obtained in ii)
# The peaks of X2 and X3 are slightly below the coefficients obtained in iii), the peak of X4 slightly above.

#### part 4: empirical correlation coefficients
# bootstrap
cor(coeff.boot)
# matrix from ii)
cov2cor(cov_mat)

# We can see that some of the empirical correlation coefficients of the bootstrap closely match the ones obtained in ii) with the whole dataset.
# However, the correlation coefficients between X2 and X3 as well as X3 and X4 seem to be quite different (-0.5669 vs. -0.2973 and -0.088 vs. -0.3168 respectively)




############################################