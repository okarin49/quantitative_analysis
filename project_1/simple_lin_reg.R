#### Change the MatrNo to one of the true student number of you
MatrNo = 9876543
set.seed(MatrNo)

#### part 1: a simple linear regression designed for your team 
beta1 = sample((1:20)/4 + 5, 1)         ## a constant between 5.25 and 10
beta2 = sample((1:20)/100 + 0.75, 1)    ## a slope between 0.76 and 0.95
sig2  = sample((4:12)/4, 1)             ## a variance between 1 and 3 for N(0, sig2)

#### call the three values and state your (theoretical) regression model 
beta1
beta2
sig2

#### part 2: a simulation with n=100 and M=200 samples (replications) 
#### You can try to use other M value, e.g. 1000, to see what happens
M  = 200
n1 = 100
X1 = ((1:n1) - 0.5)/n1 * 50 + 5 ## X1-variable fixed for all simulations in part 2

beta.est1 = matrix(0, 2, M)

for(m in 1:M){
  eps1.m = rnorm(n1) * sqrt(sig2)
  Y1.m   = beta1 + beta2 * X1 + eps1.m
  SRL1.m = lm(Y1.m ~ X1)
  beta.est1[, m] = SRL1.m$coefficients
}

# mean and variance of beta1
mean(beta.est1[1,])
var(beta.est1[1,])
# mean and variance of beta2
mean(beta.est1[2,])
var(beta.est1[2,])
# covariance
cov(beta.est1[1,], beta.est1[2,])
cor(beta.est1[1,], beta.est1[2,])



#### part 3: a simulation with n=100 and M=200 samples (replications) 
#### You can try to use other M value, e.g. 1000, to see what happend
ms2 = 100 ## change this to an integer between 2 - 200 to select a sample for Q2d) 
n2  = 400
X2  = ((1:n2) - 0.5)/n2 * 50 + 5 ## X2-variable fixed for all simulations in part 3

beta.est2 = matrix(0, 2, M)

for(m in 1:M){
  eps2.m = rnorm(n2) * sqrt(sig2)
  Y2.m = beta1 + beta2 * X2 + eps2.m
  if(m == ms2)
  {
    Ysm2 = Y2.m
  }
  SRL2.m = lm(Y2.m~X2)
  beta.est2[, m] = SRL2.m$coefficients
}

# mean and variance of beta1
mean(beta.est2[1,])
var(beta.est2[1,])
# mean and variance of beta2
mean(beta.est2[2,])
var(beta.est2[2,])
# covariance
cov(beta.est2[1,], beta.est2[2,])
cor(beta.est2[1,], beta.est2[2,])

## part d
# simple regression
# put data in data frame
data <- data.frame(X2, Ysm2)
# regression
reg <- lm(Ysm2 ~ X2, data = data)
# regression output
summary(reg)
# plot regression
library(ggplot2)
ggplot(data, aes(x=X2, y=Ysm2)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "X2", y = "Ysm2",
       title = "Simulation Study Regress Ysm2 on X2", 
       subtitle = "n = 400, M = 200")


## to used the corresponding saved results, you need to output them
## or use the variable names in a short code of you for further analysis
## end of the code