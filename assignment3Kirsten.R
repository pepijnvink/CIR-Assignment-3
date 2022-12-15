# make a (approximately normally distributed) control group  
set.seed(679) # set seed 
control <- rnorm(50, 150, 15) # n = 50, mean = 150, sd = 15

# make (approximately normally distributed) experimental group 
set.seed(679) # set seed  
exp <- rnorm(50, 160, 15) # n = 50, mean = 160, sd = 15

# perform a t-test to compare the control and experimental group 
t.test(control, exp)

# The difference between the groups is detected (p < 0.001).
# The difference between the group means is exactly 10. 
# This is because we use the same seed for both samples. 

# perform test to determine power
power.t.test(n = 50, # 50 observations per group
             delta = 10, # true difference in means is 10
             sd = 15, # standard deviation is 15
             sig.level = 0.05, # alpha is 0.05
             type = "two.sample", # two sample t-test
             alternative = "two.sided") # two sided test

# Power = 0.9099633.

# make a function to determine the power

# Input:
# n = group size
# sd = standard deviation of both groups
# mean = expected mean of one of the groups
# dif = expected difference between groups
# pvalue = alpha level
# iterations = number of iterations
# seed = seed for generating samples

# Output: 
# power = the power of the t-test

my.power <- function(n, sd, mean, dif, pvalue, iterations, seed){
  
  # make a vector to save the t-values of every iteration
  vector <- c(rep(0, iterations))
  
  # set seed
  set.seed(seed)
  
  for(i in 1:iterations){
    
    # create groups
    control <- rnorm(n, mean, sd)
    exp <- rnorm(n, mean+dif, sd)
    
    # perform t-test
    test <- t.test(control, exp)
    
    # save t-values in vector
    vector[i] <- test$p.value
  }
  
  # calculate power
  # power is the part of significant results given that there is a difference in reality
  power <- length(vector[vector<=pvalue]) / length(vector)
  
  # when calling the function, print the calculated power
  return(power = power)
}

# test the function
my.power(n = 50, 
         sd = 15, 
         mean = 150, 
         dif = 10, 
         pvalue = 0.05, 
         iterations = 50000000,
         seed = 679)

# my.power = 0.898; power.t.test = 0.9099633

# The power of our power test is lower than the calculated power from the power.t.test function.
# An explanation for this is that the sample is only taken 1000 times. 
# As the power depends on the sample, and the sample is random, the power differs from the true power.
# Increasing the iteration increases the reliability of the power function. 

# test the function with 10000 iterations
my.power(n = 50, 
         sd = 15, 
         mean = 150, 
         dif = 10, 
         pvalue = 0.05, 
         iterations = 10000,
         seed = 679)

# my.power = 0.9038; power.t.test = 0.9099633

# test the function with 100000 iterations
my.power(n = 50, 
         sd = 15, 
         mean = 150, 
         dif = 10, 
         pvalue = 0.05, 
         iterations = 100000,
         seed = 679)

# my.power = 0.90911; power.t.test = 0.9099633

# test the function with 500000 iterations
my.power(n = 50, 
         sd = 15, 
         mean = 150, 
         dif = 10, 
         pvalue = 0.05, 
         iterations = 500000,
         seed = 679)

# my.power = 0.9093; power.t.test = 0.9099633

# With our function we can get really close to the real power. 
# However, this costs the computer more time. 


