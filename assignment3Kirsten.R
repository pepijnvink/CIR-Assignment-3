# load packages

# make control group with n = 50, mean = 150, sd = 15
set.seed(679)
control <- rnorm(50, 150, 15)

# make experimental group with n = 50, mean = 160, sd = 15
set.seed(679)
exp <- rnorm(50, 160, 15)

# perform a t-test 
t.test(control, exp)

# The difference between the groups is detected (p < 0.001).
# The difference between the group means is exactly 10. 

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

my.power <- function(n, sd, mean, dif, pvalue, iterations){
  
  # make a vector to save the t-values of every iteration
  vector <- c(rep(0, iterations))
  
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
    
  # print 
  return(power)
}

# test the function
my.power(n = 50, 
         sd = 15, 
         mean = 150, 
         dif = 10, 
         pvalue = 0.05, 
         iterations = 1000)

# With this function for the power, the results will slightly differ every time you run it.
# This is because we cannot set a seed for the random drawing of groups.
# If we would put a seed, we would every time obtain the same p-value and
# the power would either be 0 or 1. 

# We can test how much the power can differ

# Make a vector of length 100
powervector <- c(rep(0, 100))

# Request the power 1000 times
for(i in 1:100){
  power <- my.power(n = 50, 
           sd = 15, 
           mean = 150, 
           dif = 10, 
           pvalue = 0.05, 
           iterations = 1000)
  
  # Put into the vector
  powervector[i] <- power
}

# The interval between the maximum and the minimum power is:
print(paste("[",max(powervector),';',min(powervector),"]"))

# The value we found with the power.t.test function is within the interval.

