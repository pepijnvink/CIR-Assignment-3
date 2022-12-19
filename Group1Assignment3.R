## Assignment 3 
## Pepijn Vink (6100252) and Kirsten van Kessel (6791220)

# Load libraries
library(tidyverse)
library(permute) # for 'shuffle' function

## Part 1

# 1. Estimate the power using built in functions

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

# 2. Estimate the power using simulation

# Input:
# n = group size.
# sd = standard deviation of both groups.
# mean = expected mean of one of the groups.
# dif = expected difference between groups.
# pvalue = alpha level. 0.05 is the default.
# iterations = number of iterations.
# seed = optional seed for generating samples.

# Output: 
# power = the power of the t-test.

my.power <- function(n, sd, mean, dif, pvalue = 0.05, iterations, seed = NULL){
  
  # make a vector to save the p-values of every iteration
  vector <- c()
  
  # set seed if specified
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  for(i in 1:iterations){
    
    # create groups
    control <- rnorm(n, mean, sd)
    exp <- rnorm(n, mean+dif, sd)
    
    # perform t-test
    test <- t.test(control, exp)
    
    # save p-values in vector
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
         iterations = 1000,
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

# my.power = 0.909308; power.t.test = 0.9099633

# With our function we can get really close to the real power. 
# However, this costs the computer more time.

# Part 2
# 1.

# hypothesis.test performs an empirical t-test based on bootstrap or permutation resampling.
#
# Group1: Vector with data from Group 1.
# Group2: Vector with data from Group2.
# method: Character string specifying the resampling method to use for the hypothesis test. Takes "bootstrap" or "permutation".
# seed: Optional argument specifying the random seed to be used.
# samples: Whole number specifying the number of samples to take for resampling.
# alternative: Character string specifying direction of hypothesis. Takes "two-sided", "greater", or "less".

# Output: A list including the t-test from the sample and the empirical p-value obtained using bootstraps or the permutation test.

hypothesis.test <- function(Group1, Group2, method, seed = NULL, samples, alternative = "two-sided"){
  # stop if incorrect sampling method is specified
  if (method != "bootstrap" & method != "permutation"){
    stop("Please specify the bootstrap or permutation as a resampling method.")
  }
  # set seed if specified
  if (!is.null(seed)){
    set.seed(seed)
  }
  # store sample t-value
  sample.t <- t.test(Group1, Group2)$statistic
  # create empty vector to store t-values in
  t.values <- numeric(samples)
  # N and M are group sizes 
  N <-length(Group1)
  M <-length(Group2)
  
  # resample if method is bootstrap
  if (method == "bootstrap"){
    for (i in 1:samples){
      # sample from original sample with replacement
      boot.sample <- sample(c(Group1,Group2), replace=TRUE)
      
      # randomly assign to group 1 with length N
      new.1 <- boot.sample[1:N]
      new.2 <- boot.sample[(N+1) : (N+M)]
      
      # compute test and add t values to vector
      # note that test is not performed if no variation in both groups
      if(length(unique(new.1))==1 & length(unique(new.2))==1) {
        t.values[i] <- NA
      } else {
          t.values[i]<-  t.test(new.1, new.2)$statistic
          }
    }
  }
  # if method is permutation test
  else{
    for (i in 1:samples)
    {
      # sample from original sample without replacement
      perm.sample <- sample(c(Group1,Group2), replace=FALSE)
      
      # random assignment to new groups. Group1 with length N and Group 2 with length M
      new.1 <- perm.sample[1:N]
      new.2 <- perm.sample[(N+1) : (N+M)]
      
      # add t-statistic to vector
      # also do not include if there is no variation in both groups
      if(length(unique(new.1))==1 & length(unique(new.2))==1) {
        t.values[i] <- NA
      } else {
          t.values[i]<-  t.test(new.1, new.2)$statistic
          }
    }
  }
  # compute empirical p-value based on type of hypothesis
  if(alternative == "two-sided") {p.emp <-  sum(abs(t.values) > abs(sample.t))/samples} else
    if(alternative == "greater") {p.emp <- sum(t.values>sample.t)/samples} else
      if(alternative == "less") {p.emp <- sum(t.values<sample.t)/samples}
  # create output
  output <- list(`t-statistic` = sample.t, # t-statistic in sample
                 `empirical p-value` = p.emp) # empirical p-value
  
  # return output
  return(output)
}

# enter example data
csfi <- c(2, 5, 5, 6, 6, 7, 8, 9)
tfi <- c(1, 1, 2, 3, 3, 4, 5, 7, 7, 8)

# run hypothesis.test function
hypothesis.test(Group1 = csfi, Group2 = tfi, method = "bootstrap", seed = 42, samples = 10000) # p = 0.108

# show if groups are switched
hypothesis.test(Group1 = tfi, Group2 = csfi, method = "bootstrap", seed = 42, samples = 10000) # p = 0.105

# run with permutation
hypothesis.test(Group1 = csfi, Group2 = tfi, method = "permutation", seed = 42, samples = 10000) # p = 0.106

# run without specifying correct method
hypothesis.test(Group1 = csfi, Group2 = tfi, method = "jackknife", seed = 42, samples = 10000) # shows error when not specifying bootstrap or permutation

# version without sample function
hypothesis.test.no_sample <- function(Group1, Group2, method, seed = NULL, samples, alternative = "two-sided"){
  # stop if incorrect sampling method is specified
  if (method != "bootstrap" & method != "permutation"){
    stop("Please specify the bootstrap or permutation as a resampling method.")
  }
  # set seed if specified
  if (!is.null(seed)){
    set.seed(seed)
  }
  # store sample t-value
  sample.t <- t.test(Group1, Group2)$statistic
  # create empty vector to store t-values in
  t.values <- numeric(samples)
  # N and M are group sizes 
  N <-length(Group1)
  M <-length(Group2)
  
  # resample if method is bootstrap
  if (method == "bootstrap"){
    for (i in 1:samples){
      # sample from original sample with replacement
      sampled.indices <- rdunif(n = N + M, b = N + M, a = 1) # select indices to sample
      boot.sample <- c(Group1, Group2)[sampled.indices] # sample datapoints
      
      # randomly assign to group 1 with length N
      new.1 <- boot.sample[1:N]
      new.2 <- boot.sample[(N+1) : (N+M)]
      
      # compute test and add t values to vector
      # note that test is not performed if no variation in both groups
      if(length(unique(new.1))==1 & length(unique(new.2))==1) {
        t.values[i] <- NA
      } else {
          t.values[i]<-  t.test(new.1, new.2)$statistic
          }
    }
  }
  # if method is permutation test
  else{
    for (i in 1:samples)
    {
      # shuffle original sample
      perm.sample <- shuffle(c(Group1,Group2))
      
      # random assignment to new groups. Group1 with length N and Group 2 with length M
      new.1 <- perm.sample[1:N]
      new.2 <- perm.sample[(N+1) : (N+M)]
      
      # add t-statistic to vector
      # also do not include if there is no variation in both groups
      if(length(unique(new.1))==1 & length(unique(new.2))==1) {
        t.values[i] <- NA
      } else {
          t.values[i]<-  t.test(new.1, new.2)$statistic
          }
    }
  }
  # compute empirical p-value based on type of hypothesis
  if(alternative == "two-sided") {p.emp <-  sum(abs(t.values) > abs(sample.t))/samples} else
    if(alternative == "greater") {p.emp <- sum(t.values>sample.t)/samples} else
      if(alternative == "less") {p.emp <- sum(t.values<sample.t)/samples}
  # create output
  output <- list(`t-statistic` = sample.t, # t-statistic in sample
                 `empirical p-value` = p.emp) # empirical p-value
  
  # return output
  return(output)
}

# test on example data
hypothesis.test.no_sample(Group1 = csfi, Group2 = tfi, method = "bootstrap", seed = 42, samples = 10000) # p = 0.102

## There is a small difference between p-values of both tests, since although same seeds are used, both functions use them differently and thus different samples are drawn in each function.

# 3.
# respampling.estimation estimates group means, mean differences, standard errors, confidence intervale, and bias based on the bootstrap or the jackknife.
#
# Group1: Vector with data from Group 1.
# Group2: Vector with data from Group2.
# method: Character string specifying the resampling method to use for the hypothesis test. Takes "bootstrap" of "jackknife".
# seed: Optional argument specifying the random seed to be used.
# samples: Whole number specifying the number of samples to take for resampling.
# CI.type: Character string specifying the type of Confidence Interval if the bootstrap is used. Takes "normal" or "percentile".

# Output: List of tibbles with mean and SE estimates for the sample and for the bootstrap/jackknife samples. If CI.type is specified, bootstrapped CI's are included. A tibble estimation the bias of mean estimates is also included.

resampling.estimation <- function(Group1, Group2, method, seed = NULL, samples, CI.type = NULL){
  # warning if method is not correctly specified and set to bootstrap.
  if (method != "bootstrap" & method != "jackknife"){
    method <- "bootstrap"
    warning("No correct resampling method was specified. The bootstrap will be used.")
  }
  # set seed if specified
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  # save means of groups
  mean_group1 <- mean(Group1)
  mean_group2 <- mean(Group2)
  
  # save standard errors of groups
  se.G1 <- sd(Group1)/sqrt(length(Group1))
  se.G2 <- sd(Group2)/sqrt(length(Group2))
  
  # perform t.test for SE and confidence interval
  test <- t.test(Group1, Group2)
  
  # extract SE of group difference
  se.difference <- test$stderr
  
  # extract lower and upper confidence interval bounds of difference
  conf.int.lower <- test$conf.int[1]
  conf.int.upper <- test$conf.int[2]
  
  
  # create empty vectors for means and mean differences
  means1 <- c()
  means2 <- c()
  mean_diff <- c()
  
  # run if bootstrap is selected as method
  if (method == "bootstrap"){
    # create bootstrap samples
    for (i in 1:samples){
      # create samples with replacement
      g1.sample <- sample(Group1, length(Group1), replace = TRUE)
      g2.sample <- sample(Group2, length(Group2), replace = TRUE)
      
      # append means and mean difference to vectors
      means1[i] <- mean(g1.sample)
      means2[i] <- mean(g2.sample)
      mean_diff[i] <- mean(g1.sample) - mean(g2.sample)
    }
  }
  # run if jackknife is specified as method
  if (method == "jackknife"){
    # make sure that number of samples does not exceed length of group
    if (samples > length(Group1) & samples > length(Group2)){
      samples <- min(length(Group1), length(Group2)) # change to size of smallest group
      warning("Number of samples specified exceeded the number of datapoints. Size of the smallest group is used as number of samples.")
    }
    # create jackknife samples
    for (i in 1:samples){
      # shuffle groups to ensure randomness
      Group1 <- shuffle(Group1)
      # create jackknife sample for group 1
      g1.sample <- Group1[-i]
      
      # shuffle groups to ensure randomness
      Group2 <- shuffle(Group2)
      # create jackknife sample for group 2
      g2.sample <- Group2[-i]
      
      # append means and mean difference to vectors
      means1[i] <- mean(g1.sample)
      means2[i] <- mean(g2.sample)
      mean_diff[i] <- mean(g1.sample) - mean(g2.sample)
    }
  }
  
  # empirical means and se's
  mean.G1.emp <- mean(means1)
  mean.G2.emp <- mean(means2)
  
  se.emp.G1 <- sd(means1)
  se.emp.G2 <- sd(means2)
  
  # estimate mean difference
  mean.difference.emp <- mean(mean_diff)
  se.difference.emp  <- sd(mean_diff)
  
  # estimate bias
  bias_group1 <-mean.G1.emp - mean_group1
  bias_group2 <- mean.G2.emp - mean_group2
  bias_difference <- mean.difference.emp - (mean_group1 - mean_group2)
  
  # output without estimated CI
  if (method == "jackknife" | is.null(CI.type)){
    output <- list(`Group Means` = tibble(Group1 = mean_group1,
                                          `SE Group 1` = se.G1,
                                           Group2 = mean_group2,
                                          `SE Group 2` = se.G2,
                                           Difference = (mean_group1 - mean_group2),
                                          `SE Difference` = se.difference,
                                          `CI Lower` = conf.int.lower,
                                          `CI Upper` = conf.int.upper),
                    `Resampling Means` = tibble(Group1 = mean.G1.emp,
                                               `SE Group1` = se.emp.G1,
                                               Group2 = mean.G2.emp,
                                               `SE Group2` = se.emp.G2,
                                               Difference = mean.difference.emp,
                                               `SE Difference` = se.difference.emp),
                    `Bias` = tibble(`Group 1 Mean` = bias_group1,
                                    `Group 2 Mean` = bias_group2,
                                    `Mean Difference` = bias_difference)
                    )
    # warning if CI.type is specified for jackknife
    if (!is.null(CI.type)){
      warning("A confidence interval type was selected but this is only available for the bootstrap. Not empirical confidence interval will be shown.")
    }
  }
  
  if (method == "bootstrap" & !is.null(CI.type)){
    # error if confidence interval type is correctly specified
    if (CI.type != "normal" & CI.type != "percentile"){
      stop("The confidence interval type specified is not available. Please specify normal or percentile.")
    }
    if (CI.type == "normal"){
      # specify upper and lower bounds of CI using SE
      width.CI <- 1.96*se.difference.emp
      CI.lower.emp <- (mean_group1 - mean_group2) - width.CI
      CI.upper.emp <- (mean_group1 - mean_group2) + width.CI
    }
    if (CI.type == "percentile"){
      # specify upper and lower bounds of CI using quantiles
      CI.lower.emp <- quantile(mean_diff, probs = 0.025)
      CI.upper.emp <- quantile(mean_diff, probs = 0.975)
    }
    # create output with bootstrapped estimates (including bootstraped Confidence Intervals)
    output <- list(`Group Means` = tibble(Group1 = mean_group1,
                                          `SE Group 1` = se.G1,
                                          Group2 = mean_group2,
                                          `SE Group 2` = se.G2,
                                          Difference = (mean_group1 - mean_group2),
                                          `SE Difference` = se.difference,
                                          `CI Lower` = conf.int.lower,
                                          `CI Upper` = conf.int.upper),
                   `Resampling Means` = tibble(Group1 = mean.G1.emp,
                                               `SE Group1` = se.emp.G1,
                                               Group2 = mean.G2.emp,
                                               `SE Group2` = se.emp.G2,
                                               Difference = mean.difference.emp,
                                               `SE Difference` = se.difference.emp,
                                               `CI Lower` = CI.lower.emp,
                                               `CI Upper` = CI.upper.emp),
                   `Bias` = tibble(`Group 1 Mean` = bias_group1,
                                   `Group 2 Mean` = bias_group2,
                                   `Mean Difference` = bias_difference)
    )
  }
  
  return(output)
}
# run function on example data with bootstrap
resampling.estimation(tfi, csfi, method = "bootstrap", seed = 42, samples = 10000, CI.type = "percentile")
resampling.estimation(tfi,csfi, method = "bootstrap", seed = 42, samples = 10000, CI.type = "normal")
# the percentile and the normal bootstrap have relatively similar confidence intervals.

# run with jackknife
resampling.estimation(tfi, csfi, method = "jackknife", seed = 42, samples = 10000) # warning message works.
# estimates differ quite a bit from those of the bootstrap. Especially estimates of bias are a lot more extreme than those in the bootstrap.

# use different seed for jackknife
resampling.estimation(tfi, csfi, method = "jackknife", seed = 24, samples = 10000)
# estimates are already quite a bit different. This may be due to the small sample size.



