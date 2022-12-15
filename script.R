# 1. Function for hypothesis testing
hypothesis.test <- function(Group1, Group2, method = NULL, seed = NULL, samples = NULL, alternative = "two-sided"){
  # stop if resampling method should be specifies
  if (is.null(method)){
    stop("Please specify a resampling method")
  }
  # stop if incorrect sampling method is specified
  if (method != "bootstrap" & method != "permutation"){
    stop("Please specify the bootstrap or permutation as a resampling method.")
  }
  # set seed if specified
  if (!is.na(seed)){
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
      boot.sample <-sample(c(Group1,Group2), replace=TRUE)
      
      # randomly assign to group 1 with length N
      new.1 <- boot.sample[1:N]
      new.2 <- boot.sample[(N+1) : (N+M)]
      
      # compute test and add t values to vector
      # note that test is not performed if no variation in both groups
      if(length(unique(new.1))==1 & length(unique(new.2))==1) {
        t.values[i] <- NA} else {t.values[i]<-  t.test(new.1, new.2)$statistic}
    }
  }
  # if method is permutation test
  else{
    for (i in 1:samples)
    {
      # sample from original sample without replacement
      perm.sample <-sample(c(Group1,Group2), replace=FALSE)
      
      # random assignment to new groups. Group1 with length N and Group 2 with length M
      new.1 <- perm.sample[1:N]
      new.2 <- perm.sample[(N+1) : (N+M)]
      
      # add t-statistic to vector
      # also do not include if there is no variation in both groups
      if(length(unique(new.1))==1 & length(unique(new.2))==1) {
        t.values[i] <- NA} else {t.values[i]<-  t.test(new.1, new.2)$statistic}
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

# 3. Function for estimation

# create function
estimate <- function(data, method = NULL, seed = NULL, samples = NULL){
  # stop if no available method is specified
  if (is.null(method)){
    stop("Please specify a resampling method.")
  }
  if (method != "bootstrap" & method != "jackknife"){
    stop("Please specify the bootstrap or the jackknife as resampling method.")
  }
  # stop if number of samples is not a whole number
  if (!is.numeric(samples) | samples != round(samples)){
    stop("Please provide a whole number as the number of samples to be drawn.")
  }
  # warn 
  if (method == "jackknife" & samples > nrow(data)){
    samples <- nrow(data)
    warning("Number of samples exceeded the number of datapoints. The total number of datapoints are used.")
  }
  # set seed if specified
  if (!is.null(seed)){
    set.seed(seed)
  }
  means <- numeric(samples)
  if (method == "bootstrap"){
    
  }
}
