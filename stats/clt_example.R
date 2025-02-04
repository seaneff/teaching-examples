#######################################################################
### Load required libraries ###########################################
#######################################################################

library(ggplot2)

#######################################################################
### Example: mean of an random normal variable ########################
#######################################################################

## set a seed, so it's replicable
set.seed(123)  

## specify some information
## n: sample size (for each sample)
## num_samples: the total number of samples
## mean: mean of distribution
## sd: sd of normal distribution

n <- 100
num_samples <- 5 ## 10, 30, 300, 3000?
mean <- 10
sd <- 3

## generate a random sample for the sample size and lambda you specified above
rnorm(n, mean = mean, sd = sd)

## calculate the mean of that random sample
mean(rnorm(n, mean = mean, sd = sd))

## do it a bunch of times, save it as an object
?replicate
sample_means_norm <- replicate(num_samples, mean(rnorm(n, mean = mean, sd = sd)))

# make a plot
## reminder: mean of an exponential distribution is 1/lambda
ggplot(data.frame(sample_means_norm), aes(x = sample_means_norm)) +
  geom_histogram(fill = "#005879", color = "black") +
  labs(title = "Distribution of Sample Means", x = "Sample Mean", y = "Frequency") +
  theme_minimal()


#######################################################################
### Example: mean of an random exponential variable ###################
#######################################################################

## set a seed, so it's replicable
set.seed(123)  

## specify some information
## n: sample size (for each sample)
## num_samples: the total number of samples
## lambda: rate parameter for exponential distribution 

n <- 5
num_samples <- 30
lambda <- 5

## generate a random sample for the sample size and lambda you specified above
rexp(n, rate = lambda)

## calculate the mean of that random sample
mean(rexp(n, rate = lambda))

## do it a bunch of times, save it as an object
sample_means_exp <- replicate(num_samples, mean(rexp(n, rate = lambda)))

# make a plot
## reminder: mean of an exponential distribution is 1/lambda
ggplot(data.frame(sample_means_exp), aes(x = sample_means_exp)) +
  geom_histogram(fill = "#005879", color = "black") +
  labs(title = "Distribution of Sample Means", x = "Sample Mean", y = "Frequency") +
  theme_minimal()

