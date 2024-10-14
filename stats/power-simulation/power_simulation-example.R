#####################################################
## Load required libraries ##########################
#####################################################

library(dplyr)
library(ggplot2)
library(pwr)

#####################################################
## Define function to create synthetic data #########
#####################################################

create_sample <- function(n, mu_a, mu_b, sigma_a, sigma_b) {
  
  ## create vector called group with n values of A followed by n values of B
  ## representing group membership
  group <- c(rep("A", n), rep("B", n))
  
  ## based on thish group, generate a random outcome variable from 
  ## a draw from a population with a specified mean and standard deviation
  outcome <- ifelse(group == "A", 
                    rnorm(sum(group == "A"), mean = mu_a, sd = sigma_a), 
                    rnorm(sum(group == "B"), mean = mu_b, sd = sigma_b))
  
  ## put the info together in a dataframe and return it from the function
  output <- data.frame(group = group, outcome = outcome)
  
  return(output)
}

## show how it works and make a simple plot
create_sample(n = 1000, mu_a = 5, mu_b = 10, sigma_a = 2, sigma_b = 2) %>%
  ggplot(aes(x = outcome, group = group, fill = group)) +
  geom_density(alpha = 0.3) +
  labs(x = "Outcome", y = "Density", title = "Distribution of Simulated Sample Data")

#####################################################
## Write function to run a t-test on your data  #####
#####################################################

run_test <- function(data) {
  test_result <- t.test(outcome ~ group, data = data)
  return(as.integer(test_result$p.value < 0.05))
}

## show how it works, run it once
run_test(create_sample(n = 100, mu_a = 5, mu_b = 6, sigma_a = 2, sigma_b = 2))

#####################################################
## Do it a bunch of times  ##########################
#####################################################

# function to calculate the percentage of significant p-values
calculate_significant_proportion <- function(sample_sizes, mu_diff, sigma_a, sigma_b, num_samples = 1000) {
  
  ## create data frame with all possible combinations of per-group 
  ## sample sizes (n) and differences in the true population mean (mu_diff)
  results <- expand.grid(n = sample_sizes, mu_diff = mu_diff)
  results$percent_significant <- NA
  
  ## for each of the combinations of sample sizes (n) and differences in the true population mean (mu_diff)
  ## extract information on the sample size (per group), and the population expected values
  ## hold mu_a constant at 5 and vary mu_b based on the value of mu_diff set as an input parameter
  for(i in 1:nrow(results)) {

    n <- results$n[i]
    mu_a <- 5  # we keep mu_a fixed at 5 for comparison
    mu_b <- mu_a + results$mu_diff[i]
    
    ## print out the step we're on
    print(paste("Processing: Sample size = ", n, ", Effect size = ", results$mu_diff[i], sep = ""))
    
    ## count the number of significant t-tests across all samples
    significant_count <- 0
    
    for(j in 1:num_samples) {
      sample_data <- create_sample(n = n, mu_a = mu_a, mu_b = mu_b, sigma_a = sigma_a, sigma_b = sigma_b)
      significant_count <- significant_count + run_test(sample_data)
    }
    
    # calculate the percentage of significant p-values
    results$percent_significant[i] <- (significant_count / num_samples) 
  }
  
  ## return the results table
  return(results)
}

# define the parameters to test
sample_sizes <- c(30, 50, 100, 200, 500)          # Varying sample sizes (including both group A and group B)
mu_diff <- seq(0.1, 5, by = 0.1)                  # Varying differences in the population mean (mu_b - mu_a)
sigma_a <- 2                                      # Standard deviation for group A
sigma_b <- 2                                      # Standard deviation for group B

## run the simulation
## this is a bit slow
simulation_results <- calculate_significant_proportion(sample_sizes, mu_diff, sigma_a, sigma_b)

# plot the results
simulation_results %>%
  ggplot(aes(x = mu_diff, y = percent_significant, group = n, color = factor(n))) +
  geom_line() +
  labs(x = "True difference in population mean",
       y = "Power\n(% hypothesis tests that reject the null)",
       title = "Power vs. Sample Size (per group)",
       color = "Sample size") +
  scale_y_continuous(labels = percent) +
  scale_color_brewer(palette = "Blues") +
  theme_minimal()

#####################################################
## Compare values of our simulation function and ####
## the pwr.t.test function ##########################
#####################################################

mu_diff_example <- 0.5
n_example <- 200
pooled_sd <- 2 ## used above in simulation, carried through below

## display the results of our simulation
simulation_results[which(simulation_results$mu_diff == mu_diff_example & 
                         simulation_results$n       == n_example),]

## use the pwr.t.test to check the same thing
## from ?pwr.t.test we see that d is Cohen's d, equal to difference between the means (mu_diff) divided by pooled standard deviation
pwr.t.test(d = mu_diff_example/pooled_sd,
           n = n_example, 
           type = "two.sample", 
           alternative = "two.sided")


