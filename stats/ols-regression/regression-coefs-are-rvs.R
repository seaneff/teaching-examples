#######################################################################
### Load required libraries ###########################################
#######################################################################

library(tidyverse)

#######################################################################
### Write function to generate random data ############################
#######################################################################

rA <- function(n, slope = 0){
  x       = runif(n, min = -1, max = 1)
  epsilon = runif(n, min = -.5, max = .5)
  y       = 0 + slope*x + epsilon
  return( data.frame(x = x, y = y) )
}

#######################################################################
### Visualize dataset: random point cloud #############################
#######################################################################

set.seed(432)
random_cloud <- rA(n = 200, slope = 0) 

random_cloud %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

## If we ran a regression through these points, 
## what do you think the coefficients would be?
## Why?










random_cloud_lm <- lm(y ~ x, data = random_cloud)
summary(random_cloud_lm)

#######################################################################
### Could we have obtained a different regression line with ###########
## a different random sample from the same population? ################
#######################################################################

## The following code draws a sample from distribution A, fits a regression line, 
## and plots it. Run it a few times to see what happens. Now explain how you would visually 
## estimate the standard error of the slope coefficient. Why is this standard error important?

## run this at least five times in a row
data <-  rA(10, slope = 0)

data %>% 
  ggplot() + 
  aes(x = x, y = y) + 
  geom_point() + 
  geom_smooth(method = 'lm', formula = 'y ~ x', se = FALSE) + 
  lims(
    x = c(-2,2), 
    y = c(-1,1)) + 
  labs(title = 'Regression Fit to Distribution A')

#######################################################################
### Could we have obtained a different regression line with ###########
## a different random sample from the same population? ################
#######################################################################

base_plot_a <- rA(n = 1000) %>%  
  ggplot() + 
  aes(x = x, y = y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3)) +
  theme_minimal()

base_plot_a

for(i in 1:100) { 
  base_plot_a <- base_plot_a + 
    rA(n = 1000) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm', se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey', alpha = 0.5,
      linewidth    = 0.5
    )
}

base_plot_a

## plot with standard errors
rA(n = 1000) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() + 
  stat_smooth(
    mapping = aes(x = x, y = y), 
    method  = 'lm', se = TRUE, 
    formula = 'y~x', fullrange = TRUE,
    color   = 'grey', alpha = 0.5,
    linewidth    = 0.5) +
  scale_x_continuous(limits = c(-3, 3)) 

#######################################################################
### Could we also have done this via bootstrap? #######################
#######################################################################

## create an empty data objects that we'll populate within our for loop
value <- NA


set.seed(0432)
for(boot in 1:1000){
  
  random_linearish <- rA(n = 200, slope = 0) 
  
  random_linearish_lm <- lm(y ~ x, data = random_linearish)
  value[boot] <- coef(random_linearish_lm)["x"]
  }
  
base_plot_a

ggplot() +
  aes(x = value) +
  geom_histogram(color = "black", fill = "light blue") +
  labs(x = "Value", 
       y = "Frequency",
       title = "Distribution of Coefficient for X",
       caption = "distribution based on 10,000 bootstrap iterations") +
  theme_minimal()

## The standard error or our beta coefficient (or any statistic), like is just the 
## is the standard deviation of its sampling distribution.
## We can prove this to ourselve by seeing that the standard error returned 
## by the lm() function is equivalent to the sd of our bootstrap samples

sd(value)
coef(summary(random_linearish_lm))[, "Std. Error"]["x"]
