#######################################################################
### Load required libraries ###########################################
#######################################################################

library(tidyverse)
library(patchwork)
library(sandwich)
library(wooldridge)

#######################################################################
### Write functions to generate random data ###########################
#######################################################################

rA <- function(n, slope = 0){
  x       = runif(n, min = -1, max = 1)
  epsilon = runif(n, min = -.5, max = .5)
  y       = 0 + slope*x + epsilon
  return( data.frame(x = x, y = y) )
}

rB <- function(n, slope = 0){
  x       = runif(n, min = -1, max = 1)
  epsilon = runif(n, min = - abs(x), max = abs(x))
  y       = 0 + slope*x + epsilon
  return( data.frame(x = x,y = y) )
}

rC <- function(n, slope = 0){
  x       = runif(n, min = -1, max = 1)
  epsilon = runif(n, min = -1 + abs(x), max = 1 - abs(x))
  y       = 0 + slope*x + epsilon
  return( data.frame(x = x, y = y) )
}

#######################################################################
### Visualize dataset: random point cloud #############################
#######################################################################

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
### Visualize dataset: way more linear ################################
#######################################################################

random_linearish <- rA(n = 200, slope = 10) 

random_linearish %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

random_linearish_lm <- lm(y ~ x, data = random_linearish)
summary(random_linearish_lm)

#######################################################################
### Could we also have done this via bootstrap? #######################
#######################################################################

value <- NA

set.seed(0432)
for(boot in 1:10000){
  
  random_linearish <- rA(n = 200, slope = 10) 
  
  random_linearish_lm <- lm(y ~ x, data = random_linearish)
  value[boot] <- coef(random_linearish_lm)["x"]
}


ggplot() +
  aes(x = value) +
  geom_histogram(color = "black", fill = "light blue") +
  labs(x = "Value", 
       y = "Frequency",
       title = "Distribution of Coefficient for X",
       caption = "distribution based on 10,000 bootstrap iterations") +
  theme_minimal()

sd(value)
coef(summary(random_linearish_lm))[, "Std. Error"]["x"]

#######################################################################
### Sample from A, fit a regression line, and plot it #################
#######################################################################

## The following code draws a sample from distribution A, fits a regression line, 
## and plots it. Run it a few times to see what happens. Now explain how you would visually 
## estimate the standard error of the slope coefficient. Why is this standard error important?

## run this at least five times in a row
data <-  rA(10, slope = 0)

data %>% 
  ggplot() + 
  aes(x=x, y=y) + 
  geom_point() + 
  geom_smooth(method = 'lm', formula = 'y ~ x', se = FALSE) + 
  lims(
    x = c(-2,2), 
    y = c(-1,1)) + 
  labs(title = 'Regression Fit to Distribution A')

#######################################################################
### Sample from A, fit a regression line, and plot it  (100x) #########
### 10 data points per draw ########################################### 
#######################################################################

base_plot_a <- rA(n = 10) %>%  
  ggplot() + 
  aes(x=x, y=y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3))

base_plot_a

for(i in 1:100) { 
  base_plot_a <- base_plot_a + 
    rA(n = 10) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm',         se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey',    alpha = 0.5,
      linewidth    = 0.5
    )
}

base_plot_a


#######################################################################
### Generate bigger dataset ###########################################
#######################################################################

set.seed(432)
data <- rbind( 
  data.frame( rA(200), label = 'A'),
  data.frame( rB(200), label = 'B'),
  data.frame( rC(200), label = 'C'))

#######################################################################
### Visualize dataset #################################################
#######################################################################

data %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  lims(
    x = c(-2,2), 
    y = c(-1,1)) + 
  labs(title = 'Samples Drawn from Three Distributions') + 
  facet_grid(rows = vars(label)) +
  theme_minimal()


#######################################################################
### Sample from A, fit a regression line, and plot it  (100x) #########
### 1,000 data points per draw ########################################
#######################################################################

base_plot_a <- rA(n = 1000) %>%  
  ggplot() + 
  aes(x=x, y=y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3))

base_plot_a

for(i in 1:100) { 
  base_plot_a <- base_plot_a + 
    rA(n = 1000) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm',         se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey',    alpha = 0.5,
      linewidth    = 0.5
    )
}

base_plot_a

base_plot_b <- rB(n = 1000) %>%  
  ggplot() + 
  aes(x=x, y=y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3))

base_plot_b

for(i in 1:100) { 
  base_plot_b <- base_plot_b + 
    rB(n = 1000) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm',         se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey',    alpha = 0.5,
      linewidth    = 0.5
    )
}

base_plot_b

base_plot_c <- rC(n = 1000) %>%  
  ggplot() + 
  aes(x=x, y=y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3))

base_plot_c

for(i in 1:100) { 
  base_plot_c <- base_plot_c + 
    rC(n = 1000) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm',         se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey',    alpha = 0.5,
      linewidth    = 0.5
    )
}

base_plot_c

base_plot_a + base_plot_b + base_plot_c  + patchwork::plot_layout(axes = "collect")

#######################################################################
### Thought experiment ################################################
#######################################################################

## You have a sample from each distribution, A, B, and C and you fit a regression of Y on X. 
## Which will have the highest standard error for the slope coefficient? 
## Which will have the lowest standard error? Why?

sample_a <- rA(n = 100)
summary(lm(y~x, data = sample_a))

sample_b <- rB(n = 100)
summary(lm(y~x, data = sample_b))

sample_c <- rC(n = 100)
summary(lm(y~x, data = sample_c))

#######################################################################
### Thought experiment ################################################
#######################################################################

# For distribution A, perform a simulated experiment. 
## Draw a large number of samples, and for each sample fit a linear regression. 
## Store the slope coefficient from each regression in a vector. Finally, compute the standard 
## deviation for the slope coefficients.

regression_outputs <- c()

for(i in 1:10000) { 
  sample <-  rA(n = 100)
  sample_model <- lm(y~x, data = sample)
  regression_outputs <- c(regression_outputs, sample_model$coefficients[2])
}

hist(regression_outputs)
sd(regression_outputs)
summary(lm(y~x, data = sample_b))$coefficients


#######################################################################
### Thought experiment ################################################
#######################################################################


model_1 <- lm(price ~ sqrft, data = hprice1)

## using robust standard errors
sandwich::vcovHC(model_1)
lmtest::coeftest(model_one, sandwich::vcovHC(model_1))


## boostrapping
square_foot_estimate <- NA

for(i in 1:1000){
  model_boot <- lm(price ~ sqrft,
                   data = slice_sample(hprice1, 
                                       n = 86,
                                       replace = TRUE))
  square_foot_estimate[i] <- coef(model_boot)["sqrft"]
}

sd(square_foot_estimate) ## sd 0.02
lmtest::coeftest(model_one, sandwich::vcovHC(model_1)) ## sd 0.02

## some data that are way outside of where all the other data are
## the boostrapped data that don't immediately fit in with the model
## leads to more variability in the esitmates when we boostrap
## then is getting picked up by the robust standard error estimator

## even though robust standard error estimator is being more conservative 
## than the default standard error, it's still being overly confident
## in producing its uncertainty estimate

## top-level inference remains the same
## under a weaker relationship, maybe this would be the difference

