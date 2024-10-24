#######################################################################
### Load required libraries ###########################################
#######################################################################

library(tidyverse)
library(patchwork)
library(lmtest) 

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

## which are homoskedastic?
## which are heterokedastic?
## bonus: how does this impact how we would calculate/report SEs for inference?

data %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  lims(
    x = c(-2,2), 
    y = c(-1,1)) + 
  labs(title = 'Samples Drawn from Three Distributions') + 
  facet_grid(rows = vars(label)) +
  theme_minimal()


## You have a sample from each distribution, A, B, and C and you fit a regression of Y on X. 
## Which will have the highest standard error for the slope coefficient? 
## Which will have the lowest standard error? Why?

#######################################################################
### Visualize results #################################################
#######################################################################

base_plot_a <- rA(10) %>%  
  ggplot() + 
  aes(x = x, y = y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-.3, .3))


for(i in 1:100) { 
  base_plot_a <- base_plot_a + rA(200) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm', se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey', alpha = 0.5,
      size    = 0.5
    )
}

base_plot_b <- rB(10) %>%  
  ggplot() + 
  aes(x = x, y = y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-.3, .3))


for(i in 1:100) { 
  base_plot_b <- base_plot_b + rB(200) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm',         se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey',    alpha = 0.5,
      size    = 0.5
    )
}

base_plot_c <- rC(10) %>%  
  ggplot() + 
  aes(x = x, y = y) + 
  geom_point() + 
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-.3, .3))


for(i in 1:100) { 
  base_plot_c <- base_plot_c + rC(200) %>% 
    stat_smooth(
      mapping = aes(x = x, y = y), 
      method  = 'lm',         se = FALSE, 
      formula = 'y~x', fullrange = TRUE,
      color   = 'grey',    alpha = 0.5,
      size    = 0.5
    )
}

base_plot_a + base_plot_b + base_plot_c + patchwork::plot_layout(axes = "collect")

#######################################################################
### Model results #####################################################
#######################################################################

sample_a <- rA(n = 200)
lm_a <- lm(y~x, data = sample_a)

sample_b <- rB(n = 200)
lm_b <- lm(y~x, data = sample_b)

sample_c <- rC(n = 200)
lm_c <- lm(y~x, data = sample_c)

coef(summary(lm_a))[, "Std. Error"]["x"]
coef(summary(lm_b))[, "Std. Error"]["x"]
coef(summary(lm_c))[, "Std. Error"]["x"]

#########################################################################
### What if we looked at robust standard errors (especially for B, C) ###
#########################################################################

## For A, pretty similar
coef(summary(lm_a))[, "Std. Error"]["x"]
coeftest(lm_a, vcov. = sandwich::vcovHC(lm_a))[,"Std. Error"]["x"]

## For B, somewhat different
coef(summary(lm_b))[, "Std. Error"]["x"]
coeftest(lm_b, vcov. = sandwich::vcovHC(lm_b))[,"Std. Error"]["x"]

## For C, very different
coef(summary(lm_c))[, "Std. Error"]["x"]
coeftest(lm_c, vcov. = sandwich::vcovHC(lm_c))[,"Std. Error"]["x"]

