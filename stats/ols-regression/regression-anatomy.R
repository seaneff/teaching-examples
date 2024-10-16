#####################################################
## Regression Anatomy ###############################
#####################################################

## See great explanation in Causal Inference
## the Mixtape
## https://mixtape.scunning.com/02-probability_and_regression

#####################################################
## Load required libraries ##########################
#####################################################

library(ggplot2) ## for graphics
library(broom) ## for augment function

#####################################################
## Simulate data ####################################
#####################################################

set.seed(532)
d <- data.frame(
  x1 = runif(n = 100, min = 0, max = 10), 
  x2 = runif(n = 100, min = 0, max = 10), 
  x3 = runif(n = 100, min = 0, max = 10)
)

## because we know the population model, we can produce a single sample from it 
## using the following code: 

d <- d %>% 
  mutate(y = -3 + 1*x1 + 2*x2 + 3*x3 + rnorm(n=n(), mean=0, sd=1))

head(d)

#####################################################
## Fit first model ##################################
#####################################################

## initial form of model
model_main <- lm(y ~ x1 + x2 + x3, data = d)
coef(model_main)

#####################################################
## Fit first auxillary model ########################
#####################################################

## fit model predicting x1 from other xi values
model_aux <- lm(x1 ~ x2 + x3, data = d)
coef(model_aux)

## extract residuals:
## the unique variation of x1 -- the part you can't explain with the other Xs
d_augmented <- augment(model_aux)
d_augmented$y <- d$y
head(d_augmented)

#####################################################
## Fit second auxillary model #######################
#####################################################

model_two <- lm(y ~ .resid, data = d_augmented)
coef(model_two)

#####################################################
## Fit second auxillary model #######################
#####################################################

coef(model_main) ## B_1 = 0.9614521
coef(model_two) ## B_1 = 0.9614521

## coefficient of the residuals in the second auxillary model
## is equivalent to the coefficient of x1 in our first model
## wow

## and also equal! 0.9614521
## the math maths!
cov(d_augmented$y, d_augmented$.resid)/var(d_augmented$.resid)

## why is this not equal to 1 (set in generating the data?)
summary(model_main)
