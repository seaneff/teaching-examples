#######################################################################
### Load required libraries ###########################################
#######################################################################

library(tidyverse)
library(sandwich)
library(wooldridge)
library(broom)
library(lmtest) 
library(testthat)

#######################################################################
### Start with Boston Housing data ####################################
#######################################################################

summary(hprice1)

#######################################################################
### Fit a really simple model #########################################
#######################################################################

model_1 <- lm(price ~ sqrft, data = hprice1)
summary(model_1)

#######################################################################
### Check "default" standard errors ###################################
#######################################################################

coef(summary(model_1))[,"Std. Error"]["sqrft"]

#######################################################################
### Check assumptions #################################################
#######################################################################

## Reminder: do this BEFORE fitting your regression
hprice1 %>%
  ggplot(aes(x = sqrft, y = price*1000)) +
  geom_point() +
  labs(x = "Square Feet",
       y = "Home Price",
       title = "Housing Price vs. Home Size") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

#######################################################################
### What about robust random errors? ##################################
#######################################################################

coeftest(model_1, vcov. = sandwich::vcovHC(model_1))

coeftest(model_1, vcov. = sandwich::vcovHC(model_1))[,"Std. Error"]["sqrft"]

coef(summary(model_1))[,"Std. Error"]["sqrft"]

#######################################################################
### What about the bootstrap? ########################################
#######################################################################

beta_estimate <- NA

for(i in 1:1000){
  ## step 1: take a random sample (with replacement) from our data
  ## this happens in the slice_sample function
  bootstrap_sample <- slice_sample(hprice1,  n = nrow(hprice1), replace = TRUE)
  
  ## step 2: run a linear model (or calculate whatever statistic you care about)
  ## extract the info and save it
  model_boot <- lm(price ~ sqrft, data = bootstrap_sample)
  
  ## step 3: extract the info you care about and make sure you save it
  ## somewhere you can access later
  beta_estimate[i] <- coef(model_boot)["sqrft"]
}

sd(beta_estimate) 

#######################################################################
### Compare the three results #########################################
#######################################################################

## "default" standard error
coef(summary(model_1))[,"Std. Error"]["sqrft"]

## robust standard error
coeftest(model_1, vcov. = sandwich::vcovHC(model_1))[,"Std. Error"]["sqrft"]

## boostrap standard error
sd(beta_estimate)

#######################################################################
### Hey, we learned all about the distribution of beta ################
#######################################################################

ggplot(mapping = aes(beta_estimate)) + 
  geom_histogram(fill = "light blue", color = "black", bins = 30) +
  labs(x = "β estimate",
       y = "Frequency",
       title = "Distribution of β") +
  theme_minimal()
