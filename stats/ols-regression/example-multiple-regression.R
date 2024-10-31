#######################################################################
### Background ########################################################
#######################################################################

# Based on data from Wooldridge library: 
## "These are data from the 1976 Current Population Survey, collected by Henry Farber 
## when he and I were colleagues at MIT in 1988."

#######################################################################
### Load required libraries and fonts #################################
#######################################################################

## if you don't already have these downloaded, start by running:
## install.packages("tidyverse"), etc
library(tidyverse) ## to format/restructure/plot data
library(scales) ## for commas on the y axis
library(wooldridge) ## for datasets
library(lmtest) ## for diagnostic checking (wald test of nested models)
library(sandwich) ## for robust standard error

#######################################################################
### Check out our dataset #############################################
#######################################################################

?wage1

#######################################################################
### Basic data processing #############################################
#######################################################################

wage1 <- wage1 %>%
  mutate(female_string = case_when(
    female == 0 ~ "male",
    female == 1 ~ "female",
    .default = as.character(female)
  )) %>%
  mutate(married_string = case_when(
    married == 0 ~ "unmarried",
    married == 1 ~ "married",
    .default = as.character(married)
  ))
  
#######################################################################
### Thinking about transformations ####################################
#######################################################################

## Which of the following specifications best describe the relationship between experience and hourly wage? 

## Option 1: level-level
## Wage = beta0 + beta1*experience 
wage1 |>
  ggplot(aes(x = exper, y = wage)) +
  geom_point() +
  labs(x = "Experience (in years)",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal()

## Option 2: level-log
## Wage = beta0 + beta1*ln(experience)
wage1 |>
  ggplot(aes(x = log(exper, base = exp(1)), ## base = exp(1) = e is default
             y = wage)) +
  geom_point() +
  labs(x = "Natural log Experience (in ln(years))",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal()

## Option 3: log-level
## ln(wage) = beta0 + beta1*ln(experience)
wage1 |>
  ggplot(aes(x = exper, y = log(wage))) +
  geom_point() +
  labs(x = "Experience (in years)",
       y = "Natural Log of hourly wage\n(in ln(USD, 1976))",
       title = "Relationship between\nexperience and hourly wage") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal()

## Option 4: log-log
## ln(wage) = beta0 + beta1*ln(experience)
wage1 |>
  ggplot(aes(x = log(exper), y = log(wage))) +
  geom_point() +
  labs(x = "Natural Log of experience (in ln(years))",
       y = "Natural Log of hourly wage\n(in ln(USD, 1976))",
       title = "Relationship between\nexperience and hourly wage") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal()

## What is the interpretation of β0 and β1 in your selected specification?
## Can we use R2 or Adjusted R2 to choose between level-level or log-level specifications?

#######################################################################
### Applying polynomial terms #########################################
#######################################################################

## Simple model: level-level
## Wage = beta0 + beta1*experience 
model_1 <- lm(wage ~ exper, data = wage1)
summary(model_1)

wage1 |>
  ggplot(aes(x = exper, y = wage)) +
  geom_point() +
  labs(x = "Experience (in years)",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal() +
  stat_smooth(method = "lm", formula = y ~ x)


model_2 <- lm(wage ~ exper + I(exper^2), data = wage1)
summary(model_2)

wage1 |>
  ggplot(aes(x = exper, y = wage)) +
  geom_point() +
  labs(x = "Experience (in years)",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2))

waldtest(model_1, model_2, vcov = vcovHC(model_2, type = "HC0"))

#######################################################################
### Applying indicator variables  #####################################
#######################################################################

model_3 <- lm(wage ~ exper + I(exper^2) + female_string, data = wage1)
summary(model_3)

## what if we wanted to set a separate reference category?
is(wage1$female_string)
wage1$female_factor <- factor(wage1$female_string, levels = c("male", "female"))

model_3_factor <- lm(wage ~ exper + I(exper^2) + female_factor, data = wage1)
summary(model_3_factor)

## A bit more complicated: generate a grid of all combos of 
## input variables we want to plot
pred_grid <- expand.grid(
  exper = min(wage1$exper):max(wage1$exper), 
  female_factor = unique(wage1$female_factor)
)

# Then use the model to predict wages based on this grid
pred_grid$predicted_wage_model3 <- predict(model_3_factor, newdata = pred_grid)

wage1 |>
  ggplot(aes(x = exper, y = wage, color = female_factor)) + 
  geom_point(alpha = 0.8) +
  labs(x = "Experience (in years)",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage",
       color = "Gender") +
  geom_line(data = pred_grid, aes(x = exper, y = predicted_wage_model3, color = female_factor)) +
  theme_minimal()

#######################################################################
### Indicators with interactions, oh my! ##############################
#######################################################################

model_4 <- lm(wage ~ exper + female_factor + exper*female_factor, data = wage1)
summary(model_4)

# Remember this?
pred_grid$predicted_wage_model4 <- predict(model_4, newdata = pred_grid)

wage1 |>
  ggplot(aes(x = exper, y = wage, color = female_factor)) + 
  geom_point(alpha = 0.8) +
  labs(x = "Experience (in years)",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage",
       color = "Gender") +
  geom_line(data = pred_grid, aes(x = exper, y = predicted_wage_model4, color = female_factor)) +
  theme_minimal()

#######################################################################
### Understanding your model, graphically #############################
#######################################################################

wage1 |>
  ggplot(aes(x = exper, y = wage, color = female_factor)) + 
  geom_point(alpha = 0.8) +
  labs(x = "Experience (in years)",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage",
       color = "Gender") +
  theme_minimal() +
  facet_wrap(~married_string)

wage1 |>
  ## technically don't need color here
  ggplot(aes(x = exper, y = wage, color = female_factor, group = married_string)) + 
  geom_point(alpha = 0.8) +
  labs(x = "Experience (in years)",
       y = "Hourly wage (in USD, 1976)",
       title = "Relationship between\nexperience and hourly wage",
       color = "Gender") +
  theme_minimal() +
  facet_grid(vars(female_factor), vars(married_string))

