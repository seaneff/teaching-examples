#####################################################
## References #######################################
#####################################################

## Some code based on lab from Elements of Statistical Learning
## https://hastie.su.domains/ISLR2/Labs/Rmarkdown_Notebooks/Ch3-linreg-lab.html

## Some code based from Cal 203 live session notes 

#####################################################
## Load required libraries ##########################
#####################################################

library(ggplot2) ## for graphics
library(MASS) ## large collection of datasets and functions
library(ISLR2) ## for datasets: https://hastie.su.domains/ISLR2/Labs/Rmarkdown_Notebooks/Ch3-linreg-lab.html
library(wooldridge) ## for datasets: https://rdrr.io/cran/wooldridge/man/hprice1.html
library(broom) ## for augment function
library(scales) ## to get commas on y axis

#####################################################
## Housing prices ###################################
#####################################################

## hprice dataset
head(hprice1)
View(hprice1)

#####################################################
## Explore dataset  #################################
#####################################################

## for data dictionary
?hprice1

## basic summary stats for each field/column
summary(hprice1)

## Are there variables that would not be valid outcomes for an OLS regression? If so, why?
## The colonial variable would not be suitable for OLS given that it is a binary variable. 
## Logistic regression would be an appropriate alternative in this case.
## Some of the variables show skewness, but this isn't a problem for the large sample model.  
## (Except in the rare case that you have such heavy tails that the BLP doesn't exist)

##  Are there variables that would _not_ be valid inputs for an OLS regression? If so, why? 
## 4 of the variables are just log transformations of the original variable. 
## It would be inappropriate for one of these variables to be included as an input variable if its counterpart 
## was included as the outcome variable.

#####################################################
## Assess assumptions of a larger-sample ############
## OLS model? #######################################
#####################################################

## IID 
## think about: geographic clustering, strategic interactions/competition?

## A unique BLP exists
## think about: no perfect collinearity, relevant matrix inversion possible

#####################################################
## Explore relationships with plots #################
#####################################################

hprice1 %>% 
  ggplot() + 
  aes(x = sqrft, y = price) + 
  geom_point() +
  labs(y = "Home price (USD)",
       x = "Home square footsage",
       title = "Home price vs. square footage") 

#####################################################
## Calculate correlation between price/sqrft ########
#####################################################

## base r
## person is default method, but good to be explicit here
cor(hprice1$price, hprice1$sqrft, method = "pearson")

## with pipes
## person is default method, but good to be explicit here
hprice1 %>% 
  summarise(
    calculated_cor = cor(sqrft, price, method = "pearson")
  )

#####################################################
## Calculate OLS regression line "by hand" ##########
#####################################################

cov(hprice1$sqrft, hprice1$price)/var(hprice1$sqrft)

hprice1 %>% 
  summarize(
    'By Hand Regression' = cov(sqrft, price) / var(sqrft)
  )

#####################################################
## Run regression ###################################
#####################################################

## Regress price on sqrft using the lm function. 
lm.fit <- lm(price ~ sqrft, data = hprice1)
summary(lm.fit)

## Is the coefficient the same as what we calculated "by hand"?

#####################################################
## Create a scatterplot that shows regression #######
#####################################################

hprice1 %>% 
  ggplot() + 
  aes(y = sqrft, x =  price) + 
  geom_point() +
  labs(y = "Home price (USD)",
       x = "Home square footsage",
       title = "Home price vs. square footage") +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$"))

#####################################################
## Assess residuals #################################
#####################################################

## extract residuals
hprice1_augmented <- cbind.data.frame(augment(lm.fit), 
                                      colonial = hprice1$colonial)
head(hprice1_augmented)

## what do the residuals sum to?
## what do you expect?
sum(hprice1_augmented$.resid)
summary(hprice1_augmented$.resid)

## plot residuals, histogram
hprice1_augmented %>% 
  ggplot() + 
  aes(x = .resid) + 
  geom_histogram(color = "black", fill = "#605678", bins = 20) +
  labs(y = "Frequency",
       x = "Residuals",
       title = "Distribution of residuals") 

## plot residuals, scatterplot
## residuals vs. fitted values
hprice1_augmented %>% 
  ggplot() + 
  aes(y = .resid, x = .fitted) + 
  geom_point(color = "#605678") +
  labs(y = "Residuals",
       x = "Fitted Values",
       title = "Residuals vs. Fitted") 

## plot residuals, scatterplot
## residuals vs. fitted values
## explore impact of another features not in the model
hprice1_augmented %>% 
  ggplot() + 
  aes(y = .resid, x = .fitted, color = factor(colonial == 1)) + 
  geom_point() +
  labs(y = "Residuals",
       x = "Fitted Values",
       color = "Colonial?",
       title = "Residuals vs. Fitted") 


#################################################################
## Add additional coefficients ##################################
#################################################################

## Estimate a new model (and save it into another object) that includes the size of the lot and whether the house is a colonial.
## BEFORE YOU DO, make a prediction: What do you think is going to happen to the coefficient that relates square footage and price?
## Will the coefficient increase, decrease, or stay the same? Why?
  
summary(lm(price ~ sqrft, data = hprice1))
summary(lm(price ~ sqrft + lotsize + colonial, data = hprice1))


#################################################################
## Explore a second dataset (from ISLR2 package) ################
#################################################################

## The ISLR2 library contains the Boston data set, which records medv (median house value) for 
## 506 census tracts in Boston. We will seek to predict medv using 12 predictors such as rmvar 
## (average number of rooms per house), age (proportion of owner-occupied units built prior to 1940) 
## and lstat (percent of households with low socioeconomic status).

head(Boston)

## make a plot
Boston %>% 
  ggplot() + 
  aes(y = medv, x =  lstat/100) + 
  geom_point() +
  labs(y = "Median house value (USD)",
       x = "% households with low socioeconomic status",
       title = "Socioeconomic status and home values") +
  scale_x_continuous(labels = scales::percent)

#####################################################
## Assess the assumptions of a larger sample ######## 
## linear model #####################################
#####################################################

## IID 
## think about: geographic clustering, strategic interactions/competition?

## A unique BLP exists
## think about: no perfect collinearity, relevant matrix inversion possible

#####################################################
## Fit basic univariate linear model ################
#####################################################

## We will start by using the lm() function to fit a simple linear regression model, 
## with medv as the response and lstat as the predictor. The basic syntax is lm(y ~ x, data), 
## where y is the response, x is the predictor, and data is the data set in which these two variables are kept.

lm.fit.2 <- lm(medv ~ lstat, data = Boston)

## print out full model object
lm.fit.2

## check out the data type
is(lm.fit.2)

## look under the hood at what the object contains
names(lm.fit.2)

## extract just the coefficients
coef(lm.fit.2)

## extract just the residuals
residuals(lm.fit.2)

#####################################################
## Visualize model ##################################
#####################################################

Boston %>% 
  ggplot() + 
  aes(y = medv, x =  lstat/100) + 
  geom_point() +
  labs(y = "Median house value (USD)",
       x = "% households with low socioeconomic status",
       title = "Socioeconomic status and home values") +
  scale_x_continuous(labels = scales::percent) +
  geom_smooth(method = 'lm')

#####################################################
## Explore residuals ################################
#####################################################

## extract residuals
boston_augmented <- augment(lm.fit.2)
head(boston_augmented)

## what do you think residuals will sum to?
sum(boston_augmented$.resid)

## plot residuals, basic histogram
boston_augmented %>% 
  ggplot() + 
  aes(x = .resid) + 
  geom_histogram(fill = "#605678", color = "black") +
  labs(y = "Frequency",
       x = "Model residual",
       title = "Distribution of Residuals") 

## plot residuals, scatterplot
boston_augmented %>% 
  ggplot() + 
  aes(y = .resid, x = .fitted) + 
  geom_point(color = "#605678") +
  labs(y = "Residuals",
       x = "Fitted Values",
       title = "Residuals vs. Fitted") 

par(mfrow = c(2, 2))
plot(lm.fit)

#####################################################
## What if we add a quadratic term? #################
#####################################################

Boston %>% 
  ggplot() + 
  aes(y = medv, x =  lstat/100) + 
  geom_point() +
  labs(y = "Median house value (USD)",
       x = "% households with low socioeconomic status",
       title = "Socioeconomic status and home values") +
  scale_x_continuous(labels = scales::percent) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2))

## How do you think that residuals would look?
## What are the risks of adding additional terms?
## What are the benefits?
