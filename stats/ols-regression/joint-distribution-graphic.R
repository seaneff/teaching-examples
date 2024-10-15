#####################################################
## Load required libraries ##########################
#####################################################

library(ggplot2) ## for graphics
library(MASS) ## to generate bivariate normal data

#####################################################
## Simulate bivariate normal data ###################
#####################################################

set.seed(00001)
data <- as.data.frame(mvrnorm(n = 1000, ### 1,000 observations
                              mu =  c(5, 5), ## means of x and y
                              Sigma =  matrix(c(1, 0.7, 0.7, 1), 2)))  ## covariance matrix

names(data) <- c("X", "Y")

#####################################################
## Make contour plot, plot regression line  #########
#####################################################

p <- ggplot(data, aes(x = X, y = Y)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", color = "black") +
  scale_fill_viridis_c() +  # for a nice color scale
  theme_minimal(); p

# add a regression line
p <- p + geom_smooth(method = "lm", color = "red", se = FALSE); p

