#library(palmerpenguins)
#data <- penguins

library(tidyverse)

y <- c(46.4, 48.2, 46.5, 46.4, 48.6)
x <- c(221, 221, 217, 216, 230)

one <- tibble(x, y)

one_model <- lm(y ~ x, data = one)

summary(one_model)

# find predicted values
one$pred  <- predict(one_model)  

# find residual values
one$resid <- residuals(one_model)

# find sum for denominator of est. variance
means <- summarize(one, mean_x = mean(x, na.rm=TRUE), sd_x = sd(x, na.rm=TRUE), 
                   mean_y = mean(y, na.rm=TRUE), sd_y = sd(y, na.rm=TRUE))

# create variables for summations
one$dev_x = one$x - means$mean_x[1]
one$dev_y = one$y - means$mean_y[1]
one$dev_xy = one$dev_x*one$dev_y
one$dev_x2 = one$dev_x^2
one$dev_y2 = one$dev_y^2

# find sums for all columns of the tibble
sums <- summarise_at(one, which(sapply(one, is.numeric)), sum)  