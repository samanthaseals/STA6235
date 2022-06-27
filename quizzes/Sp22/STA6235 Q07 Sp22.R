library(tidyverse)

y <- c(46.4, 48.2, 46.5, 46.4, 48.6)
x <- c(221, 221, 217, 216, 230)

one <- tibble(x, y)

m1 <- lm(y ~ x, data = one)

# find MSE
anova(m1)
