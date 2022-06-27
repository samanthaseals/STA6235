library(tidyverse)

y <- c(46.4, 48.2, 46.5, 46.4, 48.6)
x <- c(221, 221, 217, 216, 230)

one <- tibble(x, y)

one_model <- lm(y ~ x, data = one)
anova(one_model)

