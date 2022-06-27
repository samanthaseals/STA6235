library(tidyverse)

y <- c(46.4, 48.2, 46.5, 46.4, 48.6)
x <- c(221, 221, 217, 216, 230)

one <- tibble(x, y)

mean(one$x)
sd(one$x)

mean(one$y)
sd(one$y)

cor.test(one$x, one$y, method="pearson")

