library(tidyverse)

y <- c(46.4, 48.2, 46.5, 46.4, 48.6)
x <- c(221, 221, 217, 216, 230)

one <- tibble(x, y)

m1 <- lm(y ~ x, data = one)

# find MSE
anova(m1)

# pieces for s^2{b_0} and s^2{b_1}
mean(x)
mean(y)
sd(x)
sd(y)

one <- one %>% 
  mutate(sqdev_x = (x-mean(x))^2,
         sqdev_y = (y-mean(y))^2)

sum(one$sqdev_x)
sum(one$sqdev_y)


  