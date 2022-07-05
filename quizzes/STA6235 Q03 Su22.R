library(tidyverse)

body <- c(3850, 3775, 3700, 4050, 4300, 3950)
flip <- c(195, 198, 191, 203, 201, 198)

one <- tibble(body, flip)

m1 <- lm(body ~ flip, data = one)

summary(m1)

confint(m1, level = 0.9)

anova(m1)
