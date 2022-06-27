library(palmerpenguins)
library(tidyverse)

one <- penguins
one <- filter(one, species == "Adelie")

one$x <- one$flipper_length_mm
one$y <- one$bill_length_mm

one <- select(one, x, y)

one <- one %>% drop_na()

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

min(one$x) max(one$x)

one_model <- lm(y ~ x, data = one)
summary(one_model)
anova(one_model)
