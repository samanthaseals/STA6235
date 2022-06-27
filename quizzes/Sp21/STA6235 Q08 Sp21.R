library(palmerpenguins)
library(tidyverse)

one <- penguins

one <- one %>% drop_na()

test <- filter(one, species == "Gentoo")
mean(test$flipper_length_mm, na.rm = TRUE)

one_model <- lm(bill_length_mm ~ flipper_length_mm + as.factor(sex) + as.factor(species), data = penguins)
summary(one_model)
anova(one_model)
