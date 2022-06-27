library(palmerpenguins)
library(tidyverse)
library(fastDummies)

one <- penguins

one <- one %>% drop_na()
one <- dummy_cols(one, select_columns = c("sex"))

full <- lm(bill_length_mm ~ flipper_length_mm + sex_male + flipper_length_mm:sex_male, data = one)
reduced <- lm(bill_length_mm ~ flipper_length_mm, data = one)

summary(full)

anova(reduced, full)
