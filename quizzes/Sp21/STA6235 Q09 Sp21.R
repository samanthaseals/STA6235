library(palmerpenguins)
library(tidyverse)
library(fastDummies)

one <- penguins

one <- one %>% drop_na()
one <- dummy_cols(one, select_columns = c("species", "sex"))

anova(lm(bill_length_mm ~ flipper_length_mm + sex_male + species_Chinstrap + species_Gentoo, data = one))
anova(lm(bill_length_mm ~ flipper_length_mm + sex_male + species_Chinstrap, data = one))
anova(lm(bill_length_mm ~ flipper_length_mm + sex_male, data = one))
anova(lm(bill_length_mm ~ flipper_length_mm, data = one))
