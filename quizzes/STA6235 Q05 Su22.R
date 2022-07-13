library(palmerpenguins)
library(tidyverse)

data <- penguins %>%
  filter(species == "Chinstrap")

m1 <- lm(body_mass_g ~ flipper_length_mm, data = data)

summary(m1)
