library(palmerpenguins)
library(tidyverse)

data <- penguins %>%
  filter(species == "Adelie")

set.seed(756195) # set seed for reproducibility
take <- sample(1:nrow(data), 100) # determine the sample of 100
data <- data[take, ] # keep the sample of 100

m1 <- lm(body_mass_g ~ flipper_length_mm, data = data)

summary(m1)
