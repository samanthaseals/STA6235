library(palmerpenguins)
library(tidyverse)

data <- penguins %>%
  filter(species == "Chinstrap")

set.seed(1671895) # set seed for reproducibility
take <- sample(1:nrow(data), 50) # determine the sample of 100
data <- data[take, ] # keep the sample of 100

m1 <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm + sex, data = data)

m2 <- lm(body_mass_g ~ flipper_length_mm, data = data)

summary(m1)

anova(m2, m1)

anova(m2)
