library(palmerpenguins)
library(tidyverse)

data <- penguins %>%
  filter(species == "Chinstrap")

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

m1 <- lm(body_mass_g ~ flipper_length_mm, data = data)

almost_sas(m1)
