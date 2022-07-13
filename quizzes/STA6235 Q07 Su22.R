library(palmerpenguins)
library(tidyverse)

# function to create assumption assessment plots
# written by Reid Ginoza Fall 2019
almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

data <- penguins 

m1 <- lm(body_mass_g ~ flipper_length_mm, data = data)

almost_sas(m1)
