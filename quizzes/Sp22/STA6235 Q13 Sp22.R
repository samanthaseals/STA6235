library(tidyverse)
library(palmerpenguins)
library(fastDummies)

data <- na.omit(penguins)
data <- dummy_cols(data, select_columns = c("sex", "species"))

m <- lm(bill_length_mm ~ flipper_length_mm*species, data = data)
summary(m)
c <- coefficients(m)

full <- lm(bill_length_mm ~ flipper_length_mm*species, data = data)
reduced <- lm(bill_length_mm ~ flipper_length_mm + species, data = data)

anova(reduced, full)


m <- lm(bill_length_mm ~ flipper_length_mm + species_Adelie + species_Chinstrap + 
          flipper_length_mm:species_Adelie + flipper_length_mm:species_Chinstrap, data = data)
summary(m)

m <- lm(bill_length_mm ~ flipper_length_mm + species_Gentoo + species_Chinstrap + 
          flipper_length_mm:species_Gentoo + flipper_length_mm:species_Chinstrap, data = data)
summary(m)

data <- data %>% 
  mutate(y_adelie = c["(Intercept)"] + c["flipper_length_mm"]*flipper_length_mm + 
  c["speciesChinstrap"]*0 + c["speciesGentoo"]*0 + 
  c["flipper_length_mm:speciesChinstrap"]*0*flipper_length_mm +
  c["flipper_length_mm:speciesGentoo"]*0*flipper_length_mm,
  y_chinstrap = c["(Intercept)"] + c["flipper_length_mm"]*flipper_length_mm + 
    c["speciesChinstrap"]*1 + c["speciesGentoo"]*0 + 
    c["flipper_length_mm:speciesChinstrap"]*1*flipper_length_mm +
    c["flipper_length_mm:speciesGentoo"]*0*flipper_length_mm,
  y_gentoo = c["(Intercept)"] + c["flipper_length_mm"]*flipper_length_mm + 
    c["speciesChinstrap"]*0 + c["speciesGentoo"]*1 + 
    c["flipper_length_mm:speciesChinstrap"]*0*flipper_length_mm +
    c["flipper_length_mm:speciesGentoo"]*1*flipper_length_mm
  )

data %>% ggplot(aes(x = flipper_length_mm, y = bill_length_mm)) + 
  geom_point(aes(color = species)) +
  geom_line(aes(y = y_gentoo), color = "#619CFF", size = 1) +
  geom_line(aes(y = y_chinstrap), color = "#00BA38", size = 1) +
  geom_line(aes(y = y_adelie), color = "#F8766D", size = 1) + 
  theme_minimal() +
  ylab("Bill Length (mm)") +
  xlab("Flipper Length (mm)") 
