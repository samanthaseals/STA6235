library(palmerpenguins)
library(tidyverse)

data <- penguins %>%
  filter(species == "Chinstrap")

quiz <- sample_n(data, 6) 

body <- c(3850, 3775, 3700, 4050, 4300, 3950)
flip <- c(195, 198, 191, 203, 201, 198)

one <- tibble(body, flip)

m1 <- lm(body ~ flip, data = one)

summary(m1)

# find sum for denominator of est. variance
means <- summarize(one, mean_body = mean(body, na.rm=TRUE), sd_body = sd(body, na.rm=TRUE), 
                        mean_flip = mean(flip, na.rm=TRUE), sd_flip = sd(flip, na.rm=TRUE))

# create variables for summations
one <- one %>%
  mutate(dev_body = body - means$mean_body[1],
         dev_flip = flip - means$mean_flip[1]) %>%
  mutate(dev_xy = dev_body*dev_flip,
         dev_body2 = dev_body^2,
         dev_flip2 = dev_flip^2)

# find sums for all columns of the tibble
sums <- summarise_at(one, which(sapply(one, is.numeric)), sum)  


