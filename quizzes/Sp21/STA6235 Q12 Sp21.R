library(palmerpenguins)
library(tidyverse)
library(fastDummies)
library(lindia)

almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

one <- penguins

one <- one %>% drop_na()
one <- dummy_cols(one, select_columns = c("sex"))

one$i <- as.numeric(rownames(one))

m1 <- lm(bill_length_mm ~ flipper_length_mm + sex_male, data = one)

summary(m1)
almost_sas(m1)

gg_cooksd(m1) + theme_bw()

one$resid <- rstandard(m1)
one$resid_flag <- ifelse(abs(one$resid) >= 3, 1, 0)

attach(one)
one <- one[order(one$resid),]
detach(one)



