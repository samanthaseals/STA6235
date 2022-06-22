library(tidyverse)

# enter data
x <- c( 80,  30,  50,  90,  70,  60, 120,  80, 100,
        50,  40,  70,  90,  20, 110, 100,  30,  50,
        90, 110,  30,  90,  40,  80,  70)

y <- c(399, 121, 221, 376, 361, 224, 546, 352, 353,
       157, 160, 252, 389, 113, 435, 420, 212, 268,
       377, 421, 273, 468, 244, 342, 323)

# put together as tibble
one <- tibble(x, y)

# find summary stats
means <- summarize(one, mean_x = mean(x, na.rm=TRUE), sd_x = sd(x, na.rm=TRUE), 
                        mean_y = mean(y, na.rm=TRUE), sd_y = sd(y, na.rm=TRUE))

################################################################################
######### the following is used to help check your hand calculations ###########
################################################################################

# create variables for summations
one$dev_x = one$x - means$mean_x[1]
one$dev_y = one$y - means$mean_y[1]
one$dev_xy = one$dev_x*one$dev_y
one$dev_x2 = one$dev_x^2
one$dev_y2 = one$dev_y^2

# find sums for all columns of the tibble
sums <- summarise_at(one, which(sapply(one, is.numeric)), sum)  

################################################################################

# construct model
one_model <- lm(y ~ x, data = one)
one_summary <- summary(one_model)
#one_summary

# find predicted values
one$pred  <- predict(one_model) 

# find residual values
one$resid <- residuals(one_model)

# square the error (residual)
one$resid2 <- one$resid^2

# do the above data management with tidyverse
one <- one %>% 
  mutate(pred = predict(one_model),
         resid = residuals(one_model),
         resid2 = residuals(one_model)^2)

# find SSE
sum(one$resid2)
nrow(one)-2

# convert to MSE
sum(one$resid2)/(nrow(one)-2)

# use ANOVA to find SSE and MSE
anova(one_model)

# other tidyverse examples
test <- one %>%
  select(-c(dev_x, dev_y, dev_xy, dev_x2, dev_y2))

test <- one %>%
  select(c(x, y, pred, resid)) %>%
  filter(y > 250)


