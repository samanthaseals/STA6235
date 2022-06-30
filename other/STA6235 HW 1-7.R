library(tidyverse)

x <- c(7, 12, 4, 14, 25, 30)
y <- c(128, 213, 75, 250, 446, 540)

one <- tibble(x, y)

means <- summarize(one, xbar = mean(x, na.rm=TRUE), ybar = mean(y, na.rm=TRUE))

b1 <- c(17, 18, 19)
b0 <- c(means$ybar[1] - b1*means$xbar[1])

one$L1 <- (y - b0[1] - b1[1]*x)^2
one$L2 <- (y - b0[2] - b1[2]*x)^2
one$L3 <- (y - b0[3] - b1[3]*x)^2

sums <- summarize(one, L1sum = sum(L1, na.rm=TRUE), 
                       L2sum = sum(L2, na.rm=TRUE),
                       L3sum = sum(L3, na.rm=TRUE))

L1 <- 1/((32*pi)^3)*exp( (-1/32) * sums$L1sum)
L2 <- 1/((32*pi)^3)*exp( (-1/32) * sums$L2sum)
L3 <- 1/((32*pi)^3)*exp( (-1/32) * sums$L3sum)

one$xy <- one$x*one$y
one$x2 <- one$x^2 
one$y2 <- one$y^2

sum(one$xy)/sum(one$x2)

b1 <- seq(17, 19, 0.001)
b0 <- c(means$ybar[1] - b1*means$xbar[1])

sum_y <- sum(one$y)
sum_y2 <- sum(one$y2)
sum_x <- sum(one$x)
sum_x2 <- sum(one$x2)
sum_xy <- sum(one$xy)

two <- tibble(b0, b1)

two$L <- ((1/(32*pi))^3)*exp((-1/32)*(sum_y2 - two$b0*sum_y - 2*two$b1*sum_xy - two$b0*sum_y + 6*two$b0^2 + 2*two$b0*two$b1*sum_x + two$b1^2*sum_x2))

dot <- filter(two, L == max(L))

ggplot(data=two, aes(x=b1, y=L, group=1)) +
  geom_line() + 
  geom_point(x=dot$b1, y=dot$L, color="red") +
  theme_minimal() +
  xlab("Slope") +
  ylab("Likelihood")
