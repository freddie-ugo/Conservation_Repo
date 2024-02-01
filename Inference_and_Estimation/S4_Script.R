xi <- c(4,4,5,5,6,7,7,8,9,9,9,10,10,10,11)

mean <- sum(xi)/15
mean_2 <- mean(xi)

summed <- sum((xi-mean)^2)

d_sum <- (1/14)*summed

s <- sqrt(d_sum)

sd <- ((1/(length(xi)-1))*sum((xi-sum(xi)/(length(xi)))^2))^0.5

variance <- var(xi)
sd_2 <- sqrt(var(xi))
sd_3 <- sd(xi)

quartiles <- quantile(xi, probs = seq(0, 1, 0.25))

IQR <- quartiles[3]-quartiles[1]
IQR_1 <- unname(IQR)


library(dplyr)
library(tidyr)
library(ggplot2)
