#The idea is to make at least two datasets, one should be a setting where bootstrapping and
#t-methods are effective (symmetric) and the other something where they might fail (skewed).
#Per Charlotte's suggestion the first simulated population will be from mixed distributions.
#A normal distribution provides the bulk of points in the center while a t-distribution
#contributes additional observations into the tails. The mean and sd of the normal and t-distributions
#are arbitrary.
library(here)
library(tidyverse)
norm1 <- rnorm(500000, mean=20, sd=12)
norm2 <- rnorm(500000, mean=20, sd=5)
pop1 <- c(norm1,norm2)
hist(pop1, breaks=50)
abline(v=mean(pop1),col="red")

pop2 <- rlnorm(1000000, meanlog=log(1.5), sdlog=1)
hist(pop2, breaks=500)
abline(v=mean(pop2),col="red")

write_rds(pop1, here("results", "pop1.rds"))
write_rds(pop2, here("results", "pop2.rds"))
