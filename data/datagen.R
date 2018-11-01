#The idea is to make at least two datasets, one should be a setting where bootstrapping and
#t-methods are effective (symmetric) and the other something where they might fail (skewed).
#Per Charlotte's suggestion the first simulated population will be from mixed distributions.
#A normal distribution provides the bulk of points in the center while a t-distribution
#contributes additional observations into the tails. The mean and sd of the normal and t-distributions
#are arbitrary.
library(here)
library(tidyverse)
norm_part <- rnorm(500000, mean=20, sd=12)
t_part <- rt(500000, df=5, ncp=20)
pop1 <- c(norm_part,t_part)
hist(pop1, breaks=50)
sum(pop1>100)


write_rds(pop1, here("results", "pop1.rds"))
