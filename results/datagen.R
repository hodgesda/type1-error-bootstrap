#The data generated here will be used to examine the bootstrap testing procedure and see if the type I error rates are what we expect.
#The first data set is symmetric coming from a a mixture of two normal distributions. The second comes from a log-normal distribution.
#Quick histograms were made in this file but this does not need to be run again. The data generated here is in the repo.
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
