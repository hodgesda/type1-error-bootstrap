#This function draws a single sample from a population, performs a t-test and
#bootstraps that sample. Requires the mosaic package.

badfunctionname <- function(population, n, nboot, mu, alternative){
  #draw a sample from the population of size n
  samp <- sample(population, n)
  
  #perform a t-test on the sample
  ttest <- t.test(samp, alternative, mu)
  #bootstrap the sample using Do() from the package mosaic.
  bstrap <- Do(nboot)*mean(sample(samp, replace=TRUE))
  
  #need to figure out output. I want the p-value from the t-test to come out and
  #a p-value from the bootstrap. I think I need some if-else clauses to deal with the
  #direction of the alternative for the way I set up bootstrapping.
}