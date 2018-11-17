#This function draws a single sample from a population, performs a t-test and
#bootstraps that sample. Requires the mosaic package.

badfunctionname <- function(population, n, nboot, mu, alternative){
  #draw a sample from the population of size n
  samp <- sample(population, n)
  
  #perform a t-test on the sample
  ttest <- t.test(samp, alternative=alternative, mu=mu)
  #bootstrap the sample using the clever iid thing with a matrix.
  diff <- mu - mean(samp)
  adj.samp <-samp + diff
  bstrap <- matrix(sample(adj.samp, nboot*length(adj.samp),replace=TRUE),ncol=nboot)
  
  #We want some p values now, look into switch function
  if (alternative == "greater"){bootp <- sum(colMeans(bstrap)>= mean(samp))/nboot}
  else if (alternative == "less"){bootp <- sum(colMeans(bstrap)<= mean(samp))/nboot}
  else if (alternative == "two.sided"){bootp <- (sum(colMeans(bstrap)>= mu+abs(diff))+sum(colMeans(bstrap)<=mu-abs(diff)))/nboot}
  else stop("Alternative must be 'greater','less', or'two.sided'")
  c(ttest$p.value, bootp)
  
  #need to figure out output. I want the p-value from the t-test to come out and
  #a p-value from the bootstrap. I think I need some if-else clauses to deal with the
  #direction of the alternative for the way I set up bootstrapping.
}