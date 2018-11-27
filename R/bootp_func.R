TESTFUNC <- function(population, n, nboot, mu=mean(population), alternative="two.sided"){
  #draw a sample from the population of size n
  samp <- sample(population, n)
  #bootstrap the sample using the clever iid thing with a matrix.
  diff <- mu - mean(samp)
  adj.samp <-samp + diff
  bstrap <- matrix(sample(adj.samp, nboot*length(adj.samp),replace=TRUE),ncol=nboot)
  
  #We want some p values now, look into switch function
  if (alternative == "greater"){bootp <- sum(colMeans(bstrap)>= mean(samp))/nboot}
  else if (alternative == "less"){bootp <- sum(colMeans(bstrap)<= mean(samp))/nboot}
  else if (alternative == "two.sided"){bootp <- (sum(colMeans(bstrap)>= mu+abs(diff))+sum(colMeans(bstrap)<=mu-abs(diff)))/nboot}
  else stop("Alternative must be 'greater','less', or 'two.sided'")
  bootp
  
  #need to figure out output. I want the p-value from the t-test to come out and
  #a p-value from the bootstrap. I think I need some if-else clauses to deal with the
  #direction of the alternative for the way I set up bootstrapping.
}
