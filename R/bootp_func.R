boot_pval <- function(population, n, nboot, mu=mean(population), alternative="two.sided"){
#draw a sample from the population of size n
  samp <- sample(population, n)
#calculate the difference between the sample mean and null hypothesis value.
  diff <- mu - mean(samp)
#adjust the data so the null hypothesis is true.
  adj.samp <-samp + diff
#bootstrap the sample using a matrix, exploiting the fact that the bootstraps are iid.
  bstrap <- matrix(sample(adj.samp, nboot*length(adj.samp),replace=TRUE),ncol=nboot)
  
#p values, if-else statements are for the direction of alternative.
  if (alternative == "greater"){bootp <- sum(colMeans(bstrap)>= mean(samp))/nboot}
  else if (alternative == "less"){bootp <- sum(colMeans(bstrap)<= mean(samp))/nboot}
  else if (alternative == "two.sided"){bootp <- (sum(colMeans(bstrap)>= mu+abs(diff))+sum(colMeans(bstrap)<=mu-abs(diff)))/nboot}
  else stop("Alternative must be 'greater','less', or 'two.sided'")
  (boot.pval=bootp)
}
