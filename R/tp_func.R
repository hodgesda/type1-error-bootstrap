TTESTONLY <- function(population, n, nboot, mu, alternative="two.sided"){
  #draw a sample from the population of size n
  samp <- sample(population, n)
  
  #perform a t-test on the sample
  ttest <- t.test(samp, alternative=alternative, mu=mu)
  ttest$p.value
}
