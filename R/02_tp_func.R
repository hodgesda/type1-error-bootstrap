#Originally I made one function that did both a bootstrap and a t-test and stored those results as a list.
#This did not mesh well with the way I wanted to do my simulation. So I wrote a short function to just extract
#the p-value from the t.test function. This way the output is exactly the way I wanted it and it takes
#the same set of arguments as the other function.

t_pval <- function(population, n, mu=mean(population), alternative="two.sided"){
  #draw a sample from the population of size n
  samp <- sample(population, n)
  
  #perform a t-test on the sample
  ttest <- t.test(samp, alternative=alternative, mu=mu)
  ttest$p.value
}
