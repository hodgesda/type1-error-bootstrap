#The populations generated are named after my two cats. 
#Pepper is 6 years old and her energy levels follow a roughly symmetric distribution.
#Ibo is only half a year old and he has sometimes erractic amounts of energy and zips across the house.
#So the skewed distribution is named after him (pronounced "E-bow").
pepper <- readRDS("/cloud/project/results/pop1.rds")
ibo <- readRDS("/cloud/project/results/pop2.rds")
require(tidyverse)

sampsizes <- c(10,20,30,45,60,75)
#testn is where I am setting the simulation size. I map over the vector of sample sizes.
testn <- rep(sampsizes,each=5000)

#Note to the reader. I originally planned to use map2 to avoid all this iteration.
#I had trouble figuring it out and ended up doing it this way. Take the points away, I don't deserve them!
#I should have given myself another day of fiddling with the simulation to get it working.

# Results for population Pepper #

#Mapping over the sample vector with different bootstrap sizes.
testnb500p <- map_dbl(testn,boot_pval,nboot=500,population=pepper)
testnb1000p <- map_dbl(testn,boot_pval,nboot=1000,population=pepper)
testnb1500p <- map_dbl(testn,boot_pval,nboot=1500,population=pepper)
testnb2000p <- map_dbl(testn,boot_pval,nboot=2000,population=pepper)
testnb2500p <- map_dbl(testn,boot_pval,nboot=2500,population=pepper)
testnb3000p <- map_dbl(testn,boot_pval,nboot=3000,population=pepper)

#Type I error is estimated by taking the number that have pvalues less than .05.
#When testing against the null hypothesis that is true.
pep_nb500 <- colSums(matrix(testnb500p, ncol=6)<.05)/5000
pep_nb1000 <- colSums(matrix(testnb1000p, ncol=6)<.05)/5000
pep_nb1500 <- colSums(matrix(testnb1500p, ncol=6)<.05)/5000
pep_nb2000 <- colSums(matrix(testnb2000p, ncol=6)<.05)/5000
pep_nb2500 <- colSums(matrix(testnb2500p, ncol=6)<.05)/5000
pep_nb3000 <- colSums(matrix(testnb3000p, ncol=6)<.05)/5000

#Getting results from the t-test for comparison.
ttestpep <- map_dbl(testn, t_pval, mu=mean(pepper), population=pepper)
t_pep <- colSums(matrix(ttestpep, ncol=6)<.05)/5000

# Results for Ibo #

testnb500i <- map_dbl(testn,boot_pval,nboot=500,population=ibo)
testnb1000i <- map_dbl(testn,boot_pval,nboot=1000,population=ibo)
testnb1500i <- map_dbl(testn,boot_pval,nboot=1500,population=ibo)
testnb2000i <- map_dbl(testn,boot_pval,nboot=2000,population=ibo)
testnb2500i <- map_dbl(testn,boot_pval,nboot=2500,population=ibo)
testnb3000i <- map_dbl(testn,boot_pval,nboot=3000,population=ibo)

ibo_nb500 <- colSums(matrix(testnb500i, ncol=6)<.05)/5000
ibo_nb1000 <- colSums(matrix(testnb1000i, ncol=6)<.05)/5000
ibo_nb1500 <- colSums(matrix(testnb1500i, ncol=6)<.05)/5000
ibo_nb2000 <- colSums(matrix(testnb2000i, ncol=6)<.05)/5000
ibo_nb2500 <- colSums(matrix(testnb2500i, ncol=6)<.05)/5000
ibo_nb3000 <- colSums(matrix(testnb3000i, ncol=6)<.05)/5000

ttestibo <- map_dbl(testn, t_pval, mu=mean(ibo), population=ibo)
t_ibo <- colSums(matrix(ttestibo, ncol=6)<.05)/5000

#Setting up some identifying columns for populations and bootstrap sizes.
popnames <- rep(c("Pepper","Ibo"), each=6)
nsims <- rep(c(500,1000,1500,2000,2500,3000),2)

#Concatinating the results into long-form vectors
nb500 <- c(pep_nb500, ibo_nb500)
nb1000 <- c(pep_nb1000, ibo_nb1000)
nb1500 <- c(pep_nb1500, ibo_nb1500)
nb2000 <- c(pep_nb2000, ibo_nb2000)
nb2500 <- c(pep_nb2500, ibo_nb2500)
nb3000 <- c(pep_nb3000, ibo_nb3000)

#Pulling the results together into a dataframe for plotting.
partial.data <- data.frame(c(pep_nb500, pep_nb1000, pep_nb1500, 
                             pep_nb2000, pep_nb2500, pep_nb3000,
                             ibo_nb500, ibo_nb1000, ibo_nb1500, 
                             ibo_nb2000, ibo_nb2500, ibo_nb3000),
                          rep(nsims,each=6),
                          rep(sampsizes,6),
                          rep(c("Pepper","Ibo"),each=36)
                          )

colnames(partial.data) <- c("alpha", "nboot", "n", "population")

tdata <- data.frame(c(t_pep, t_ibo), rep("t",12),rep(sampsizes,2),rep(c("Pepper","Ibo"),each=6))
colnames(tdata) <- c("alpha", "nboot", "n", "population")

#Sim data is the resulting data object, bootstrap size changed into a factor rather than a double.
sim.data <- rbind(partial.data,tdata)
sim.data$nboot <- as.factor(sim.data$nboot)
sim.data$n <- as.factor(sim.data$n)

#Plotting for presentation/report.
library(ggplot2)
ggplot(sim.data[sim.data$population=="Pepper",], aes(x=as.factor(n), y=alpha, color=as.factor(nboot), group=as.factor(nboot))) +
  geom_point() +
  geom_line()+
  theme_bw()


ggplot(sim.data[sim.data$population=="Ibo",], aes(x=as.factor(n), y=alpha, color=as.factor(nboot), group=as.factor(nboot))) +
  geom_point() +
  geom_line()+
  theme_bw()

#The final line writes the .rds file in the repo.
library(here)
write_rds(sim.data, here("sim_data.rds"))
