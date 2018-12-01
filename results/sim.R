#read-in for my desktop
pepper <- readRDS(file="/Users/Dan/Downloads/pop1.rds")
ibo <- readRDS("/Users/Dan/Downloads/pop2.rds")
#The populations generated are named after my two cats.
#pepper <- readRDS("/cloud/project/results/pop1.rds")
#ibo <- readRDS("/cloud/project/results/pop2.rds")
require(tidyverse)

sampsizes <- c(10,20,30,45,60,75)
testn <- rep(sampsizes,each=5000)
# Results for Pepper #
testnb500p <- map_dbl(testn,boot_pval,nboot=500,population=pepper)
testnb1000p <- map_dbl(testn,boot_pval,nboot=1000,population=pepper)
testnb1500p <- map_dbl(testn,boot_pval,nboot=1500,population=pepper)
testnb2000p <- map_dbl(testn,boot_pval,nboot=2000,population=pepper)
testnb2500p <- map_dbl(testn,boot_pval,nboot=2500,population=pepper)
testnb3000p <- map_dbl(testn,boot_pval,nboot=3000,population=pepper)

pep_nb500 <- colSums(matrix(testnb500p, ncol=6)<.05)/5000
pep_nb1000 <- colSums(matrix(testnb1000p, ncol=6)<.05)/5000
pep_nb1500 <- colSums(matrix(testnb1500p, ncol=6)<.05)/5000
pep_nb2000 <- colSums(matrix(testnb2000p, ncol=6)<.05)/5000
pep_nb2500 <- colSums(matrix(testnb2500p, ncol=6)<.05)/5000
pep_nb3000 <- colSums(matrix(testnb3000p, ncol=6)<.05)/5000

ttestpep <- map_dbl(testn, t_pval, nboot=1, mu=mean(pepper), population=pepper)
t_pep <- colSums(matrix(ttestpep, ncol=6)<.05)/5000
t_pep
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

t_pval(population=ibo,n=15, nboot=1000, mu=mean(ibo))

ttestibo <- map_dbl(testn, t_pval, nboot=1, mu=mean(ibo), population=ibo)
t_ibo <- colSums(matrix(ttestibo, ncol=6)<.05)/5000
t_ibo

popnames <- rep(c("Pepper","Ibo"), each=6)
nsims <- rep(c(500,1000,1500,2000,2500,3000),2)

nb500 <- c(pep_nb500, ibo_nb500)
nb1000 <- c(pep_nb1000, ibo_nb1000)
nb1500 <- c(pep_nb1500, ibo_nb1500)
nb2000 <- c(pep_nb2000, ibo_nb2000)
nb2500 <- c(pep_nb2500, ibo_nb2500)
nb3000 <- c(pep_nb3000, ibo_nb3000)

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
sim.data <- rbind(partial.data,tdata)
sim.data$nboot <- as.factor(sim.data$nboot)
sim.data$n <- as.factor(sim.data$n)

library(ggplot2)
ggplot(sim.data[sim.data$population=="Pepper",], aes(x=as.factor(n), y=alpha, color=as.factor(nboot), group=as.factor(nboot))) +
  geom_point() +
  geom_line()+
  theme_bw()


ggplot(sim.data[sim.data$population=="Ibo",], aes(x=as.factor(n), y=alpha, color=as.factor(nboot), group=as.factor(nboot))) +
  geom_point() +
  geom_line()+
  theme_bw()

library(here)
write_rds(sim.data, here("sim_data.rds"))
