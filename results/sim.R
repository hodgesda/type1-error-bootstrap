bootandt_pval(pepper, 30, 5000, mean(pepper))
sampsizes <- c(10,20,30,40,50,60)
require(mosaic)
testn <- rep(sampsizes,each=500)

### Preliminary Results for presentation ###
sampsizes <- c(10,20,30,40,50,60)
testn <- rep(sampsizes,each=500)
# Results for Pepper #
testnb500p <- map_dbl(testn,TESTFUNC,nboot=500,population=pepper)
testnb1000p <- map_dbl(testn,TESTFUNC,nboot=1000,population=pepper)
testnb1500p <- map_dbl(testn,TESTFUNC,nboot=1500,population=pepper)
testnb2000p <- map_dbl(testn,TESTFUNC,nboot=2000,population=pepper)
testnb2500p <- map_dbl(testn,TESTFUNC,nboot=2500,population=pepper)
testnb3000p <- map_dbl(testn,TESTFUNC,nboot=3000,population=pepper)

p_prelim_nb500 <- colSums(matrix(testnb500p, ncol=6)<.05)/500
p_prelim_nb1000 <- colSums(matrix(testnb1000p, ncol=6)<.05)/500
p_prelim_nb1500 <- colSums(matrix(testnb1500p, ncol=6)<.05)/500
p_prelim_nb2000 <- colSums(matrix(testnb2000p, ncol=6)<.05)/500
p_prelim_nb2500 <- colSums(matrix(testnb2500p, ncol=6)<.05)/500
p_prelim_nb3000 <- colSums(matrix(testnb3000p, ncol=6)<.05)/500

ttestpep <- map_dbl(testn, TTESTONLY, nboot=1, mu=mean(pepper), population=pepper)
prelim_t_p <- colSums(matrix(ttestpep, ncol=6)<.05)/500
prelim_t_p
# Results for Ibo #

testnb500i <- map_dbl(testn,TESTFUNC,nboot=500,population=ibo)
testnb1000i <- map_dbl(testn,TESTFUNC,nboot=1000,population=ibo)
testnb1500i <- map_dbl(testn,TESTFUNC,nboot=1500,population=ibo)
testnb2000i <- map_dbl(testn,TESTFUNC,nboot=2000,population=ibo)
testnb2500i <- map_dbl(testn,TESTFUNC,nboot=2500,population=ibo)
testnb3000i <- map_dbl(testn,TESTFUNC,nboot=3000,population=ibo)

i_prelim_nb500 <- colSums(matrix(testnb500i, ncol=6)<.05)/500
i_prelim_nb1000 <- colSums(matrix(testnb1000i, ncol=6)<.05)/500
i_prelim_nb1500 <- colSums(matrix(testnb1500i, ncol=6)<.05)/500
i_prelim_nb2000 <- colSums(matrix(testnb2000i, ncol=6)<.05)/500
i_prelim_nb2500 <- colSums(matrix(testnb2500i, ncol=6)<.05)/500
i_prelim_nb3000 <- colSums(matrix(testnb3000i, ncol=6)<.05)/500

TTESTONLY(population=ibo,n=15, nboot=1000, mu=mean(ibo))

ttestibo <- map_dbl(testn, TTESTONLY, nboot=1, mu=mean(ibo), population=ibo)
prelim_t_i <- colSums(matrix(ttestibo, ncol=6)<.05)/500
prelim_t_i

popnames <- rep(c("Pepper","Ibo"), each=6)
nsims <- rep(c(500,1000,1500,2000,2500,3000),2)

prelim_nb500 <- c(p_prelim_nb500, i_prelim_nb500)
prelim_nb1000 <- c(p_prelim_nb1000, i_prelim_nb1000)
prelim_nb1500 <- c(p_prelim_nb1500, i_prelim_nb1500)
prelim_nb2000 <- c(p_prelim_nb2000, i_prelim_nb2000)
prelim_nb2500 <- c(p_prelim_nb2500, i_prelim_nb2500)
prelim_nb3000 <- c(p_prelim_nb3000, i_prelim_nb3000)

prelim.data <- data.frame(c(p_prelim_nb500, p_prelim_nb1000, p_prelim_nb1500, 
                            p_prelim_nb2000, p_prelim_nb2500, p_prelim_nb3000,
                            i_prelim_nb500, i_prelim_nb1000, i_prelim_nb1500, 
                            i_prelim_nb2000, i_prelim_nb2500, i_prelim_nb3000),
                          rep(nsims,each=6),
                          rep(sampsizes,6),
                          rep(c("Pepper","Ibo"),each=36)
                          )

colnames(prelim.data) <- c("alpha", "nboot", "n", "population")

tdata <- data.frame(c(prelim_t_p, prelim_t_i), rep("t",12),rep(sampsizes,2),rep(c("Pepper","Ibo"),each=6))
colnames(tdata) <- c("alpha", "nboot", "n", "population")
presentdata <- rbind(prelim.data,tdata)
presentdata$nboot <- as.factor(presentdata$nboot)
presentdata$n <- as.factor(presentdata$n)

library(ggplot2)
ggplot(presentdata[presentdata$population=="Pepper",], aes(x=as.factor(n), y=alpha, color=as.factor(nboot), group=as.factor(nboot))) +
  geom_point() +
  geom_line()+
  theme_bw()


ggplot(presentdata[presentdata$population=="Ibo",], aes(x=as.factor(n), y=alpha, color=as.factor(nboot), group=as.factor(nboot))) +
  geom_point() +
  geom_line()+
  theme_bw()

#library(devtools)
#install_github("Gibbsdavidl/CatterPlots")

#library(CatterPlots)
#multicat(prelim.data$n, prelim.data$alpha, type="justcats", cat=c(sample(1:10,5)), size=.075)

library(here)
write_rds(presentdata, here("results", "presentdata.rds"))
