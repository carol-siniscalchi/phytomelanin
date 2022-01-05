setwd("~/phytomelanin/recon/")

library("ape")
library ("geiger")
library ("caper")

#Frizt and Purvis D statistics (2010)

tree <- read.tree("tree.tre")
char <- read.csv("traits.bi.csv")
rownames(char) <- tree$tip.label
D <-cbind(as.data.frame(char),rownames(char))
colnames(char) <- c("taxa","Periciclo", "cor","F1","F2","X1","X2","Med")
phylo.d(char,tree,names.col=taxa, binvar=cor)

# delta statistics from Borges et al. (2019)

source("code.R") #from https://github.com/mrborges23/delta_statistic

tree <- read.tree("tree.tre")
char <- read.csv("traits.bi.csv")
rownames(char) <- tree$tip.label

attach(char)

rownames(char) -> names(Med)

#SOME PARAMETERS... 
lambda0 <- 0.1   #rate parameter of the proposal 
se      <- 0.5   #standard deviation of the proposal
sim     <- 100000 #number of iterations
thin    <- 100    #we kept only each 10th iterate 
burn    <- 1000  #100 iterates are burned-in

deltaA <- delta(Med,tree,lambda0,se,sim,thin,burn)
print(deltaA)

deltaA.dis <- NULL
for (i in 1:999) {
  deltaA.dis[i] <-delta(Med,tree,lambda0,se,sim,thin,burn)[1]
}

quantile(deltaA.dis, probs = c(0.975)) -> quantile.deltaA.dis
median(deltaA.dis) -> median.deltaA.dis
quantile.deltaA.dis
median.deltaA.dis

rand.D <- NULL
for (i in 1:999) {
  rand.D[i] <- delta(sample(Med),tree,lambda0,se,sim,thin,burn)[1]
}
quantile(rand.D, probs = c(0.975)) -> quantile.rand.D
median(rand.D)-> median.rand.D
quantile.rand.D
median.rand.D

#hist(rand.D, xlim=c(2.25,2.35), col="grey")
#abline(v=quantile.rand.D, col="grey", lty=2, lwd=2)
#abline(v=median.rand.D, col="grey", lwd=1)
#hist(deltaA.dis, add=T, col="white")
#abline(v=quantile.deltaA.dis, col="black", lty=2, lwd=2)
#abline(v=median.deltaA.dis, col="black", lwd=1)