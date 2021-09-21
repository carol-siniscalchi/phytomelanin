setwd("~/phytomelanin/recon/")

library(ape)

#color pallete
co <- c("#a6dba0","#7b3294")

#tree
read.tree("tree.tre") -> tree

#charcaters for tip states
X<-read.csv("traits.bi.csv", row.names=1)
peri<-as.factor(setNames(X[,1],rownames(X)))
cort<-as.factor(setNames(X[,2],rownames(X)))
secf<-as.factor(setNames(X[,4],rownames(X)))
pith<-as.factor(setNames(X[,7],rownames(X)))

##Cortex
ace(cort,tree, type="d", model="ARD")-> cor.ARD

#Floema secundario
ace(secf,tree, type="d", model="ARD")-> F2.ARD

#Medula
ace(pith,tree, type="d", model="ARD")-> med.ARD

#Periciclo
ace(peri, tree, type="d", model="ARD")-> periARD

#plot all trees together 2x2
pdf(file="phyto.pdf", width = 16, height = 16)
par(mfrow = c(2,2))
plot(tree, cex = 0.6) 
nodelabels(pie=periARD$lik.anc, piecol=co, cex=0.5)
tiplabels(pie=to.matrix(peri[tree$tip.label], levels(peri)),piecol=co,cex=0.2)
plot(tree, cex = 0.6)
nodelabels(pie=cor.ARD$lik.anc, piecol=co, cex=0.5)
tiplabels(pie=to.matrix(cort[tree$tip.label], levels(cort)),piecol=co,cex=0.2)
plot(tree, cex = 0.6)
nodelabels(pie=F2.ARD$lik.anc, piecol=co, cex=0.5)
tiplabels(pie=to.matrix(secf[tree$tip.label], levels(secf)),piecol=co,cex=0.2)
plot(tree, cex = 0.6) 
nodelabels(pie=med.ARD$lik.anc, piecol=co, cex=0.5)
tiplabels(pie=to.matrix(pith[tree$tip.label], levels(pith)),piecol=co,cex=0.2)
dev.off()

#plot all trees together 3 columns 1 row
pdf(file="phyto2.pdf", width = 16, height = 20)
par(mfrow = c(1,3))
plot(tree, cex = 1.5)
nodelabels(pie=cor.ARD$lik.anc, piecol=co, cex=1)
tiplabels(pie=to.matrix(cort[tree$tip.label], levels(cort)),piecol=co,cex=0.5)
plot(tree, cex = 1.5)
nodelabels(pie=F2.ARD$lik.anc, piecol=co, cex=1)
tiplabels(pie=to.matrix(secf[tree$tip.label], levels(secf)),piecol=co,cex=0.5)
plot(tree, cex = 1.5) 
nodelabels(pie=med.ARD$lik.anc, piecol=co, cex=1)
tiplabels(pie=to.matrix(pith[tree$tip.label], levels(pith)),piecol=co,cex=0.5)
dev.off()

#plot all trees together 1 column, 3 rows
pdf(file="phyto3.pdf", width = 16, height = 20)
par(mfrow = c(3,1))
plot(tree, cex = 1)
nodelabels(pie=cor.ARD$lik.anc, piecol=co, cex=0.3)
tiplabels(pie=to.matrix(cort[tree$tip.label], levels(cort)),piecol=co,cex=0.1)
plot(tree, cex = 1)
nodelabels(pie=F2.ARD$lik.anc, piecol=co, cex=0.3)
tiplabels(pie=to.matrix(secf[tree$tip.label], levels(secf)),piecol=co,cex=0.1)
plot(tree, cex = 1) 
nodelabels(pie=med.ARD$lik.anc, piecol=co, cex=0.3)
tiplabels(pie=to.matrix(pith[tree$tip.label], levels(pith)),piecol=co,cex=0.1)
dev.off()

#plot all tip states in a single figure
read.tree("tree.tre") -> tree
X<-as.matrix(read.csv("traits.bi.csv", row.names=1))
colsy <-setNames(c("#a6dba0","#7b3294"), c("a","b"))
pdf(file="all.tipstates.pdf", width = 6, height = 7)
dotTree(tree2, X, labels=TRUE, colors=colsy,data.type="discrete",fsize=0.6)
dev.off()