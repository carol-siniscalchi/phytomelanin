setwd("~/phytomelanin/recon/")

library(ape)

#get tree
tree <- read.tree("tree.tre")
co <- c("#a6dba0","#7b3294")

#get traits
X<-read.csv("traits.bi.csv", row.names=1)
peri<-as.factor(setNames(X[,1],rownames(X)))
cort<-as.factor(setNames(X[,2],rownames(X)))
prif<-as.factor(setNames(X[,3],rownames(X)))
secf<-as.factor(setNames(X[,4],rownames(X)))
pxyl<-as.factor(setNames(X[,5],rownames(X)))
sxyl<-as.factor(setNames(X[,6],rownames(X)))
pith<-as.factor(setNames(X[,7],rownames(X)))

#Pericycle
#ER
er.peri <- ace(peri, tree, type="d", model="ER")
plot(tree, cex = .6)
nodelabels(pie=er.peri$lik.anc, piecol=co, cex=0.4)
aic.er.peri <- AIC(er.peri)
##Pericycle:(ER)-Log-likelihood: -29.87272 

#ARD
ard.peri <- ace(peri, tree, type="d", model="ARD")
plot(tree, cex = .6)
nodelabels(pie=ard.peri$lik.anc, piecol=co, cex=0.4)
aic.ard.peri <- AIC(ard.peri)
##Pericycle:(ARD)-Log-likelihood: -16.88091 



##Cortex
#ER
er.cort <- ace(cort, tree, type="d", model="ER")
plot(tree, cex = .6)
nodelabels(pie=er.cort$lik.anc, piecol=co, cex=0.4)
aic.er.cort <- AIC(er.cort)
##Cortex:(ER)-Log-likelihood: -28.89608 

#ARD
ard.cort <- ace(cort, tree, type="d", model="ARD")
plot(tree, cex = .6)
nodelabels(pie=ard.cort$lik.anc, piecol=co, cex=0.4)
aic.ard.cort <- AIC(ard.cort)


###Primary Phloem
#ER
er.prif <- ace(prif, tree, type="d", model="ER")
plot(tree, cex = .6)
nodelabels(pie=er.prif$lik.anc, piecol=co, cex=0.4)
aic.er.prif <- AIC(er.prif)
#Primary Phloem:(ER)- Log-likelihood: -23.87447 

#ARD
ard.prif <- ace(prif, tree, type="d", model="ARD")
plot(tree, cex = .6)
nodelabels(pie=ard.prif$lik.anc, piecol=co, cex=0.4)
aic.ard.prif <- AIC(ard.prif)
#Primary Phloem:(ARD)-Log-likelihood: -14.8851



#Seconday Phloem
#ER
er.secf <- ace(secf, tree, type="d", model="ER")
plot(tree, cex = .6)
nodelabels(pie=er.secf$lik.anc, piecol=co, cex=0.4)
aic.er.secf <- AIC(er.secf)
#Seconday Phloem:(ER)-Log-likelihood: -21.64607 

#ARD
ard.secf <- ace(secf, tree, type="d", model="ARD")
plot(tree, cex = .6)
nodelabels(pie=ard.secf$lik.anc, piecol=co, cex=0.4)
aic.ard.secf <- AIC(ard.secf)
#Seconday Phloem:(ARD)- -14.77448  

#Secondary Xylem
#ER
er.sxyl <- ace(sxyl, tree, type="d", model="ER")
plot(tree, cex = .6)
nodelabels(pie=er.sxyl$lik.anc, piecol=co, cex=0.4)
aic.er.sxyl <- AIC(er.sxyl)
#Secondary Xylem:(ER)-Log-likelihood: -11.29634

#ARD
ard.sxyl <- ace(sxyl, tree, type="d", model="ARD")
plot(tree, cex = .6)
nodelabels(pie=ard.sxyl$lik.anc, piecol=co, cex=0.4)
aic.ard.sxyl <- AIC(ard.sxyl)
#Secondary Xylem: (ARD)-  Log-likelihood: -7.442778


#Pith
#ER
er.pith <- ace(pith, tree, type="d", model="ER")
plot(tree, cex = .6)
nodelabels(pie=er.pith$lik.anc, piecol=co, cex=0.4)
aic.er.pith <- AIC(er.pith)
#Pith:(ER)- Log-likelihood: -25.47666 

#ADR
ard.pith <- ace(pith, tree, type="d", model="ARD")
plot(tree, cex = .6)
nodelabels(pie=ard.pith$lik.anc, piecol=co, cex=0.4)
aic.ard.pith <- AIC(ard.pith)
#Pith:(ARD)- Log-likelihood: -17.50855 



tissue <- c("pericycle" ,"cortex" ,"primary.phloem" ,"secondary.phloem","secondary.xylem" ,"pith")
ER.model <- c(aic.er.peri, aic.er.cort, aic.er.prif, aic.er.secf, aic.er.sxyl, aic.er.pith)
ADR.model <- c(aic.ard.peri, aic.ard.cort, aic.ard.prif, aic.ard.secf, aic.ard.sxyl, aic.ard.pith)
aic.table <- data.frame(tissue, ER.model, ADR.model)
write.csv(aic.table, file = "phytomelanin.aic.table.csv")

########
###             tissue ER.model  ADR.model
### 1        pericycle 61.74544  37.76182 
### 2           cortex 59.79216  39.48754
### 3   primary.phloem 49.74894  33.77021
### 4 secondary.phloem 45.29215  33.54896
### 5  secondary.xylem 24.59268  18.88556
### 6             pith 52.95331  39.01711

