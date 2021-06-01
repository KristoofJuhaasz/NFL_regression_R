#library-k kiemelve#
library(lattice)
library(lmtest)
library(car)
library(sandwich)
library(corrplot)
library(lavaan)

#adatbazis beolvasasa#
library(lattice)
nfl<-read.csv("QBStats.csv",header=TRUE,sep=";", dec=",")
fit<-lm(points~yard+int+sack,data=nfl)

#modellszelekcio#
summary(fit)
summary(aov(fit))

#ramsay reset test#
library(lmtest)
resettest(fit)

#korrelacio#
rownames(nfl)<-nfl[,1]
nfl<-nfl[,-1]
cor(nfl)
corrplot(cor(nfl))

#multikollinearitas#
library(car)
vif(fit)

#homoszkedaszticitas-Breusch-Pagan test (H0=homoszkedaszticitas)#
library(lmtest)
bptest(fit)
sqrt(diag(vcovHC(fit)))
coeftest(fit,vcovHC(fit))

#kov mátrix#
library(sandwich)
sqrt(diag(vcovHC(fit)))
coeftest(fit,vcovHC(fit))

#elorejelzes-1pontbecsles-2intervallumbecsles#
predict(fit, data.frame(yard=250,int=1,sack=3))
predict(fit, data.frame(yard=250,int=1,sack=3),interval="predict")

#utelemzes - yard hatasa#
#kozvetlen#
summary(lm(points~yard,data=nfl))
#kozvetett
summary(lm(points~yard+int+sack,data=nfl))
summary(lm(yard~int+sack,data=nfl))
22.241*-3.679397+6.577*-1.815737
