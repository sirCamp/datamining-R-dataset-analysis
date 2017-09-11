library(HDclassif)
data(wine)

names(wine)
print(nrow(wine))


#valutazione grafica tra le variabili
pairs(wine[,-1], col = wine$class, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("tipo 1","tipo 2","tipo 3"),  pch = 16, col = c("black","red","green")) ##come si interpreta?

pr <-prcomp(wine[,-1],scale=TRUE)

names(pr)

"""
ovviamente tolgo il prcomp
• sdev: radice della varianza spiegata da ogni componente principale
• rotation: i vettori dei loadings (phi)
• center: media delle variabili
• scale: scala delle variabili
• x: matrice le cui colonne sono gli score (vettori) per tutte le unità statistiche

"""

pr$center
pr$rotation


#ci sono 13 componenti principali! in un dataset con n righe e p var --> min(n-1,p)

par(mfrow = c(2,2))
biplot(pr,	scale=0,	cex=0.5)
plot(pr$x[,1:2],	col=wine$class) 

#calcolo devianza e porzione della devianza spiegata
varianza	<-	pr$sdev^2
prop.varianza	<-	varianza/sum(varianza)
prop.varianza

plot(prop.varianza, xlab='Componenti principali',
     ylab='Proporzione di varianza spiegata', type='l', cex.lab=0.7)
## la funzione cumsum calcola la somma cumulata
plot(cumsum(prop.varianza), xlab='Componenti principali',
     ylab='Proporzione cumulata di varianza spiegata', type='l', cex.lab=0.7)


#NUOVA LIB
install.packages('pls')
library(pls)

data(gasoline)
?gasoline

y<-gasoline$octane
x<-gasoline$NIR ##matrice

set.seed(222)

m.pcr <-pcr(y~x,ncomp=20,scale=TRUE,validation='CV')
#utilizzo il cross-validation con 10 fold numero massimo di componenti=20 

summary(m.pcr)


#par(mfrow=c(1,2))
validationplot(m.pcr, val.type='MSEP', main='Gasoline')
validationplot(m.pcr, val.type='R2', main='Gasoline')

#per capire quale sciegliere vado o via grafico o via dati:

explvar(m.pcr)

plot(explvar(m.pcr),type='l') #poche variabili???

coefplot(m.pcr,	ncomp=1:5,	legendpos='bottomleft')

coefplot(m.pcr,	ncomp=1:10,	legendpos='bottomleft')

scoreplot(m.pcr,	comps=1:5,	cex=0.5)
plot(m.pcr)
abline(0,1)


