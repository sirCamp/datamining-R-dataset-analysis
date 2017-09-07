rm(list=ls())

library(ISLR)

data(Carseats)

dim(Carseats)

names(Carseats)

?Carseats


dati <- Carseats[,c('Sales','Price','Urban','US','ShelveLoc')]

dim(dati)
names(dati)


boxplot(dati$Sales,ylab='Sales',cex.lab=2,cex.axis=2)

#boxplot(sqrt(dati$Sales),dati$Sales,ylab='Sales',cex.lab=2,cex.axis=2) // se asiummetrica


hist(dati$Sales,prob=TRUE)

plot(dati$Price,dati$Sales)
# posso dire che la relazione è negativa, all'aumentare del prezzo calano le vendite


boxplot(dati$Sales~dati$Urban) #non c'è molta variazione

boxplot(dati$Sales~dati$US)
#sembra che le vendite siano meglio in usa che all'estero

boxplot(dati$Sales~dati$ShelveLoc) ## ce molta varietà

summary(dati$ShelveLoc)

plot(dati$Price,	dati$Sales)
points(dati$Price[dati$ShelveLoc=='Bad'],	dati$Sales[dati$ShelveLoc=='Bad'],	col='red',	pch=19)
points(dati$Price[dati$ShelveLoc=='Good'],	dati$Sales[dati$ShelveLoc=='Good'],	col='green',	pch=19)
points(dati$Price[dati$ShelveLoc=='Medium'],	dati$Sales[dati$ShelveLoc=='Medium'],	col='blue',	pch=19)

legend('bottomleft',	pch=c(19,19,19),	col=c('red',	'green',	'blue'),	legend=c('Bad',	'Good',	'Medium'),	bty='n')

m <- lm(Sales ~ Price + Urban + US + ShelveLoc, data=dati)

summary(m)

m2	<-	update(m,	.	~	.	-	Urban)
summary(m2)

anova(m2,m)

#calcolo il quantile
qf(0.95,1,395)

#calcolo il p-value 
1-pf(1.4386,1,395)

# la statistica F è più piccola del quantile quindi gtengo il modello piu piccolo


#cerco la correlazione tra Price e ShelveLoc
m3 <- lm(Sales ~ Price * ShelveLoc + US, data=dati)
summary(m3)

anova(m2,m3)
qf(0.95,1,394)
1-pf(0.648,1,394)

#direi che p meglio tenere l'm3 visto chge F è piu piccola e dentro il quantile inoltre il p-value è più alto e quindi si sposta dallo 0: