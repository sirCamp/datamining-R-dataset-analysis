library(HDclassif)
data(wine)

dim(wine)

names(wine)

?wine


barplot(table(wine$class))
pie(table(wine$class))


## faccio analisi di distribuzione dei vini

pairs(wine[,2:7])
pairs(wine[,8:13])


# facciamo analisi del discriminante lineare

library(MASS)

modello.lda <- lda(class ~ ., data=wine) #usando "." R usa tutte le var come esplicative
summary(modello.lda)
print(modello.lda)

"""
group means: indica la media dei gruppi
Coefficents indica i discriminanti
Portion of trace la percentuale di separazione dei gruppi

Essencodi molte variabili ho due funziioni discriminanti

"""

plot(modello.lda)
""" 
si può niotare che c'è molta separazione per quanto figuarda LDA1 --> 0.69 --> 69%
mentre p minore per LDA2

un altr modo per vedere la divisione e la capacità di discriminazione è:

"""

previsioni.lda <- predict(modello.lda)

ldahist(previsioni.lda$x[,1],g=wine$class) ## si vedono belli separati
ldahist(previsioni.lda$x[,2],g=wine$class) ## si vede che sono sovrapposti


"""""
TORNIAMO A BOSTON

"""""
rm(list=ls())

data(Boston)
dim(Boston)

names(Boston)
?Boston


modello1 <- lm(medv ~ lstat, data=Boston)
modello2 <- lm(medv ~ lstat + I(lstat^2),data=Boston)
modello3 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3),data=Boston)

summary(modello2)
summary(modello3)

anova(modello2,modello3)

aic.m2 <- 2*2 - 2*logLik(modello2)
print(aic.m2)

aic.m3 <- 2*2 - 2*logLik(modello3)
print(aic.m3)

"""
da quin vediamo che modello3 è migliore rispetto a modello2 perchè aic è piu piccolo.
Se la differenza tra i due è tra è compresa in 1 allora non serve cambiare, se è maggiore di uno allora si.
Nel nostro caso abbiamo:
"""

366.516 - 3141.796 # che èp decisametne maggiore di uno quindi tengo mdello3

"""
orqa provo ad usare il modello generalizzato per modello2 e vedo cosa cambia

"""

modello2.glm <- glm(medv ~ lstat + I(lstat^2), data=Boston)
summary(modello2.glm)

modello3.glm <- glm(medv ~ lstat + I(lstat^2) + I(lstat^3), data=Boston)
summary(modello3.glm)

modello2.glm$aic - modello3.glm$aic
#valore altissimo che indica che è meglio tenere m3
#La bic ci suggerisce che potrebbe essere anche peggio con polonmi di livello 5 o6

modello5.glm <- glm(medv ~ poly(lstat,5), data=Boston)
summary(modello5.glm)

modello6.glm <- glm(medv ~ poly(lstat,6), data=Boston)
summary(modello6.glm)

# dai risultati il 5 ha un AIC sensato il 6 ha un AIC addiruttra maggire: non va bene
# ora provo a fare validation caricando boot
library(boot)

set.seed(123)
cv.err.m2 <- cv.glm(Boston, modello2.glm, K=10)
names(cv.err.m2)

##questa funziona calcola la stima dell'errore dei modeli di questo tipo quindi
## ora prendo i delta, il secondo numero è quello coretto

cv.err.m2$delta ## 30.64750 30.63077

#faccio per modello3
set.seed(123)
cv.err.m3 <- cv.glm(Boston, modello3.glm, K=10)
cv.err.m3$delta

#scelgo il modello 3 perchè ha il valore piu dbasso di mse
#proviamo con gli altri polinomi

set.seed(123)
cv.err.m5 <- cv.glm(Boston, modello5.glm, K=10)
cv.err.m5$delta

set.seed(123)
cv.err.m6 <- cv.glm(Boston,modello6.glm,K=10)
cv.err.m6$delta

#6 e 5 si assomigliano quindi tengo il 5. nn hasenso andarenoltre
# ora provo uitilizzando loocv

cv.err.m3 <- cv.glm(Boston,modello3.glm)
print(cv.err.m3$delta)
#in questo caso nn serve il seed perch+ faccio la validazione di tutto

"""
a questo punto provo a fare i test con  R aggiustato e con aic

"""

r2.aggiustato <- rep(0,6)
aic <- rep(0,6)

for(i in 1:6){
  
  m <- lm(medv ~ poly(lstat,i), data=Boston)
  r2.aggiustato[i] <- summary(m)$adj.r.squared
  aic[i] <- 2*(i+2) - 2*logLik(m)
}

print(r2.aggiustato)
print(aic)
             
############# nuovo dataset  

rm(list=ls())

library(ISLR)
data(Hitters)
dim(Hitters)


summary(Hitters)
#notiamo che ci sono tanti valori vuoti  a "na"

sum(is.na(Hitters))

#con questo comando rimuovo quelli che non hanno i dati comoleti
hitters <- na.omit(Hitters)
dim(hitters)
## [1] 263 20
sum(is.na(hitters))

boxplot(hitters$Salary, ylab='Salary')

?Hitters

"""
ci sono tante variabili vale la pena provare a fare una cosa diversa

Dato l’alto numero delle variabili, consideriamo una selezione automatica di tipo forward
regression. La funzione regsubsets() inclusa nella libreria leaps permette di effettuare
una stepwise regression per modelli lineari sulla base di diversi criteri usando un numero
specificato di variabili esplicative. La specificazione del modello segue la sintassi usata
per lm().

"""

library(leaps)
m.forward <- regsubsets(Salary ~ ., data=hitters, nvmax=19, method='forward')

"""
In particolare, contiene i valori di RSS, R
2
, R
2
e BIC per i vari modelli stimati.
"""

summary(m.forward)$rss

which.min(summary(m.forward)$rss)
#oil modello con rss minore è quello numero 19 variabili esplicative con queste var

coef(m.forward, 19)

## secondo il bic le cose cambiano: è il modello 6
which.min(summary(m.forward)$bic)

coef(m.forward, 6)

vcov(m.forward, 6)
sqrt(diag(vcov(m.forward, 6)))
plot(m.forward)
"""
In alto in nero sono indicate le variabili che rimangono nel modello migliore, quello con
BIC minore. Sono 6 variabili (intercetta esclusa), esattamente come notato in precedenza.
"""
plot(m.forward, scale='adjr2')

#cambiando scala di rif cambiano i grafici e le interpretazioni possibili

#par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
## R quadrato
plot(summary(m.forward)$rsq, xlab='Numero di variabili', ylab='R2', type='l')
## aggiungiamo l'indicazione del modello migliore
points(which.max(summary(m.forward)$rsq),
       summary(m.forward)$rsq[which.max(summary(m.forward)$rsq)],
       col='red', pch=16)
## RSS
plot(summary(m.forward)$rss, xlab='Numero di variabili', ylab='RSS', type='l')
points(which.min(summary(m.forward)$rss),
       summary(m.forward)$rss[which.min(summary(m.forward)$rss)],
       col='red', pch=16)

## R quadrato aggiustato
plot(summary(m.forward)$adjr2, xlab='Numero di variabili',
     ylab='R2 aggiustato', type='l')
points(which.max(summary(m.forward)$adjr2),
       summary(m.forward)$adjr2[which.max(summary(m.forward)$adjr2)],
       col='red', pch=16)
## BIC
plot(summary(m.forward)$bic, xlab='Numero di variabili', ylab='BIC', type='l')
points(which.min(summary(m.forward)$bic),
       summary(m.forward)$bic[which.min(summary(m.forward)$bic)],
       col='red', pch=16)

#wich.min e wich .max ritornano le posizioni del minimo e del massimo
plot(summary(m.forward)$rsq, xlab='Numero di variabili', ylab='R2', type='l')
id<- which.max(summary(m.forward)$rsq)
points(id,summary(m.forward)$rsq[id],col='red',pch=16)


"""
una volta trovato il modello migliore procedo come per la regressione linere a spiego i ris

"""

modello.bic <- lm(Salary ~ AtBat + Hits + Walks + CRBI + Division + PutOuts,
                  data=hitters)
summary(modello.bic)

"""
R^2 suggerisce che comunque potrebbe migliorare ancora visto che è 0.5 ma è comunuque un buon ris
il p-value è molto basso praticamente 0 quindi il modello è buono.
Vediamo l'analisi dei residui

"""

par(mfrow=c(2,2))
plot(modello.bic, pch=16, cex=0.7)

"""
dall'analisi vediamo che cosi poterbbe andare abbastanza bene l'unico grfico che lacia un po'
così è il residual vx leverage

"""

m.backword <- regsubsets(Salary ~ .,data=hitters,nvmax=19,method='backward')
summary(m.backword)

par(mfrow=c(1,1))
plot(m.backword)

# possiamo notare che non si ottengono necessariamente gli stessi risultati del forward"

m.seqrep <- regsubsets(Salary ~ .,data=hitters,nvmax=19,method='seqrep')
plot(m.seqrep)
