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
               