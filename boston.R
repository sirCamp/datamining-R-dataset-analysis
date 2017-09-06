rm(list=ls())

library(MASS)
data(Boston)

y <- Boston$medv
x <- Boston$lstat
n <- nrow(Boston)

summary(y)
hist(y, prob=TRUE, xlab='Prezzo mediano delle abitazioni', ylab='Densita di frequenza')


#distribuzione boxplot ( sembra che ci sinao piu elementi tra 1 quartile e mediana )
boxplot(y)


#distribuzione modello
plot(x,y, pch=19)

modello <- lm (y ~ x, data=Boston)

summary(modello)

#aggiunge linea rossa  della retta del grafico che già esisteva (intercetta con inclunazione b)
abline(a=modello$coefficients[1], b=modello$coefficients[2], col='red', lwd=2)

#valori stimati che poi vengon stampati con delle x
valori.stimati <- modello$fitted.values

points(x, valori.stimati, col='green', pch='x')