library(ISLR)
data("Hitters")

hitters <- na.omit(Hitters)
dim(hitters)

names(hitters)
"""
Consideriamo un modello che spieghi lo stipendio stagionale in funzione delle altre variabili.
Per applicare i metodi ridge e lasso utilizziamo le funzioni contenute nella libreria
glmnet.
"""
install.packages('glmnet')
library(glmnet)

y <- hitters$Salary
X <- model.matrix(Salary ~ ., data=hitters)[,-1]
#trasformiamo tutto in matrice togliendo la colonna dell'intercetta, ovviamente!

# alpha a 0 identifica la regressione ridge
m.ridge <- glmnet(X, y, alpha=0)

print(m.ridge)

names(m.ridge)
length(m.ridge$lambda)
dim(m.ridge$beta)

plot(m.ridge, xvar='lambda', xlab=expression(log(lambda)))

# cv.glmnet() serve per fare validazione incrociata

set.seed(2906)

cv.ridge <- cv.glmnet(X,y,alpha = 0)

plot(cv.ridge, xlab=expression(log(lambda)))
lambda.min.ridge <- cv.ridge$lambda.min
cv.ridge$cvm[cv.ridge$lambda == lambda.min.ridge]

m.ridge.min <- glmnet(X, y, alpha=0, lambda=lambda.min.ridge)

print(m.ridge.min)

coef(m.ridge.min)


par(mfrow=c(1,2))
plot(m.ridge, xvar='lambda', xlab=expression(log(lambda)))
## aggiungiamo la retta in corrispondenza a lambda minimo
abline(v=log(lambda.min.ridge), lty=2)
## devianza
plot(log(m.ridge$lambda), m.ridge$dev.ratio, type='l',
     xlab=expression(log(lambda)), ylab='Devianza spiegata')
abline(v=log(lambda.min.ridge), lty=2)

lambda.ridge.1se <- cv.ridge$lambda.1se
abline(v=log(lambda.ridge.1se), lty=2, lwd=2)

##costruisco la variante lineare e vedo come procedere
m <- lm(Salary ~ ., data=hitters)
cbind(coef(m), coef(m.ridge.min))

# le intercette non sono mai da confrontare perchè sono di due modelli diversi 
min(cv.ridge$cvm)
cv.ridge$cvm[cv.ridge$lambda == lambda.ridge.1se]
plot(m.ridge$lambda, m.ridge$dev.ratio, type='l')
abline(v=lambda.min.ridge, lty=2, lwd=2)



########## PASSO AL LASSO 
par(mfrow=c(1,1))
m.lasso <- glmnet(X,y,alpha=1)
plot(m.lasso,xvar='lambda',xlab=expression(lof(lambda)))

set.seed(2906)
cv.lass <- cv.glmnet(X,y,alpha=1)
names(cv.lass)

abline(v=log(cv.lass$lambda.min),lty=2,lwd=2)
abline(v=log(cv.lass$lambda.1se),lty=2,lwd=3)


# proviamo a fare i conti leggermente diversi

set.seed(2096)
cv.lasso <- cv.glmnet(X,y,alpha=1,type.measure ='deviance')
min(m.lasso$cvm)
min(cv.ridge$cvm)

m.lasso.min <- glmnet(X, y, alpha=1, lambda=cv.lasso$lambda.min)

cbind(coef(m), coef(m.ridge.min), coef(m.lasso.min))

"""
i puntini dicoono che questo sono le variabili non utilizate

"""

m.selezione <- lm(Salary ~ AtBat+Hits+Walks+Years+CHmRun+CRuns+CRBI+CWalks+League+Division+PutOuts+Assists+Errors, data=hitters)
summary(m.selezione)

par(mfrow=c(1,2))

previsione.ridge <- predict(m.ridge.min, newx=X)

previsione.lasso <- predict(m.lasso.min, newx=X)

plot(hitters$Salary, previsione.ridge, pch=16,col='green')
abline(0,1)

plot(hitters$Salary, previsione.lasso, pch=16, col='red')
abline(0,1)
