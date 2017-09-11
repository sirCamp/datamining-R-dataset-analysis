id.zero	<-	which	(coef(m.lasso.min)==0)
length(id.zero)
library(pls)
data(gasoline)
y <- gasoline$octane
x <- gasoline$NIR
dim(x)


m.pcr <- pcr(y~x, scale=TRUE,ncomp=5)

names(m.pcr)

install.packages('glmnet')
rm(list=ls())
library(glmnet)

set.seed(222)

cv.ridge <- cv.glmnet(x,y, alpha=0,lambda.min=1e-4)

cv.ridge$lambda.min
cv.ridge$lambda.1se


m.ridge.min <- glmnet(x,y,alpha=0,lambda.min=cv.ridge$lambda.min)
min(cv.ridge$cvm) #questo è il minimo errore quadratico medio

set.seed(222)
cv.lasso <- cv.glmnet(x,y,alpha=1,lambda.min = 1e-4)
cv.lasso$lambda.min

m.lasso.min <- glmnet(x,y,alpha=1,lambda.min=cv.lasso$lambda.min)
min(cv.lasso$cvm)

print(c(min(cv.ridge$cvm),min(cv.lasso$cvm)))

id.zero	<-	which	(coef(m.lasso.min)==0)
length(id.zero)

id.nonzero <- which(coef(m.lasso.min) != 0)
length(id.zero)
