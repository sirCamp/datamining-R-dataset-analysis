load('/home/stefano/Repository/datamining-R-dataset-analysis/Leukemia.RData')

#controllo se lista

is.list(Leukemia)

names(Leukemia)
"""
y è malato o no  x soo var

"""

length(Leukemia$y)
dim(Leukemia$x)

modello.glm <- glm(y ~ x, data=Leukemia, family = 'binomial')

summary(modello.glm)

"""
il modello non va bene , ma non posso usare la selezione automatica perchè si sn solo 72 dati

posso regolazizzare per schiacciare verso 0 o posso usare i componenti principali per ridurre il problema

"""

#vado di rideg
library(glmnet)

leukemia.ridge <- glmnet(Leukemia$x, Leukemia$y, alpha=0, family='binomial')
summary(leukemia.ridge$lambda)
plot(leukemia.ridge)

#provo a rifare con un lambda minimo:
leukemia.ridge <- glmnet(Leukemia$x, Leukemia$y, alpha=0, family='binomial', lambda.min=1e-4)
summary(leukemia.ridge$lambda)

#cerco il lambda migliore con la cross validation

par(mfrow=c(1,1))
set.seed(111)
cv.leukemia.ridge <- cv.glmnet(Leukemia$x,Leukemia$y, alpha=0, family='binomial', lambda.min=1e-4)

names(cv.leukemia)

print(cv.leukemia$lambda.min) # lamda minummo
print(cv.leukemia$lambda.1se) # labmda con distanza 1 standard error

plot(leukemia.ridge, xvar = 'lambda')
abline(v=log(cv.leukemia.ridge$lambda.min),lty=2,lwd=3) #non capisco bene come inerpretare il gafico

min(cv.leukemia.ridge$cv) #nn esiste
min(cv.leukemia.ridge$cvm)

leukemia.ridge.finale <- glmnet(Leukemia$x, Leukemia$y, alpha=0, family='binomial', lambda=cv.leukemia.ridge$lambda.min)
leukemia.ridge.finale

plot(leukemia.ridge)

names(leukemia.ridge)

plot(log(leukemia.ridge$lambda), leukemia.ridge$dev.ratio, type='l')
abline(v=log(cv.leukemia.ridge$lambda.min), lty=2, lwd=3)



#######################
# LASSO
#######################
leukemia.lasso <- glmnet(Leukemia$x, Leukemia$y, alpha=1,family='binomial',lambda.min = 1e-4)
plot(leukemia.lasso,xvar='lambda')

cv.leukemia.lasso <- cv.glmnet(Leukemia$x, Leukemia$y, alpha=1,family='binomial',lambda.min = 1e-4)
best.lambda.leukemia.lasso <- cv.leukemia.lasso$lambda.min
print(best.lambda.leukemia.lasso) #0.0099 che è molto basso
min(cv.leukemia.lasso$cvm) #0.287724



#comando piu8n veloce suggerisce che sia piu performante e ui coeffiucent abbiano minor riduzione
plot(cv.leukemia.lasso)
plot(leukemia.lasso, xvar='lambda')

abline(v=log(cv.leukemia.lasso$lambda.min),lty=2, lwd=3)
abline(v=log(cv.leukemia.lasso$lambda.1se),lty=2, lwd=3)

min(cv.leukemia.lasso$cvm)
min(cv.leukemia.ridge$cvm)


cv.leukemia.lasso$cvm[cv.leukemia.lasso$lambda==cv.leukemia.lasso$lambda.1se]
cv.leukemia.ridge$cvm[cv.leukemia.ridge$lambda==cv.leukemia.ridge$lambda.1se] ## serve?

max(leukemia.lasso$dev.ratio) #la mdevianza totale è SPIEGATA quasi al 100%

leukemia.lasso.finale <- glmnet(Leukemia$x, Leukemia$y, alpha=1, family='binomial', lambda=cv.leukemia.lasso$lambda.min)

leukemia.lasso.finale1se <- glmnet(Leukemia$x, Leukemia$y, alpha=1, family='binomial', lambda=cv.leukemia.lasso$lambda.1se)


id.zero <- which(coef(leukemia.lasso.finale)==0)
id.nonzero <- which(coef(leukemia.lasso.finale)!= 0)
c(length(id.zero),length(id.nonzero))

#3550 22
varnames <- rownames(coef(leukemia.lasso.finale))[id.nonzero]
values.nonzero <- coef(leukemia.lasso.finale)[id.nonzero]
names(values.nonzero) <- varnames

#ora provo con 1se

id.zero.1se <- which(coef(leukemia.lasso.finale1se)==0)
id.nonzero.1se <- which(coef(leukemia.lasso.finale1se)!=0)

c(length(id.zero.1se),length(id.nonzero.1se))

# ne mette a zero altri 5