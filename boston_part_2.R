rm(list=ls())

library(MASS)
data(Boston)
Boston[1:3,]

m <- lm(medv ~ lstat, data=Boston) 


summary(m)


names(m)


m$coefficients

residui <- residuals(m)

par(mfrow=c(2,2))

hist(residui, prob=TRUE)

plot(residui)

abline(h=0,col='green')


plot(Boston$lstat, residui, cex.lab=1.4, cex.axis=1.4) #residui in y lstat in x
abline(h=0,col='red')

plot(fitted(m), residui, cex.lab=1.4, cex.axis=1.4) #residui vs residui stimati
abline(h=0,col='blue')

r.standard = rstandard(m) #residui standard

plot(r.standard)
abline(h=0,col='violet')

plot(Boston$lstat, r.standard, cex.lab=1.4, cex.axis=1.4)
abline(h=0,col='red')
abline(h=-2, col='green')
abline(h=2, col='green')

par(mfrow=c(2,2))

plot(m)

qt(0.025, df=506-2)

#usando i quantili  ma perchè 0.0975 ??
-0.95005 - qt(0.975, df=506-2)*0.03873

-0.95005 + qt(0.975, df=506-2)*0.03873

"""
l'intervallo non comprende lo zero  e quindi rifiuto b1 = 0
"""
confint(m)

confint(m,	level=0.90)

statistica.t	<-	(-0.95005	-	(-1))/0.03873

pt(statistica.t,	df=506-2)

1-pt(statistica.t,	df=506-2)

2*(1-pt(statistica.t,	df=506-2))

predict(m,	newdata=data.frame(list(lstat=c(5,	10,	25))),interval='prediction')


plot(Boston$crim,	Boston$medv,	xlab='Crimine',	ylab='Prezzo')

m2	<-	lm(medv	~	lstat	+	crim,	data=Boston)

summary(m2)

