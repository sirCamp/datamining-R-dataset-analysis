rm(list=ls())

dati <- read.csv2('/home/stefano/Repository/datamining-R-dataset-analysis/Gender_Discrimination.csv',sep=",")

dim(dati)
names(dati)
summary(dati)
is.factor(dati$Gender)#veridfico se R ha caito che p qualitativa

levels(dati$Gender) #livelli della var

boxplot(dati$Salary)
hist(dati$Salary)
pie(table(dati$Gender))
boxplot(dati$Salary ~ dati$Gender, col=c('pink', 'blue'))
boxplot(dati$Experience ~ dati$Gender, col=c('pink', 'blue'))
plot(dati$Experience, dati$Salary, xlab='Anni di esperienza', ylab='Stipendio annuale')

points(dati$Experience[dati$Gender=='Female'],dati$Salary[dati$Gender=='Female'],col='pink')
points(dati$Experience[dati$Gender=='Male'],dati$Salary[dati$Gender=='Male'],col='blue')
legend('topleft', pch=c(1,1), col=c('pink', 'blue'), legend=c('Female', 'Male'))


#modello

modello <- lm(Salary ~ Gender + Experience, data=dati)
summary(modello)

abline(53260, 1744.6, col='pink')
abline(53260+17020.6, 1744.6, col='blue')

coef(modello)

modello2 <- lm(Salary ~ Gender * Experience, data=dati)

summary(modello2)

coeff <- coef(modello2)
abline(coeff[1],coeff[3],col='pink',lwd=3,lty=2)
abline(coeff[1]+coeff[2],coeff[3]+coeff[4],col='blue',lwd=3,lty=2)

#previsione maschio con 25 anni di esp
coeff[1]+coeff[2]+coeff[3]*25+coeff[4]*25
#previsione donna conn 25 anni di esperienza
coeff[1]+coeff[3]*25

#comando autimatico
predict(modello2,	newdata=data.frame(list(Gender='Male',	Experience=25)))
predict(modello2,	newdata=data.frame(list(Gender='Female',	Experience=25)))

