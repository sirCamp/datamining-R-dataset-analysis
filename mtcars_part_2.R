library(ISLR)
data(Auto)
dim(Auto)

Auto[1:3,]

?Auto
"""
creo una variabile new.mpg che indica un alto o basso consumo di carburante tra 1 e 0 usando i valpori
che sono sopra o sotto la mediana di mpg
"""

valori.mediana <- median(Auto$mpg)
new.mpg <- rep(1,length(Auto$mpg))
new.mpg[Auto$mpg < valori.mediana] <- 0

print(new.mpg)

## creao il nuovo dataset

dati.auto <- data.frame(new.mpg=new.mpg, Auto[,c('displacement','horsepower','origin')])
is.factor(dati.auto$origin)
dati.auto$origin <- as.factor(dati.auto$origin)

### cambio il nome assegnandole il nome della nazione
levels(dati.auto$origin) <- c('America','Europa','Giappone')
par(mfrow=c(1,3))

boxplot(displacement~new.mpg,data=dati.auto,subset=dati.auto$origin=='America',main='America', xlab='efficienza',ylab='cilindrata')
boxplot(displacement~new.mpg,data=dati.auto,subset = dati.auto$origin=='Europa',main='Europa',xlab='efficienza',ylab='cilindrata')
boxplot(displacement~new.mpg,data=dati.auto,subset=dati.auto$origin=='Giappone',main='Giappone',xlabl='efficienza',ylabel='cilindrata')

# i diversi andamenti suggeriscono che ci possa essere una relazione tra displacement e origin.
# provo con horsepower e  origin

par(mfrow=c(1,3))

boxplot(horsepower~new.mpg,data=dati.auto,subset=dati.auto$origin=='America',main='America', xlab='efficienza',ylab='cavalli')
boxplot(horsepower~new.mpg,data=dati.auto,subset = dati.auto$origin=='Europa',main='Europa',xlab='efficienza',ylab='cavalli')
boxplot(horsepower~new.mpg,data=dati.auto,subset=dati.auto$origin=='Giappone',main='Giappone',xlabl='efficienza',ylabel='cavalli')

## è evidente che ci sia una relazione horsepower con roginn
## ora provo con  mosaicplot

mosaicplot(table(dati.auto$origin,dati.auto$new.mpg),las=1,cex.axis = 1,main='efficienza del carburante vs origin')

modello <- glm(new.mpg~displacement*origin+horsepower*origin,data=dati.auto,family=binomial)
summary(modello)


##calcolo l'andamento del modello con la devianza..(non torna)
1-pchisq(205.04,386)
## da chiedere non capisco

modello2 <- glm(new.mpg~displacement*origin+horsepower,data=dati.auto,family=binomial)
summary(modello2)

anova(modello2,modello,test='Chisq')

qchisq(0.95,2) #5.991465
1-pchisq(10.186,2) #0.006139574

## la devianza è decisamente + grande di 5.99 (10.186) il valore quindi va scartato il modello + piccolo
## ora calcolo i valori stimati
previsioni <- rep(0,nrow(dati.auto))
valori.stimati <- predict(modello)
prob.stimate <- predict(modello,type='response')
previsioni[prob.stimate>0.5] <- 1
print(previsioni)

addmargins(table(previsioni=previsioni,mpg=dati.auto$new.mpg))
stime <- coef(modello)

"""
ora proviamo a stimare efficienza prevista di un auto giapponese con displacemente=350 e horsepower=170
a meno. Uso i coefficienti giusti 1,2,4,5,7,9
"""
## (non mi torna il perchè sia cosi)
valore.previsto.giappone <- stime[1]+stime[2]*350+stime[4]+stime[5]*170+stime[7]*350+stime[9]*170

##prob prevista del giappone
prob.prevista.giappone <- exp(valore.previsto.giappone)/(1+exp(valore.previsto.giappone))
print(prob.prevista.giappone)
"""
il risultato applicando la soglia del 50% da come risultato new.mpg.1 e quindi efficienza sopra la media

"""

predict(modello,newdata=data.frame(horsepower=170,displacement=350,origin="Giappone"),type="response")

## il risutlato è identico a quello ottenuto manualmente