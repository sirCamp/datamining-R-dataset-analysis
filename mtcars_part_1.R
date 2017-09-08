rm(list=ls())

#loadDataset
data(mtcars)

dim(mtcars)

?mtcars

dati <- mtcars[,c('mpg','vs','am')]

#verifico load
names(dati)

dati[1:4,]

#verifico se R ha capito che è un fattore 'am'
is.factor(dati$am)

is.numeric(dati$am)

#è quantitativa ma a me serve qualitativa: converto

dati$am <- as.factor(dati$am)
is.factor(dati$am)

boxplot(dati$mpg ~ dati$vs)

boxplot(dati$mpg ~ dati$vs*dati$am)
## non si capiscono bene le interazioni ma sembra che am influenzi vs

modello <- glm(vs ~ mpg * am, data=dati,family=binomial)
summary(modello)

""" 
serve per dire che bisogna usare il logit e non il modello lienare classico.
(Dispersion parameter for binomial family taken to be 1) dice che c'è molta dispersione nei dati

La DEVIANZA è 2(log Lmax - Lmod) --> verosomiglianza massima - corrente
Inoltre DEVIANZA ~ X^2_n-p-1 (p = variabili, n = righe del modello)

La devianza NULL è troppo grande e anche quella Residua.

calcolo i quantili per capire se sono nella zona di rifiuto

"""

qchisq(0.95,31)

"""
ci da: 44.98534 > 43.860 che è la devianza pero' 43.86 ~ 44.98 e quindi siamo sulla soglia del rifuto
"""

1-pchisq(19.125,28)

"""
ci da un livello di significatività altissimo : 0.8942349

Number of Fisher Scoring iterations: 7
il fatto che ci siano 7 iterazioni ci dice che ci sono dati molto sparsi

"""

modello2 <- glm(vs~mpg+am, data=dati,family=binomial)
summary(modello2)

anova(modello2,modello)

anova(modello2,modello,test='Chisq')

#in teoria il modello proposto è migliore visto che è tendente allo 0

qchisq(0.95,1) #3.841459
1-pchisq(1.5214,1) #0.2174078

"""
1-pschisq(devianza,free degree) ci dice quanto è buono il modello.
più tende a 0 meglio è più tende a 1 peggio è.

inoltre la devianza a 1.52 è quasi metà dell limite dell'intervallo di specificità e quindi è sicuramente 
un modello migliore!!!
"""

#calcolo i coefficenti del modello

stime <- coef(modello2)
print(stime)
#-12.7051158   0.6809205  -3.0072739 

#calcolo i loro standard error
se <- sqrt(diag(vcov(modello2)))
print(se)

# a questo punto mi calcolo gli intervallli di confidenza per il coefficiente di mpg
int.2 <- c(stime[2]-qnorm((1+0.90)/2)*se[2],stime[2]+qnorm((1+0.90)/2)*se[2])
## è come scrivere: qnorm(0.95) che poi è confidenza 0.05
print(int.2)

confint.default(modello2)

##provo a semplificare ulteriormente il modello togliendo am

modello3 <-glm(vs~mpg, data=dati,family=binomial)
##volendo 
#modello3 <- update(modello2,.~.-am)
summary(modello3)

#cosa posso dire?? iterazioni ancora alte mpg semra essere un po + singificativa ma ordine 10^-4
# le varianze sono ancora molto alte provo a fare un confronto

anova(modello3,modello2,test='Chisq')
#calcolo il quantile
qchisq(0.95,1)
#siamo sopra, non dovrebbe andare bene in teoria ci dice che teniamo il modello 2
par(mfrow=c(2,2))
plot(modello2)

valori.stimati <- predict(modello2)
valori.probabili <- predict(modello2,type="response")
print(valori.stimati)
print(valori.probabili)


"""
proviamo a fare prob stimate sulla base di am
proviamo a fare l'inverso del logit' con grafico per am = 0 e per am = 1

"""
plot(dati$mpg,dati$vs,xlab='mpg',ylab='vs',main='Previsioni')
curve(predict(modello2,newdata=data.frame(mpg=x,am="0"),type='response'),add='TRUE')
curve(predict(modello2,newdata=data.frame(mpg=x,am="1"),type='response'),add='TRUE',lty=2)
legend(28,0.6,lty = c(1,2),legend=c('am 0','am 1'))

"""
provo ad effettiare delle previsioni
"""
previsioni <- rep(0,nrow(dati))
previsioni[valori.probabili > 0.5] <- 1

table(previsioni,dati$vs)
"""
da questo emerge che 15 sono 0 e vengono classificati come 0, 10 sono 1 vengono classificati come 1
7 sono sbagliti --|> 7 su 32 sono un errore dello 20% quindi altino.
Questo è un errore chiamato training error rate
testare dati al di fuoriu?
Prendo il test spezzo in 2. il 60% lo uso per stimare il mdoello e il 40% lo uso per fare i test
"""
n <- nrow(dati)
set.seed(222)
#definisco la selezione del campione
selezione <- sample(n, 0.60*n,replace=FALSE)
#deficnisco il tesset
test.set <- dati[-selezione,]
training.set <- dati[selezione,]

modello2.training <- glm(vs~mpg+am,data=training.set,family=binomial)

#una volta creato il modello provo a testarlo
summary(modello2.training)

prob.test <- predict(modello2.training,newdata=test.set,type='response')
previsioni.test <- rep(0,nrow(test.set))
previsioni.test[prob.test>0.5] <- 1
table(previsioni.test,test.set$vs)

"""
Il risultato dimostra che la percentuale di elementi corretti è minore.
Questpo può essere dovueto a due fatti principali:
test set troppo grande rispetto all'intero test
il seed mette elementi in modo casuiale excludendo elementi che potrebbero migliorare il ris
"""

#########################################################
#         USO LDA
#########################################################

library(MASS)

modello2.lda <- lda(vs~mpg+am,data=training.set)

print(modello2.lda)

## la funaizone discriminante quindi è:  0.28* mpg  + -0.65*am1

plot(modello2.lda)

modello2.lda$counts

previsioni.lda <- predict(modello2.lda,test.set)
print(previsioni.lda)

addmargins(table(previsioni.lda$class,test.set$vs))
## fa meno errori rispetto alla logit.
# provo comunque a fare e cercare tutte le previsioni con pro > 20%

previsioni.lda2 = rep(0,nrow(test.set))
previsioni.lda2[previsioni.lda$posterior[,2]>0.2] <- 1
addmargins(table(previsioni=previsioni.lda2,vs=test.set$vs))

#ora proviamo a fare la ROC

library(pROC)
valori.roc <- roc(test.set$vs,predictor=previsioni.lda$posterior[,2])
print(valori.roc)
      
names(valori.roc)
plot(valori.roc)
plot(valori.roc,legacy.axes=TRUE,print.auc=TRUE,auc.polygon = TRUE)

"""
ora provo a usare la discriminante quadratica --> binomiale -> lda -> qda
"""

modello2.qda <- qda(vs~mpg*am,data=training.set)
print(modello2.qda)

previsioni.qda <- predict(modello2.qda,test.set)
print(previsioni.qda)
addmargins(table(previsioni=previsioni.qda$class,test.set$vs))
## in termini di prestazioni sono uguali lda e qda (3/13 errori) quindi tengo lda che è + semplice
