library(MASS)

data(Boston)


names(Boston)



plot(Boston$lstat, Boston$medv)

m <- lm(medv ~ lstat,data=Boston)

summary(m)

m2 <- lm(medv ~ lstat + I(lstat^2), data=Boston)

summary(m2)

m3 <- lm(medv ~ poly(lstat,2), data=Boston)

summary(m3)

par(mfrow=c(2,2))

plot(m2)

1-pf(448.5,2,503)

rss1 <- 6.216^2*(504)

f <- (rss1 - rss2 )/rss2 * (504/1)
# f è alto e quindi dovrebbe essere rifiutato

qf(0.95,1,504)
"""
questo da come risutlato 3.859975 che è molto piu piccolo di 135
e quindi devo rifiutare il modello piu semplice secondo alpha 0.05.
DEvo sapere la prob con cui questo non deve essere cartato
"""

1 - pf(f,1,504)
