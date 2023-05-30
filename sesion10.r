#Distribucion normal

numvar <- 150;
media <- 0;
desvest <- 1;
limites <- 5;
datos <- rnorm(numvar,media,desvest)
rango <- seq(-limites, limites, length.out = 100)
densidad <- dnorm(rango, mean=media, sd = desvest)

plot(rango,densidad, type = 'l', lwd=3,
     col='red', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 1 Distribucion normal')

hist(datos, breaks=30, add=TRUE, col = 'yellow',
     border = 'green', freq = FALSE)


numvar <- 120;
media <- 15;
desvest <- 2.5;
liminf <- 0
limitsup <- 2*media;
datos <- rnorm(numvar,media,desvest)
rango <- seq(-liminf, limitsup, length.out = 100)
densidad <- dnorm(rango, mean=media, sd = desvest)

plot(rango,densidad, type = 'l', lwd=3,
     col='red', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 1 Distribucion normal',
     ylim= c(0,0.2))

hist(datos, breaks=30, add=TRUE, col = 'yellow',
     border = 'green', freq = FALSE)

#------------------------------------------------------------------
#Distribucion exponencial

numvar <- 1500
lambda <- 0.6
valMax <- 10
datos <- rexp(numvar, lambda)
rango <- seq(0,valMax, length.out=100)
densidad <- dexp(rango, rate = lambda)

plot(rango,densidad, type = 'l', lwd=3,
     col='blue', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 2 Distribucion Exponencial',
     ylim= c(0,1))

hist(datos, breaks=30, add=TRUE, col = 'orange',
     border = 'green', freq = FALSE)


numvar <- 1500
lambda <- 0.5
valMax <- 10
datos <- rexp(numvar, lambda)
rango <- seq(0,valMax, length.out=100)
densidad <- dexp(rango, rate = lambda)

plot(rango,densidad, type = 'l', lwd=3,
     col='blue', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 2 Distribucion Exponencial',
     ylim= c(0,0.75))

hist(datos, breaks=30, add=TRUE, col = 'orange',
     border = 'green', freq = FALSE)

#----------------------------------------------------------------------------------
#Distribucion gamma

numvar <- 1500
a <- 2
b <- 0.5
valMax <- 15
datos <- rgamma(numvar, shape =a, rate=b)
rango <- seq(0,valMax, length.out=100)
densidad <- dgamma(rango, shape =a, rate=b)


plot(rango,densidad, type = 'l', lwd=3,
     col='pink', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 3 Distribucion Gamma',
     ylim= c(0,0.3))

hist(datos, breaks=30, add=TRUE, col = 'purple',
     border = 'green', freq = FALSE)


numvar <- 1500
a <- 5
b <- 0.5
valMax <- 35
datos <- rgamma(numvar, shape =a, rate=b)
rango <- seq(0,valMax, length.out=100)
densidad <- dgamma(rango, shape =a, rate=b)


plot(rango,densidad, type = 'l', lwd=3,
     col='pink', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 3 Distribucion Gamma')
    # , ylim= c(0,0.3))

hist(datos, breaks=30, add=TRUE, col = 'purple',
     border = 'green', freq = FALSE)

#------------------------------------------------------------------------------------------
#DISTRIBUCION BETA


numvar <- 1500
a <-2.5
c <- 5
valMax <- 1
datos <- rbeta(numvar, shape1=a, shape2=c)
rango <- seq(0,valMax, length.out=100)
densidad <- dbeta(rango, shape1=a, shape2=c)


plot(rango,densidad, type = 'l', lwd=3,
     col='pink', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 4 Distribucion Beta')
     #, ylim= c(0,0.3))

hist(datos, breaks=30, add=TRUE, col = 'cyan',
     border = 'green', freq = FALSE)

#------------------------------------------------------------------------------------------
#DISTRIBUCION cauchy


numvar <- 100
a <-0.5
b <- 5
liminf <- -60
limsup <- 60

datos <- rcauchy(numvar, location=a, scale=b)
rango <- seq(liminf,limsup, length.out=100)
densidad <- dcauchy(rango, location=a, scale=b)


plot(rango,densidad, type = 'l', lwd=3,
     col='pink', xlab = 'Valores a evaluar',
     ylab = 'Densidad', main = 'Ejemplo 5 Distribucion Cauchy')
#, ylim= c(0,0.3))

hist(datos, breaks=30, add=TRUE, col = 'cyan',
     border = 'green', freq = FALSE)





