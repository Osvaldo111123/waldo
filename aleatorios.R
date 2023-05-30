#Distribucion Normal
numvar <- 100; media <- 0; varianza <- 1; a <- 5
#generar datos aleatorios de una distribucion normal
datos <- rnorm(numvar, mean = media, sd = varianza)
#calcular la densidad de la distribucion normal en un rango de valores
rango <- seq(-a,a, length.out = 100)
densidad <- dnorm(rango, mean = media, sd = varianza)
#Dibujar la grafica de la distribucion normal
plot(rango, densidad, type = "l", lwd = 2, xlab ="Valores", ylab = "Densidad",
     main = "Distribucion normal estandar")
#agregar los dartos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#-----------------------------------------------------------------------------
#Distribucion Exponencial
lambda <- 0.6; valMax <- 10;
rango <- seq(0, valMax, length.out = 100)
datos <- rexp(numvar, lambda);
densidad <- dexp(rango, rate = lambda);
#dibujar la grafica de la distribucion exponencial
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion exponencial")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#------------------------------------------------------------------------------
#Distribucion Gamma
a<- 2; b <- 0.5;
#generar datos aleatorios de una distribucion gamma
datos <- rgamma(numvar, shape = a, rate = b)
#calcular la densidad de la distribucion gamma en un rango de valores
rango <- seq(0, valMax, length.out = 100)
densidad <- dgamma(rango, shape = a, rate = b)
#dibujar la grafica de la distribucion gamma
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion Gamma")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#------------------------------------------------------------------------------
#Distribucion Beta
c <- 5 
datos <- rbeta(numvar, shape1 = a, shape2 = c)
#calcular la densidad de la distribucion beta en un rango de valores
rango <- seq(0,1, length.out = 100)
densidad <- dbeta(rango, shape1 = a, shape2 = c)
#dibujar la grafica de la distribucion Beta
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion Beta")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#------------------------------------------------------------------------------
#Distribucion Cauchy
numvar <- 150; a = 0; b = 1; valMax <- 10;
#generar datos aleatorios de una distribcuion de cauchy
datos <- rcauchy(numvar, location = a, scale = b)
#calcular la densidad de la distribucion de cauchy en un rango de valores
rango <- seq(-valMax, valMax, length.out = 100)
densidad <- dcauchy(rango, location = a, scale = b)
#dibujar la grafica de la distribucion Cauchy
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion Cauchy")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#------------------------------------------------------------------------------
#Distribucion T-Student
#genera datos aleatorios de una distribucion t de student
gl <- 5; datos <- rt(numvar, df = gl); valMax <- 5
#calcular la densidad de la distribucion t de strudent en un rango de valores
rango <- seq(-valMax, valMax, length.out = 100)
densidad <- dt(rango, df = gl)
#dibujar la grafica de la distribucion t de student
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion t de Student")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#------------------------------------------------------------------------------
#Distribucion Chi-cuadrada
gl <- 5;  numvar <- 150; valMax <- 20
#Genera datos aleatorios de una distribucion chi-cuadrada
datos <- rchisq(numvar, df = gl)
#calcular la densidad de la distribucion chi-cuadrada en un rango de valores
rango <- seq(0, valMax, length.out = 100)
densidad <- dchisq(rango, df = gl)
#dibujar la grafica de la distribucion chi-cuadrada
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion chi-cuadrada")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#------------------------------------------------------------------------------
#Definir la funcion de densidad de probabilidad
pdf_unif <- function(x){ifelse(x >= 0 & x <=1, 1,0)}
#genera datos de la variable aleatoria uniforme continua
set.seed(123) #establecer una semilla para la reproducibilidad
datos <- runif(1000, min = 0, max = 1)
#grafica la funcion de densidad de probabilidad y el histograma
par(mfrow = c(1,2)) #mostrar dos graficos en una fila
curve(pdf_unif, from = -0.5, to = 1.5, n = 1000, col = "red", lwd = 4,
      main = "Variable Aleatoria Uniforme Continua",
      xlab = "Valor de la variable aleatoria", ylab = "Densidad de probabilidad")
hist(datos, breaks = 20, add = TRUE, col = "lightblue", freq = FALSE)
#-------------------------------------------------------------------------------
par(mfrow = c(1,1)) #volver a la configuracion de graficos predeterminada
#-------------------------------------------------------------------------------
#todas en una sola ventana
par(mfrow = c(3,3))
#Distribucion Normal
numvar <- 100; media <- 0; varianza <- 1; a <- 5
#generar datos aleatorios de una distribucion normal
datos <- rnorm(numvar, mean = media, sd = varianza)
#calcular la densidad de la distribucion normal en un rango de valores
rango <- seq(-a,a, length.out = 100)
densidad <- dnorm(rango, mean = media, sd = varianza)
#Dibujar la grafica de la distribucion normal
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion normal estandar")
#Agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = "green", freq = FALSE)
#-------------------------------------------------------------------------------
#Distribucion Exponencial
lambda <- 0.6; valMax <- 10;
rango <- seq(0, valMax, length.out = 100)
datos <- rexp(numvar, lambda);
densidad <- dexp(rango, rate = lambda);
#dibujar la grafica de la distribucion exponencial
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion exponencial")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#-------------------------------------------------------------------------------
#Distribucion Gamma
a<- 2; b <- 0.5;
#generar datos aleatorios de una distribucion gamma
datos <- rgamma(numvar, shape = a, rate = b)
#calcular la densidad de la distribucion gamma en un rango de valores
rango <- seq(0, valMax, length.out = 100)
densidad <- dgamma(rango, shape = a, rate = b)
#dibujar la grafica de la distribucion gamma
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion Gamma")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#-------------------------------------------------------------------------------
#Distribucion Beta
c <- 5 
datos <- rbeta(numvar, shape1 = a, shape2 = c)
#calcular la densidad de la distribucion beta en un rango de valores
rango <- seq(0,1, length.out = 100)
densidad <- dbeta(rango, shape1 = a, shape2 = c)
#dibujar la grafica de la distribucion Beta
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion Beta")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#-------------------------------------------------------------------------------
#Distribucion Cauchy
numvar <- 150; a = 0; b = 1; valMax <- 10;
#generar datos aleatorios de una distribcuion de cauchy
datos <- rcauchy(numvar, location = a, scale = b)
#calcular la densidad de la distribucion de cauchy en un rango de valores
rango <- seq(-valMax, valMax, length.out = 100)
densidad <- dcauchy(rango, location = a, scale = b)
#dibujar la grafica de la distribucion Cauchy
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion Cauchy")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#-------------------------------------------------------------------------------
#Distribucion T-Student
#genera datos aleatorios de una distribucion t de student
gl <- 5; datos <- rt(numvar, df = gl); valMax <- 5
#calcular la densidad de la distribucion t de strudent en un rango de valores
rango <- seq(-valMax, valMax, length.out = 100)
densidad <- dt(rango, df = gl)
#dibujar la grafica de la distribucion t de student
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion t de Student")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#-------------------------------------------------------------------------------
#Distribucion Chi-cuadrada
gl <- 5;  numvar <- 150; valMax <- 20
#Genera datos aleatorios de una distribucion chi-cuadrada
datos <- rchisq(numvar, df = gl)
#calcular la densidad de la distribucion chi-cuadrada en un rango de valores
rango <- seq(0, valMax, length.out = 100)
densidad <- dchisq(rango, df = gl)
#dibujar la grafica de la distribucion chi-cuadrada
plot(rango, densidad, type = "l", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Distribucion chi-cuadrada")
#agregar los datos aleatorios a la grafica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border = 'green', freq = FALSE)
#-------------------------------------------------------------------------------
#Definir la funcion de densidad de probabilidad
pdf_unif <- function(x){ifelse(x >= 0 & x <=1, 1,0)}
#genera datos de la variable aleatoria uniforme continua
set.seed(123) #establecer una semilla para la reproducibilidad
datos <- runif(1000, min = 0, max = 1)
#grafica la funcion de densidad de probabilidad y el histograma
#par(mfrow = c(1,2)) #mostrar dos graficos en una fila
curve(pdf_unif, from = -0.5, to = 1.5, n = 1000, col = "red", lwd = 4,
      main = "Variable Aleatoria Uniforme Continua",
      xlab = "Valor de la variable aleatoria", ylab = "Densidad de probabilidad")
hist(datos, breaks = 20, add = TRUE, col = "lightblue", freq = FALSE)
#------------------------------------------------------------------------------#
#Distribucion Discretas
#Calcular la distribucion binomial
n <- 10; p <- 0.5; x <- 0:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
#graficar la distribucion binomial
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribucion Binomial", xlab ="Numero de exitos",
        ylab = "Probabilidad")
#--------------------------------------------------------------------------------
#calcular la distribucion binomial y la distribucion teorica
n <- 20; p <- 0.3; x <-0:n #valores posibles exitos
prob <- dbinom(x, size = n, prob = p) #probanilidad de cada valor x
mu <- n*p #media de la distribucion binomial
sigma <- sqrt(n*p*(1-p))#desviacion estandar de la distribucion binomial
x_teoria <- seq(0, n, length.out = 100) #valores de x para la distribucion teorica
prob_teoria <- dnorm(x_teoria, mean = mu, sd = sigma) #probabilidad 
#de cada valor de x_teoria segun la distribucion normal
#graficar la distribucion binomial y la distribucion teorica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribucion Binomial vs. Distribucion Normal",
        xlab = "Numero de exitos", ylab = "Probabilidad")
lines(x_teoria, prob_teoria, col= "red", lwd = 2)
#-------------------------------------------------------------------------------
#calcular la distribucion geometrica y la distribucion teorica
p <- 0.3; x <- 0:10 #valores posibles de ensayos antes del primer exito
prob <- dgeom(x, prob = p) #probablidad de cada valor de x
x_teoria <- seq(0,10, length.out = 100) #valores de x para la distribucion teorica
prob_teoria <- dgeom(x_teoria, prob = p) #probabilidad de cada valor de x_teoria
#segun la distribucion geometrica
#graficar la distribuicuon geometrica vs. Distribucion teorica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribucion Binomial vs. Distribucion Teorica",
        xlab = "Numero de ensayos antes del primer exito", ylab = "Probabilidad")
points(x_teoria, prob_teoria, col = "red", pch = 19)
#--------------------------------------------------------------------------------
#generar una muestra aleatoria de una vaiable geometrica
set.seed(123) #fijar la semilla para reproducibilidad
p <-0.3; n <- 1000; muestra <- rgeom(n, prob = p)
#calcular la distribucion teorica geometrica
x_teoria <- 0:10 #valores posibles de la variable aleatoria
prob_teoria <- dgeom(x_teoria, prob = p)# probabilidad de cada valor 
#de x_teoria segun la distribucion geometrica
#graficar el histograma y la distribucion teorica 
hist(muestra, prob =TRUE, col = "lightblue",
     main = "Variables Aleatoria Geometrica vs. Distribucion Teorica",
     xlab = "Valor de la variable aleatoria", ylab = "Densidad")
lines(x_teoria, prob_teoria, col= "red", lwd = 2)
#-------------------------------------------------------------------------------
#calcular la distribucion teorica Poisson
lambda <- 2 #parametro lambda de la distribucion poisson
x_teoria<- 0:10 #valores posibles de la variable aleatoria
prob_teoria <- dpois(x_teoria, lambda) #probabilidad de cada valor de x_teoria
#segun la distribucion poisson
#graficar el diagrama de barras y la distribucion teorica
barplot(prob_teoria, names.arg = x_teoria, col ="lightblue",
        main = "Distribucion Poisson vs. Distribucion Teorica",
        xlab = "Valor de la variable aleatoria", ylab = "Probabilidad")
lines(x_teoria, prob_teoria, col= "red", lwd = 2)
#-------------------------------------------------------------------------------
#definir los valores posibles de la variable aleatoria y sus probabilidades
x <- 1:5 #valores posibles de la variable aleatoria
prob <- rep(1/5,5)#probabilidad igual para cada valor
#graficar el diagrama de barras
barplot(prob, names.arg = x, col = "lightblue",
        main = "Variable Aleatoria Uniforme Discreta",
        xlab = "Valor de la variable aleatoria", ylab ="Probabilidad")
#-------------------------------------------------------------------------------#
#Todas en una sola ventana
par(mfrow = c(2,3))
#Distribucion Discretas
#Calcular la distribucion binomial
n <- 10; p <- 0.5; x <- 0:n #valores posibles de exitos
prob <- dbinom(x, size = n, prob = p) #probabilidad de cada valor de x
#graficar la distribucion binomial
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribucion Binomial", xlab ="Numero de exitos",
        ylab = "Probabilidad")
#-------------------------------------------------------------------------------
#calcular la distribucion binomial y la distribucion teorica
n <- 20; p <- 0.3; x <-0:n #valores posibles exitos
prob <- dbinom(x, size = n, prob = p) #probanilidad de cada valor x
mu <- n*p #media de la distribucion binomial
sigma <- sqrt(n*p*(1-p))#desviacion estandar de la distribucion binomial
x_teoria <- seq(0, n, length.out = 100) #valores de x para la distribucion teorica
prob_teoria <- dnorm(x_teoria, mean = mu, sd = sigma) #probabilidad 
#de cada valor de x_teoria segun la distribucion normal
#graficar la distribucion binomial y la distribucion teorica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribucion Binomial vs. Distribucion Normal",
        xlab = "Numero de exitos", ylab = "Probabilidad")
lines(x_teoria, prob_teoria,col= "red", lwd = 2)
#-------------------------------------------------------------------------------
#calcular la distribucion geometrica y la distribucion teorica
p <- 0.3; x <- 0:10 #valores posibles de ensayos antes del primer exito
prob <- dgeom(x, prob = p) #probablidad de cada valor de x
x_teoria <- seq(0,10, length.out = 100) #valores de x para la distribucion teorica
prob_teoria <- dgeom(x_teoria, prob = p) #probabilidad de cada valor de x_teoria
#segun la distribucion geometrica
#graficar la distribuicuon geometrica y la distribucion teorica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribucion Binomial vs. Distribucion Teorica",
        xlab = "Numero de ensayos antes del primer exito", ylab = "Probabilidad")
points(x_teoria, prob_teoria, col = "red", pch = 19)
#--------------------------------------------------------------------------------
#calcular la distribucion teorica Poisson
lambda <- 2 #parametro lambda de la distribucion poisson
x_teoria<- 0:10 #valores posibles de la variable aleatoria
prob_teoria <- dpois(x_teoria, lambda) #probabilidad de cada valor de x_teoria
#segun la distribucion poisson
#graficar el diagrama de barras y la distribucion teorica
barplot(prob_teoria, names.arg = x_teoria, col ="lightblue",
        main = "Distribucion Poisson vs. Distribucion Teorica",
        xlab = "Valor de la variable aleatoria", ylab = "Probabilidad")
lines(x_teoria, prob_teoria, col= "red", lwd = 2)
#--------------------------------------------------------------------------------
#definir los valores posibles de la variable aleatoria y sus probabilidades
x <- 1:5 #valores posibles de la variable aleatoria
prob <- rep(1/5,5)#probabilidad igual para cada valor
#graficar el diagrama de barras
barplot(prob, names.arg = x, col = "lightblue",
        main = "Variable Aleatoria Uniforme Discreta",
        xlab = "Valor de la variable aleatoria", ylab ="Probabilidad")



