# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Normal
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
numvar <- 100;  media <- 0;  varianza <- 1; a <- 5
# Generar datos aleatorios de una distribución normal
datos <- rnorm(numvar, mean = media, sd = varianza)
# Calcular la densidad de la distribución normal en un rango de valores
rango <- seq(-a, a, length.out = 100)
densidad <- dnorm(rango, mean = media, sd = varianza)
# Dibujar la gráfica de la distribución normal
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución normal estándar")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border= 'green',freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Exponencial
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
lambda <- 0.6; ValMax <- 10;
rango <- seq(0, ValMax, length.out = 100)
datos <- rexp(numvar, lambda);
densidad <- dexp(rango, rate = lambda);
# Dibujar la gráfica de la distribución exponencial
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución exponencial")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green",
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Gamma
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
a <- 2;  b <- 0.5
# Generar datos aleatorios de una distribución gamma
datos <- rgamma(numvar, shape = a, rate = b)
# Calcular la densidad de la distribución gamma en un rango de valores
rango <- seq(0, ValMax, length.out = 100)
densidad <- dgamma(rango, shape = a, rate = b)
# Dibujar la gráfica de la distribución gamma
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución gamma")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green", 
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribución Beta
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
c <- 5
datos <- rbeta(numvar, shape1 = a, shape2 = c)
# Calcular la densidad de la distribución beta en un rango de valores
rango <- seq(0, 1, length.out = 100)
densidad <- dbeta(rango, shape1 = a, shape2 = c)
# Dibujar la gráfica de la distribución beta
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución beta")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green", 
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Cauchy
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
numvar <- 150; a = 0; b = 1; ValMax <- 10
# Generar datos aleatorios de una distribución de Cauchy
datos <- rcauchy(numvar, location = a, scale = b)
# Calcular la densidad de la distribución de Cauchy en un rango de valores
rango <- seq(-ValMax, ValMax, length.out = 100)
densidad <- dcauchy(rango, location = a, scale = b)
# Dibujar la gráfica de la distribución de Cauchy
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución de Cauchy")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green",
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion T-Student
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Generar datos aleatorios de una distribución t de Student
gl <- 5; datos <- rt(numvar, df = gl); ValMax <- 5
# Calcular la densidad de la distribución t de Student en un rango de valores
rango <- seq(-ValMax, ValMax, length.out = 100)
densidad <- dt(rango, df = gl)
# Dibujar la gráfica de la distribución t de Student
plot(rango, densidad, type = "l", lwd = 2, xlab = "Valores", ylab = "Densidad",
     main = "Distribución t de Student")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green", 
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Chi-cuadrada
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
gl - 5;  numvar <- 150; ValMax <- 20
# Generar datos aleatorios de una distribución chi-cuadrada
datos <- rchisq(numvar, df = gl)
# Calcular la densidad de la distribución chi-cuadrada en un rango de valores
rango <- seq(0, ValMax, length.out = 100)
densidad <- dchisq(rango, df = gl)
# Dibujar la gráfica de la distribución chi-cuadrada
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución chi-cuadrada")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green",
     add=TRUE, freq = FALSE)
# Definir la función de densidad de probabilidad
pdf_unif <- function(x) {ifelse(x >= 0 & x <= 1, 1, 0)}
# Generar datos de la variable aleatoria uniforme continua
set.seed(123)  # establecer una semilla para la reproducibilidad
datos <- runif(1000, min = 0, max = 1)
# Graficar la función de densidad de probabilidad y el histograma
par(mfrow = c(1,2))  # mostrar dos gráficos en una fila
curve(pdf_unif, from = -0.5, to = 1.5, n = 1000, col = "blue",
      main = "Variable Aleatoria Uniforme Continua",
      xlab = "Valor de la variable aleatoria", ylab = "Densidad de probabilidad")
hist(datos, breaks = 20, col = "lightblue", freq = FALSE, add = TRUE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
par(mfrow = c(1,1))  # volver a la configuración de gráficos predeterminada
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Todas en una sola ventana
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
par(mfrow = c(3,3))
# Distribucion Normal
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
numvar <- 100;  media <- 0;  varianza <- 1; a <- 5
# Generar datos aleatorios de una distribución normal
datos <- rnorm(numvar, mean = media, sd = varianza)
# Calcular la densidad de la distribución normal en un rango de valores
rango <- seq(-a, a, length.out = 100)
densidad <- dnorm(rango, mean = media, sd = varianza)
# Dibujar la gráfica de la distribución normal
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución normal estándar")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, add = TRUE, col = "yellow",
     border= 'green',freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Exponencial
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
lambda <- 0.6; ValMax <- 10;
rango <- seq(0, ValMax, length.out = 100)
datos <- rexp(numvar, lambda);
densidad <- dexp(rango, rate = lambda);
# Dibujar la gráfica de la distribución exponencial
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución exponencial")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green",
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Gamma
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
a <- 2;  b <- 0.5
# Generar datos aleatorios de una distribución gamma
datos <- rgamma(numvar, shape = a, rate = b)
# Calcular la densidad de la distribución gamma en un rango de valores
rango <- seq(0, ValMax, length.out = 100)
densidad <- dgamma(rango, shape = a, rate = b)
# Dibujar la gráfica de la distribución gamma
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución gamma")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green", 
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribución Beta
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
c <- 5
datos <- rbeta(numvar, shape1 = a, shape2 = c)
# Calcular la densidad de la distribución beta en un rango de valores
rango <- seq(0, 1, length.out = 100)
densidad <- dbeta(rango, shape1 = a, shape2 = c)
# Dibujar la gráfica de la distribución beta
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución beta")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green", 
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Cauchy
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
numvar <- 150; a = 0; b = 1; ValMax <- 10
# Generar datos aleatorios de una distribución de Cauchy
datos <- rcauchy(numvar, location = a, scale = b)
# Calcular la densidad de la distribución de Cauchy en un rango de valores
rango <- seq(-ValMax, ValMax, length.out = 100)
densidad <- dcauchy(rango, location = a, scale = b)
# Dibujar la gráfica de la distribución de Cauchy
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución de Cauchy")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green",
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion T-Student
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Generar datos aleatorios de una distribución t de Student
gl <- 5; datos <- rt(numvar, df = gl); ValMax <- 5
# Calcular la densidad de la distribución t de Student en un rango de valores
rango <- seq(-ValMax, ValMax, length.out = 100)
densidad <- dt(rango, df = gl)
# Dibujar la gráfica de la distribución t de Student
plot(rango, densidad, type = "l", lwd = 2, xlab = "Valores", ylab = "Densidad",
     main = "Distribución t de Student")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green", 
     add=TRUE, freq = FALSE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribucion Chi-cuadrada
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
gl - 5;  numvar <- 150; ValMax <- 20
# Generar datos aleatorios de una distribución chi-cuadrada
datos <- rchisq(numvar, df = gl)
# Calcular la densidad de la distribución chi-cuadrada en un rango de valores
rango <- seq(0, ValMax, length.out = 100)
densidad <- dchisq(rango, df = gl)
# Dibujar la gráfica de la distribución chi-cuadrada
plot(rango, densidad, type = "l", lwd = 2, 
     xlab = "Valores", ylab = "Densidad",
     main = "Distribución chi-cuadrada")
# Agregar los datos aleatorios a la gráfica
hist(datos, breaks = 30, col= "yellow", border="green",
     add=TRUE, freq = FALSE)
# Definir la función de densidad de probabilidad
pdf_unif <- function(x) {ifelse(x >= 0 & x <= 1, 1, 0)}
# Generar datos de la variable aleatoria uniforme continua
set.seed(123)  # establecer una semilla para la reproducibilidad
datos <- runif(1000, min = 0, max = 1)
# Graficar la función de densidad de probabilidad y el histograma
#par(mfrow = c(1,2))  # mostrar dos gráficos en una fila
curve(pdf_unif, from = -0.5, to = 1.5, n = 1000, col = "blue",
      main = "Variable Aleatoria Uniforme Continua",
      xlab = "Valor de la variable aleatoria", ylab = "Densidad de probabilidad")
hist(datos, breaks = 20, col = "lightblue", freq = FALSE, add = TRUE)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Distribuciones Discretas
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Calcular la distribución binomial
n <- 10; p <- 0.5; x <- 0:n  # valores posibles de éxitos
prob <- dbinom(x, size = n, prob = p)  # probabilidad de cada valor de x
# Graficar la distribución binomial
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribución Binomial", xlab = "Número de éxitos",
        ylab = "Probabilidad")
# Calcular la distribución binomial y la distribución teórica
n <- 20; p <- 0.3; x <- 0:n  # valores posibles de éxitos
prob <- dbinom(x, size = n, prob = p)  # probabilidad de cada valor de x
mu <- n * p  # media de la distribución binomial
sigma <- sqrt(n * p * (1 - p))  # desviación estándar de la distribución binomial
x_teoria <- seq(0, n, length.out = 100)  # valores de x para la distribución teórica
prob_teoria <- dnorm(x_teoria, mean = mu, sd = sigma)  # probabilidad de cada valor de x_teoria
# según la distribución normal
# Graficar la distribución binomial y la distribución teórica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribución Binomial vs. Distribución Normal",
        xlab = "Número de éxitos", ylab = "Probabilidad")
lines(x_teoria, prob_teoria, col = "red", lwd = 2)
# Calcular la distribución geométrica y la distribución teórica
p <- 0.3; x <- 0:10  # valores posibles de ensayos antes del primer éxito
prob <- dgeom(x, prob = p)  # probabilidad de cada valor de x
x_teoria <- seq(0, 10, length.out = 100)  # valores de x para la distribución teórica
prob_teoria <- dgeom(x_teoria, prob = p)  # probabilidad de cada valor de x_teoria según 
# la distribución geométrica
# Graficar la distribución geométrica y la distribución teórica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribución Geométrica vs. Distribución Teórica",
        xlab = "Número de ensayos antes del primer éxito", ylab = "Probabilidad")
points(x_teoria, prob_teoria, col = "red", pch = 19)
# Generar una muestra aleatoria de una variable geométrica
set.seed(123)  # fijar la semilla para reproducibilidad
p <- 0.3; n <- 1000; muestra <- rgeom(n, prob = p)
# Calcular la distribución teórica geométrica
x_teoria <- 0:10  # valores posibles de la variable aleatoria
prob_teoria <- dgeom(x_teoria, prob = p)  # probabilidad de cada valor de x_teoria según 
# la distribución geométrica
# Graficar el histograma y la distribución teórica
hist(muestra, prob = TRUE, col = "lightblue",
     main = "Variable Aleatoria Geométrica vs. Distribución Teórica",
     xlab = "Valor de la variable aleatoria", ylab = "Densidad")
lines(x_teoria, prob_teoria, col = "red")
# Calcular la distribución teórica Poisson
lambda <- 2  # parámetro lambda de la distribución Poisson
x_teoria <- 0:10  # valores posibles de la variable aleatoria
prob_teoria <- dpois(x_teoria, lambda)  # probabilidad de cada valor de x_teoria según 
# la distribución Poisson
# Graficar el diagrama de barras y la distribución teórica
barplot(prob_teoria, names.arg = x_teoria, col = "lightblue",
        main = "Distribución Poisson vs. Distribución Teórica",
        xlab = "Valor de la variable aleatoria", ylab = "Probabilidad")
lines(x_teoria, prob_teoria, col = "red")
# Definir los valores posibles de la variable aleatoria y sus probabilidades
x <- 1:5  # valores posibles de la variable aleatoria
prob <- rep(1/5, 5)  # probabilidad igual para cada valor
# Graficar el diagrama de barras
barplot(prob, names.arg = x, col = "lightblue",
        main = "Variable Aleatoria Uniforme Discreta",
        xlab = "Valor de la variable aleatoria", ylab = "Probabilidad")
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# Todas en una sola ventana
par(mfrow = c(2,3))
# Calcular la distribución binomial
n <- 10; p <- 0.5; x <- 0:n  # valores posibles de éxitos
prob <- dbinom(x, size = n, prob = p)  # probabilidad de cada valor de x
# Graficar la distribución binomial
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribución Binomial", xlab = "Número de éxitos",
        ylab = "Probabilidad")
# Calcular la distribución binomial y la distribución teórica
n <- 20; p <- 0.3; x <- 0:n  # valores posibles de éxitos
prob <- dbinom(x, size = n, prob = p)  # probabilidad de cada valor de x
mu <- n * p  # media de la distribución binomial
sigma <- sqrt(n * p * (1 - p))  # desviación estándar de la distribución binomial
x_teoria <- seq(0, n, length.out = 100)  # valores de x para la distribución teórica
prob_teoria <- dnorm(x_teoria, mean = mu, sd = sigma)  # probabilidad de cada valor de x_teoria 
#según la distribución normal
# Graficar la distribución binomial y la distribución teórica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribución Binomial vs. Distribución Normal",
        xlab = "Número de éxitos", ylab = "Probabilidad")
lines(x_teoria, prob_teoria, col = "red", lwd = 2)
# Calcular la distribución geométrica y la distribución teórica
p <- 0.3; x <- 0:10  # valores posibles de ensayos antes del primer éxito
prob <- dgeom(x, prob = p)  # probabilidad de cada valor de x
x_teoria <- seq(0, 10, length.out = 100)  # valores de x para la distribución teórica
prob_teoria <- dgeom(x_teoria, prob = p)  # probabilidad de cada valor de x_teoria según la 
#distribución geométrica
# Graficar la distribución geométrica y la distribución teórica
barplot(prob, names.arg = x, col = "lightblue",
        main = "Distribución Geométrica vs. Distribución Teórica",
        xlab = "Número de ensayos antes del primer éxito", ylab = "Probabilidad")
points(x_teoria, prob_teoria, col = "red", pch = 19)
# Generar una muestra aleatoria de una variable geométrica
set.seed(123)  # fijar la semilla para reproducibilidad
p <- 0.3; n <- 1000; muestra <- rgeom(n, prob = p)
# Calcular la distribución teórica geométrica
x_teoria <- 0:10  # valores posibles de la variable aleatoria
prob_teoria <- dgeom(x_teoria, prob = p)  # probabilidad de cada valor de x_teoria según la 
#distribución geométrica
# Graficar el histograma y la distribución teórica
hist(muestra, prob = TRUE, col = "lightblue",
     main = "Variable Aleatoria Geométrica vs. Distribución Teórica",
     xlab = "Valor de la variable aleatoria", ylab = "Densidad")
lines(x_teoria, prob_teoria, col = "red")
# Calcular la distribución teórica Poisson
lambda <- 2  # parámetro lambda de la distribución Poisson
x_teoria <- 0:10  # valores posibles de la variable aleatoria
prob_teoria <- dpois(x_teoria, lambda)  # probabilidad de cada valor de x_teoria según la 
#distribución Poisson
# Graficar el diagrama de barras y la distribución teórica
barplot(prob_teoria, names.arg = x_teoria, col = "lightblue",
        main = "Distribución Poisson vs. Distribución Teórica",
        xlab = "Valor de la variable aleatoria", ylab = "Probabilidad")
lines(x_teoria, prob_teoria, col = "red")
# Definir los valores posibles de la variable aleatoria y sus probabilidades
x <- 1:5  # valores posibles de la variable aleatoria
prob <- rep(1/5, 5)  # probabilidad igual para cada valor
# Graficar el diagrama de barras
barplot(prob, names.arg = x, col = "lightblue",
        main = "Variable Aleatoria Uniforme Discreta",
        xlab = "Valor de la variable aleatoria", ylab = "Probabilidad")
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
