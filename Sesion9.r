set.seed(123)
numvar <- 150;
X <- rnorm(numvar);      print(X)
a1 <- 1/4
a2 <- 2
Y <- a1*X+a2*X^2 +rnorm(numvar); print(Y)

plot(X,Y,main= 'Ejemplo de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'blue',
     type = 'p')

numvar <- length(X);
suma <- 0;

for(i in 1:numvar){suma <- suma +X[i]}
Sx <- suma;

suma <- 0;
for(i in 1:numvar){suma <- suma +Y[i]}
Sy <- suma;

suma <- 0;
for(i in 1:numvar){suma <- suma +(X[i])^2}
Sxx <- suma;

suma <- 0;
for(i in 1:numvar){suma <- suma +(X[i])^3}
Sxxx <- suma;

for(i in 1:numvar){suma <- suma + X[i]^4}
Sxxxx <-  suma

for(i in 1:numvar){suma <- suma + X[i]*Y[i]}
Sxy <-  suma

for(i in 1:numvar){suma <- suma + (X[i]^2)*Y[i]}
Sxxy <-  suma

A <- matrix(0,3,3)
b <- matrix(0,3,1)
A[1,1] <- numvar; A[1,2] <- Sx; A[1,3] <- Sxx;
A[2,1] <- Sx; A[2,2] <- Sxx; A[2,3] <- Sxxx;
A[3,1] <- Sxx; A[3,2] <- Sxxx; A[3,3] <- Sxxxx
b[1,1] <- Sy
b[2,1] <- Sxy
b[3,1] <- Sxxy;

print(A)
print(b)
det (A)
Xsol <- solve(A,b)
print(Xsol)

inv(A)

Ainv <- solve(A)
A%*%Ainv



source('C:/Users/Alumno/Desktop/Nueva carpeta/MiRegresionNLn.R')
Soluciones <- MiRegresionNLn(X,Y)

a0 <- Soluciones$a0
a1 <- Soluciones$a1
a2 <- Soluciones$a2
yest <- a0*a1*X+a2*X^2


plot(X,Y,main= 'Ejemplo de Regresion Lineal',
     xlab = 'Variable Independiente',
     ylab = 'Variable Dependiente',
     col = 'pink',
     type = 'p')
par(new = True)
plot(X,yest,col='red',type='p')

