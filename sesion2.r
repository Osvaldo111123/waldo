install.packages("dplyr")
install.packages("janitor")
library(dplyr)
library(janitor)

#Generar una matriz aleatoria de 7*7
#etiquetas las columnas, los renglones
n <- 7; m <- 7;
A<- matrix(0,n,n); print(A)

for(i in 1:n){
  for(j in 1:m){
    A[i,j] <- ceiling(runif(1,0,15)) #ceiling redondeo
    
  }
}
colnames (A) <- c('Uno', 'Dos','Tres','Cuatro','Cinco','Seis','Siete')
rownames(A) <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio')
print(A)
View(A)

#general una submatriz U que 
#1)contenga los primeros 3 renglones y las primeras 4 columnas
###1:3 renglones 1:4 columnas
A[1:3,1:4] 
#2)generar una matriz v que contenga las ultimas 4 columnas y los ultimos renglones
A[4:7,4:7]
#3)W que contenga las columnas impares
indices <- c(1,3,5,7)
A[,indices]
#4)R que contenga los renglones pares
ind <- c(2,4,6)
A[ind,]
#5)S que contenga renglones y columnas impares
A[indices,indices]
#6)T que contenga renglones y columnas pares
A[ind,ind]
#7)Q que contenga renglones pares y columnas impares
A[ind,indices]
#8)P que contenga renglones impares y columnas pares
A[indices,ind]
--------------------------------------------------------------------------------
  #ejercicio 2: generar 4 matrices aleatorias con valores 
  #entre -50 y 50 utilizando los comandos de: techo, piso, redondeo, truncamiento
  #comparar y hacer operaciones elementales
  
  x <- 5.64896  # Número positivo elegido
round(x, digits=3) #redondeo

n <- 4; m <- 4;
A1<- matrix(0,n,n); print(A)

for(i in 1:n){
  for(j in 1:m){
    A1[i,j] <- round(runif(1,-50,50)) #ceiling techo
    
  }
}
print(A1)
View(A1)
--------------------------------------------------------------------------------
  x <- 5.64896  # Número positivo elegido
ceiling(x) #techo
n <- 4; m <- 4;
A2<- matrix(0,n,n); print(A)

for(i in 1:n){
  for(j in 1:m){
    A2[i,j] <- ceiling(runif(1,-50,50)) #ceiling techo
    
  }
}
print(A2)
View(A2)
--------------------------------------------------------------------------------
  x <- 5.64896  # Número positivo elegido
floor(x) #piso
n <- 4; m <- 4;
A3<- matrix(0,n,n); print(A)

for(i in 1:n){
  for(j in 1:m){
    A3[i,j] <- floor(runif(1,-50,50)) #ceiling techo
    
  }
}
print(A3)
View(A3)
--------------------------------------------------------------------------------
  x <- 5.64896  # Número positivo elegido
trunc(x) #truncamiento
n <- 4; m <- 4;
A4<- matrix(0,n,n); print(A)

for(i in 1:n){
  for(j in 1:m){
    A4[i,j] <- floor(runif(1,-50,50)) #ceiling techo
    
  }
}
print(A4)
View(A4)
--------------------------------------------------------------------------------
  #ejercicio 3 generar una matriz diagonal NxN
  n <- 4; m <- 4;
A5<- matrix(0,n,n); print(A5)

for(i in 1:n){
  for(j in 1:m){
    if(i==j){
      A5[i,j] <- floor(runif(1.1,0,49.5)) #ceiling techo
    }
  }
}
print(A5)
View(A5)
--------------------------------------------------------------------------------
#vector aleatorio y sacar sus sumas de las entradas entrada
#sacar el promedio
m <- 15

x <- matrix(0,1,m)
for(i in 1:n){
  x[i] <- round(runif(1,0,100)); print(x1)
}

head(x)

suma <- 0
for(i in 1:m){
  suma <- suma + x[i]
}

xbarra <- suma/m; print(xbarra)

#varianza
suma2 <- 0
varianza <- 0
for(i in 1:m){
  termino <- x[i] -xbarra
  termino2 <- termino^2
  suma2 <- suma2 + termino2
}

varianza <- suma2/(n-1);print(varianza)


  
  
  
  
  