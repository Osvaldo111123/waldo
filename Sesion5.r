NumVar <- 150
numMuestras <- 30 
Datos <- matrix(0,NumVar, NumMuestras); View(Datos)

#------------------------------------------------------
for(i in 1:NumMuestras){
  Datos[,i] <- round(runif(NumVar,30,50))
}
View(Datos)
Ind <- abs(round(rnorm(150,24,5))); View(Ind)
MiBDD <- cbind(Ind,Datos); View(MiBDD)
#--------------------------------------------------------
head(MiBDD,5)
#--------------------------------------------------------
X <- MiBDD[,1]; head(X)
Y <- MiBDD[,2]; head(Y)

contando <- table(X); print(contando)
-------------------------------------------------------------
barplot(contando)

barplot(contando,col=rainbow(15,0.55), ylim = c(0,20))

barplot(contando,col=rainbow(15,0.55), ylim = c(0,17))

barplot(contando,col=rainbow(15,0.55), ylim = c(0,15), xlab = 'Indices', ylab = 'Frecuencias', main = 'Mi primer grafica')
grid(1.5,10)

barplot(contando,col=rainbow(15,0.55), ylim = c(0,15), xlim = c(-1,40), border ="red",xlab = 'Indices', ylab = 'Frecuencias', main = 'Mi primer grafica')
grid(1.5,10)
#-----------------------------------------------------------------
nbreaks = 15
hist(Y)
hist(Y, breaks = nbreaks,col=rainbow(15,0.55), main = 'Mediciones de tiempos de traslado', xlab = 'Frecuencias', ylab= 'Tiempos', ylim = c(0,15))

hist(Y, breaks = nbreaks,col="darkmagenta", main = 'Mediciones de tiempos de traslado', xlab = 'Frecuencias', ylab= 'Tiempos', ylim = c(0,15), fred=TRUE)


h <- hist(Y, breaks = nbreaks,col=rainbow(15,0.55), main = 'Mediciones de tiempos de traslado', xlab = 'Frecuencias', ylab= 'Tiempos', ylim = c(0,15))
summary(h)
text(h$mids, h$counts, labels =h$counts, adj=c(0.5, -0.5))
dev.off()
#crear archivo pdf el histograma
#----------------------------------------------------------------------
#simulacion de edades y tiempos de traslado

Edades<- rnorm(NumVar,22,1.5)
Ttras1 <-rnorm(NumVar,90,3.5)
plot(Edades, Ttras1)
#----------------------------------------------------------------------
MiBDD[,2]
y <- MiBDD[,3]
plot(x,y)

          
