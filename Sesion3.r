#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
# SE SIMULA UNA BASE DE DATOS CON 150 REALIZACIONES EN CADA MUESTRA, Y 30 MUESTRAS.
#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
NumVar     <- 150   # numero de datos a generar
NumMuestra <- 30    # NUMERO DE MUESTRAS A CONSIDERAR
Datos <- matrix(0,NumVar,NumMuestra); head(Datos)
# A CONTINUACION SE SIMULA LA BASE DE DATOS
for(i in 1:NumMuestra){
  Datos[,i] <- round(runif(NumVar, 30,50));
}
View(Datos)
#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
# VAMOS A DETERMINAR LAS MEDIDAS DE TENDENCIA CENTRAL Y DE DISPERSION PARA CADA UNA DE LAS MUESTRAS
# MEDIA PARA LA PRIMERA MUESTRA
X1 <- Datos[,1]; head(X1)
suma <- 0
for(i in 1:NumVar){
  suma <- suma + X1[i]
}
PromedioX1 <- suma/NumVar
# AHORA VAMOS A CALCULAR LA MODA
contando <- table(X1); print(contando)
# ESTA INSTRUCCION CUENTA LAS VECES QUE SE REPITE CADA UNO DE LOS VALORES
ordenado <- sort(contando);
n <- length(ordenado); # CALCULAMOS EL NUMERO DE VARIABLES QUE SE TIENEN
modaX1 <- ordenado[n]
# PARA DETERMINAR LA MEDIANA RECORDEMOS QUE ES EL VALOR QUE SE ENCUENTRA UBICADO EXACTAMENTE
# EN LA MITAD, PARA ESO DETERMINAREMOS EL NUMERO DE DATOS Y DEPENDIENDO DE SI ES PAR O IMPAR SE 
# DETERMINARA LA MEDIANA
N <- length(X1)
# DETERMINEMOS SI LA CANTIDAD ES PAR O IMPAR
if( N%%2==0){
  print('ES UN NUMERO PAR DE DATOS')
  X1Ord <- sort(X1) # ESTA INSTRUCCION ORDENA DE MENOR A MAYOR.
  primero <- X1Ord[N/2]   # OBTENEMOS EL DATO QUE ESTA AL FINAL DE LOS PRIMEROS n/2 DATOS
  segundo <- X1Ord[N/2+1] # OBTENEMOS EL DATO QUE ESTA AL INICIO DE LOS SEGUNDOS n/2 DATOS
  medianaX1 <- (primero+segundo)/2
}else{
  X1Ord <- sort(X1) # ESTA INSTRUCCION ORDENA DE MENOR A MAYOR.
  medianaX1 <- X1Ord[N/2+1] # OBTENEMOS EL DATO QUE ESTA A LA MITAD DE LOS DATOS
}

MTC <- matrix(0,1,3)
MTC[1,1] <- PromedioX1;
MTC[1,2] <- medianaX1
MTC[1,3] <- modaX1
colnames(MTC) <- c('Media','Mediana','Moda'); print(MTC)
#rownames(MTC) < c('Muestra1')

# CALCULEMOS AHORA LA PRINCIPAL MEDIDA DE DISPERSION

suma <- 0;
for(i in 1:NumVar){
  termino1 <- X1[i]-PromedioX1
  termino12 <- termino1^2
  suma <- suma + termino12
}
Varianza <- suma/(NumVar-1)
DesvEst <- sqrt(Varianza)
#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
#Alternativamente
suma1 <-0;
for(i in 1:NumVar){
  termino <- x[i];
  termino2 <- termino^2
  suma1 <- suma1 + termino2
}
Ter1 <- suma1/NumVar
Varianza2 <- Ter1 -(PromedioX1)^2
DesvStd2 <- sqrt(Varianza2)
#<><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><><><> ===== <><>
MTC <- matrix(0, NumMuestra,3); colnames(MTC) <- c ('Media','Mediana','Moda');
for(i in 1:NumMuestra){
  x <- Datos[,i];
  suma <- 0
  for(j in 1 NumVar){
    suma <- suma + x[j]
  }
  PromedioX[i] <- suma/NumVar
  contando <- table(x); ordenado <- sort(contando);
  n<- length(ordenado);
modaX1[i] <- ordenado[n]; N <- length(x)
  if(N%%2==0){
    print('ES UN NUMERO PAR DE DATOS');
    x1ord <- sort(x);         primero <- x1ord[N/2]
    segundo <- x1ord[N/2+1];  medianaX[i] <- (primero+segundo)/2
  }else{
    x1ord <- sort (x); medianaX[i] <- x1ord[N/2+1]
  }
  MTC[i,1] <- PromedioX;  MTC[i,2] <- medianaX; MTC[i,3] <- modaX;

  suma <-0;
  for(j in 1:NumVar){
    termino1 <- x[j]-PromedioX; termino12
    suma <- suma + termino12
  }
  varianza[i] <- suma/(NumVar-1)
  DesvEst[i] <- sqrt(Varianza[i])
}
view(MTC)

MTD <- cbind(Varianza,DesvEst); View(MTD)
MatrizResultados <- cbin(MTC,MTD)
View(MatrizResultados)


