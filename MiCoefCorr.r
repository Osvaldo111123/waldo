MiCoefCorr <- function(X,Y) {
  numvar <- length(X);
  suma <- 0;
  for(i in 1:numvar){suma <- suma + X[i]*Y[i]}
  Sxy <- numvar * suma
  
  suma <- 0
  for(i in 1:numvar){suma <- suma +X[i]}
  Sx <- suma;
  
  suma <- 0;
  for(i in 1:numvar){suma <- suma +Y[i]}
  Sy <- suma;
  
  suma <- 0;
  for(i in 1:numvar){suma <- suma +(X[i])^2}
  Sxx <- suma*numvar;
  
  suma <- 0;
  for(i in 1:numvar){suma <- suma +(Y[i])^2}
  Syy <- suma*numvar;
  
  Termino1 <- Sxy-Sx*Sy;
  Raiz1 <- sqrt(Sxx-(Sx^2))
  Raiz2 <-  sqrt(Syy-(Sy^2))
  Termino2 <- Raiz1*Raiz2
  Rxy <- Termino1/Termino2

 expr1 <- 'Perfecta'
 expr2 <- 'Fuerte'
 expr3 <- 'Significativa'
 expr4 <- 'Moderada'
 expr5 <- 'Debil'
 expr6 <- ' Muy debil'
 expr7 <- 'nula'
 
 Condicion1 <- (Rxy>=-1 && Rxy <= -0.96)||(Rxy<=1 && Rxy>=0.96)
 Condicion2 <- (Rxy>=-0.95 && Rxy <= -0.85)||(Rxy<=0.85 && Rxy>=0.95)
 Condicion3 <- (Rxy>=-0.84 && Rxy <= -0.7)||(Rxy<=0.7 && Rxy>=0.84)
 Condicion4 <- (Rxy>=-0.59 && Rxy <= -0.5)||(Rxy<=0.5 && Rxy>=0.59)
 Condicion5 <- (Rxy>=-0.49 && Rxy <= -0.2)||(Rxy<=0.2 && Rxy>=0.49)
 Condicion6 <- (Rxy>=-0.19 && Rxy <= -0.1)||(Rxy<=0.1 && Rxy>=0.19)
 Condicion7 <- (Rxy>=-0.09 && Rxy <= 0)||(Rxy<=0 && Rxy>=0.09)
 
  if(Condicion1){
  print(expr1); NivelCorr <- expr1
  }else if(Condicion2){
    print(expr2); NivelCorr <- expr2
  }else if(Condicion3){
    print(expr3); NivelCorr <- expr3
  }else if(Condicion4){
    print(expr4);NivelCorr <- expr4
  }else if(Condicion5){
    print(expr5);NivelCorr <- expr5
  }else if(Condicion6){
    print(expr6); NivelCorr <- expr6
  }else {
    print(expr7); NivelCorr <- expr7
  }
  
  resultado <- list(CoefCorr = Rxy, Nivel = NivelCorr)
  return(resultado)
}
