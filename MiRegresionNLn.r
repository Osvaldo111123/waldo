MiRegresionNLn <- function(X,Y) {
  numvar <- length(X);
  suma <- 0;
  
  suma <- 0
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
  A[1,1] <- numvar;  A[1,2] <- Sx;   A[1,3] <- Sxx;
  A[2,1] <- Sx;      A[2,2] <- Sxx;  A[2,3] <- Sxxx;
  A[3,1] <- Sxx;    A[3,2] <- Sxxx;  A[3,3] <- Sxxxx
  b[1,1] <- Sy;      b[2,1] <- Sxy;   b[3,1] <- Sxxy;
  
  detA <- det(A)
  if(detA !=0){
    print('Tiene solucion')
    Xsol <- solve(A,b);
    print(Xsol)
  }else{
    print('No se puede hacer el ajuste')
  }
  a <- Xsol[1]
  b <- Xsol[2]
  c <- Xsol[3]
 
  
  resultado <- list(a0 =a, a1 =b, a2=c)
  return(resultado)
}
