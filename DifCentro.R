# METODO DE DIFERENCIACION NUMERICA CENTRADA

## parametros iniciales
rm(list = ls())

f<-function(x){
  (32*x^2)-7
}

DfC<-function(f,x,h){
  fx<-(f(x+h)-f(x-h))/(2*h)
  
  cat("La derivada evaluada en ", x,
      " da como resultado: ", fx,
      "\n")
  return(fx) #Imprimir valores
}

DfC(f,1,0.00000000001)