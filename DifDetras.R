# METODO DE DIFERENCIACION NUMERICA POR DETRAS 

## parametros iniciales
rm(list = ls())

f<-function(x){
  (14*x^2)+86
}

DfD<-function(f,x,h){
  fx<-(f(x)-f(x-h))/h
  
  cat("La derivada evaluada en ", x,
      " da como resultado: ", fx,
      "\n")
  return(fx) #Imprimir valores
}

DfD(f,1,0.00000000001)