# METODO DE DIFERENCIACION NUMERICA POR DELANTE 

## parametros iniciales
rm(list = ls())

f<-function(x){
  (22*x^2)+1
}

DfA<-function(f,x,h){
  fx<-(f(x+h)-f(x))/h
  
  cat("La derivada evaluada en ", x,
      " da como resultado: ", fx,
      "\n")
  return(fx) #Imprimir valores
}

DfA(f,1,0.00000000001)

