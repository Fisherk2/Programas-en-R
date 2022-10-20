#METODO DE BISECCION

##parametros iniciales
rm(list = ls())
x1<-1
x2<-3.2
precision<-0.99
ecuacion<-function(X){
  X^3 - 7*X^2 + 14*X - 6 ### ECUACION PROPUESTA
}
iterador<-1
tope<-1000
xAprox<-(x1+x2)/2
error <-abs(ecuacion(xAprox))

##algoritmo
while (error>(1-precision)){ ###error>0.01
  cambio<-xAprox
  
  ifelse(ecuacion(x1)*ecuacion(xAprox)<0,x2<-cambio,x1<-cambio)
  
  xAprox<-(x1+x2)/2 ###Se vuelve a calcular para la proxima iteracion
  error <-abs(ecuacion(xAprox)) ###Se vuelve a calcular para la proxima iteracion
  
  if (iterador==tope) { ###Rompemos el ciclo al tope de iteraciones
    break
  }
  
  iterador<-iterador+1
}  
cat("Valor aproximado de la ecuacion fue: ", xAprox," con un error de: ", ecuacion(xAprox)," y ",iterador," iteraciones")

## grafica
Xg<-seq(-10,10,1)
plot(Xg,ecuacion(Xg),type="l",col="blue",main="Biseccion"
     ,xlab = "Dominio",ylab = "Rango")
abline(v=xAprox,h=0,col="red")
grid()




