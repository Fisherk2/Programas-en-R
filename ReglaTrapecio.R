# METODO DE INTEGRACION POR REGLA DE TRAPECIO

## parametros iniciales
rm(list = ls())
##DESVIACION ESTANDAR = 1, MEDIA= 0
ecuacion<-function(x){1/sqrt(2*pi)*exp(-x^2/2)}


## Algoritmo
TRM<-function(ecuacion,a,b,n,es){
  if (a>b) {### Sirve para acomodar los rangos de intervalos de una funcion [a,b]
    cambio<-a
    b<-cambio
    a<-b
  }
  
  dx<-(b-a)/n
  
  ##Grafica
  xg<-seq(a,b,dx)
  plot(xg,ecuacion(xg),type="l",col="blue",main="Regla Trapecio"
       ,xlab = "x",ylab = "F(x)")
  grid()
  for (i in 1:n) {
    segments(xg[i],0,xg[i+1],0,col = "green",lwd = 2)
    segments(xg[i+1],0,xg[i+1],ecuacion(xg[i+1]),col = "green",lwd = 2)
    segments(xg[i+1],ecuacion(xg[i+1]),xg[i],ecuacion(xg[i]),col = "green",lwd = 2)
  }
  
  
  ## SUMATORIA
  xi<-a
  sumatoria<-ecuacion(xi)#Se suma la primer altura
  for (i in 1:n-1) {
    xi<-xi+dx #Se va recorriendo intervalos para sumar su altura
    sumatoria<-sumatoria+2*ecuacion(xi)
  }
  sumatoria<-sumatoria+ecuacion(b)#Se suma la ultima altura
  area<-sumatoria*dx/2
  
  cat("El area bajo la curva fue de: ", area,
      " con un numero de subintervalos de: ", n,
      "intervalos \n")
  L<-integrate(ecuacion,1,3)
  return(L) #Imprimir valores con error absoluto
}

TRM(ecuacion,0,1,300,0)
