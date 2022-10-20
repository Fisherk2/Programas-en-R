# METODO DE INTEGRACION POR REGLA DE SIMPSON 1/3 (SOLO SIRVE CUANDO N ES PAR)

## parametros iniciales
rm(list = ls())
##DESVIACION ESTANDAR = 1, MEDIA= 0
ecuacion<-function(x){1/sqrt(2*pi)*exp(-x^2/2)}


## Algoritmo
SIMPSON<-function(ecuacion,a,b,n,es){
  if (a>b) {### Sirve para acomodar los rangos de intervalos de una funcion [a,b]
    cambio<-a
    b<-cambio
    a<-b
  }
  
  dx<-(b-a)/n
  
  ##Grafica
  xg<-seq(a,b,dx)
  plot(xg,ecuacion(xg),type="l",col="blue",main="Regla Simpson 1/3"
       ,xlab = "x",ylab = "F(x)")
  grid()
  
  for (i in 1:n) {
    segments(xg[i],0,xg[i+1],0,col = "green",lwd = 2)
    segments(xg[i+1],0,xg[i+1],ecuacion(xg[i+1]),col = "green",lwd = 2)
    segments(xg[i+1],ecuacion(xg[i+1]),xg[i],ecuacion(xg[i]),col = "green",lwd = 2)
  }
  
  xi<-a
  ## PRIMER TRAMO
  sumatoria<-ecuacion(xi)
  ## SUMATORIA CADA DOS TRAMOS
  for (i in 1:n-2) {
    if (i%%2!=0) { #si no es par la iteracion, debe multiplicar por 4
      xi<-xi+dx
      sumatoria = sumatoria + 4*ecuacion(xi)
    }else{ #si es par la iteracion, debe multiplicar por 2
      xi <- xi+dx
      sumatoria = sumatoria + 2*ecuacion(xi)
    }
  }
  ## ULTIMO TRAMO
  sumatoria = sumatoria + ecuacion(b)
  area = sumatoria*dx/3
  
  
  cat("El area bajo la curva fue de: ", area,
      " con un numero de subintervalos de: ", n,
      "intervalos \n")
  L<-integrate(ecuacion,1,3)
  return(L) #Imprimir valores con error absoluto
}

SIMPSON(ecuacion,0,1,300,0)
