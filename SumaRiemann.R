# METODO DE INTEGRACION POR SUMA DE RIEMANN

## parametros iniciales
rm(list = ls())
##DESVIACION ESTANDAR = 1, MEDIA= 0
ecuacion<-function(x){1/sqrt(2*pi)*exp(-x^2/2)}


##POR IZQUIERDA
SRI<-function(ecuacion,a,b,n,es){
  if (a>b) {### Sirve para acomodar los rangos de intervalos de una funcion [a,b]
    cambio<-a
    b<-cambio
    a<-b
  }
  dx<-(b-a)/n
  
  ##Grafica
  xg<-seq(a,b,dx)
  plot(xg,ecuacion(xg),type="l",col="blue",main="Suma Riemann por la izquierda"
       ,xlab = "x",ylab = "F(x)")
  grid()
  for (i in 1:n) {
    segments(xg[i],0,xg[i+1],0,col = "green",lwd = 2)
    segments(xg[i],0,xg[i],ecuacion(xg[i]),col = "green",lwd = 2)
    segments(xg[i+1],0,xg[i+1],ecuacion(xg[i]),col = "green",lwd = 2)
    segments(xg[i+1],ecuacion(xg[i]),xg[i],ecuacion(xg[i]),col = "green",lwd = 2)
  }
  
  
  ## SUMATORIA
  sumatoria<-0
  for (i in 0:n-1) {
    sumatoria<-sumatoria+ecuacion(a+i*dx)
  }
  area<-dx*sumatoria
  cat("El area bajo la curva fue de: ", area,
      " por la izquierda con un numero de subintervalos de: ", n,
      "intervalos \n")
  L<-integrate(ecuacion,1,3)
  return(L) #Imprimir valores con error absoluto
}

##POR DERECHA
SRD<-function(ecuacion,a,b,n,es){
  if (a>b) {### Sirve para acomodar los rangos de intervalos de una funcion [a,b]
    cambio<-a
    b<-cambio
    a<-b
  }
  dx<-(b-a)/n
  
  ##Grafica
  xg<-seq(a,b,dx)
  plot(xg,ecuacion(xg),type="l",col="blue",main="Suma Riemann por la derecha"
       ,xlab = "x",ylab = "F(x)")
  grid()
  for (i in 1:n) {
    segments(xg[i],0,xg[i+1],0,col = "red",lwd = 2)
    segments(xg[i+1],0,xg[i+1],ecuacion(xg[i+1]),col = "red",lwd = 2)
    segments(xg[i+1],ecuacion(xg[i+1]),xg[i],ecuacion(xg[i+1]),col = "red",lwd = 2)
    segments(xg[i],ecuacion(xg[i+1]),xg[i],0,col = "red",lwd = 2)
  }
  
  
  ## SUMATORIA
  sumatoria<-0
  for (i in 1:n) {
    sumatoria<-sumatoria+ecuacion(a+i*dx)
  }
  area<-dx*sumatoria
  cat("El area bajo la curva fue de: ", area,
      " por la derecha con un numero de subintervalos de: ", n,
      "intervalos \n")
  L<-integrate(ecuacion,1,3)
  return(L) #Imprimir valores con error absoluto
}

##CENTRADA
SRM<-function(ecuacion,a,b,n,es){
  if (a>b) {### Sirve para acomodar los rangos de intervalos de una funcion [a,b]
    cambio<-a
    b<-cambio
    a<-b
  }
  dx<-(b-a)/n
  
  ##Grafica
  xg<-seq(a,b,dx)
  plot(xg,ecuacion(xg),type="l",col="blue",main="Suma Riemann por la derecha"
       ,xlab = "x",ylab = "F(x)")
  grid()
  
  ## SUMATORIA
  sumatoria<-0
  for (i in 1:n) {
    sumatoria<-sumatoria+ecuacion(a+(2*i-1)*dx/2)
  }
  area<-dx*sumatoria
  cat("El area bajo la curva fue de: ", area,
      "por en medio con un numero de subintervalos de: ", n,
      "intervalos \n")
  L<-integrate(ecuacion,1,3)
  return(L) #Imprimir valores con error absoluto
}
SRI(ecuacion,0,1,300,0)
SRD(ecuacion,0,1,300,0)
SRM(ecuacion,0,1,300,0)

