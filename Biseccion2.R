# METODO DE BISECCION

## parametros iniciales
rm(list = ls())
ecuacion<-function(x){x^3 - 7*x^2 + 14*x-6
}

##Grafica
Xg<-seq(0,4,0.1)
plot(Xg,ecuacion(Xg),type="l",col="blue",main="Biseccion"
     ,xlab = "x",ylab = "F(x)")
grid()

## Algoritmo
BS<-function(xi,xu,ecuacion,iMax,es){
  iteracion<-0
  ea<-100
  xr<-0
  
  while (ea>es&&iteracion<iMax) {
    xrOld<-xr
    xr<-(xu+xi)/2
    iteracion<-iteracion+1
    
    if (ecuacion(xr)!=0) {
      ea<-abs((xr-xrOld)/xr)*100 #%
    }
    test<-ecuacion(xi)*ecuacion(xr)
    if (test<0) {
      xu<-xr
    }else if (test>0) {
      xi<-xr
    }else{
      ea<-0
    }
  }
  abline(v=xr,h=0,col="red")
  cat("Valor aproximado de la ecuacion fue: ", xr,
      " con un error porcentual relativo de: ", ea,
      "% y ",iteracion," iteraciones \n")
  
  return(sprintf(xr, fmt = '%#.10f')) #Imprimir valores con 10 decimales de precision
}

BS(xi = 1,xu = 3.2,ecuacion = ecuacion,iMax = 100,es = 0.001)
