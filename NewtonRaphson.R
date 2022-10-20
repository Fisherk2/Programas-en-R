# METODO DE NEWTON - RAPHSON
install.packages("Deriv")
library(Deriv)
rm(list = ls())

## F(X)
f<-function(x){-x^2 + 1.8*x + 2.5}
## F'(X)
fdx<-Deriv(f,"x")

## Grafica
Xg<-seq(-5,10,0.1)
plot(Xg,f(Xg),type="l",col="blue",main="Newton - Raphson"
     ,xlab = "x",ylab = "F(x)")
grid()

## Algoritmo
NR<-function(x0, f, fdx , iMax, es){
  iteracion<-0
  ea<-100
  xr<-x0
  
  while (ea>es&&iteracion<iMax) {
    xrOld<-xr
    xr<-xrOld-f(xrOld)/fdx(xrOld)
    iteracion<-iteracion+1
    
    if (f(xr)!=0) {
      ea<-abs((xr-xrOld)/xr)*100 #%
    }
  }
  abline(v=xr,h=0,col="red")
  cat("Valor aproximado de la ecuacion fue: ", xr,
             " con un error porcentual relativo de: ", ea,
             "% y ",iteracion," iteraciones \n")
  
  return(sprintf(xr, fmt = '%#.10f')) #Imprimir valores con 10 decimales de precision
}

NR(x0 = 5,f = f,fdx = fdx,iMax = 5,es = 0.05)
