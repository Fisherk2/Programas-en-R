# METODO DE ITERACION DE PUNTO FIJO

## f(x) original
ecuacion<-function(X){y<-2*X^3-11.7*X^2+17.7*X-5
return(y)
}
## g(x) = x
g<-function(X){
  y<-(5-2*X^3+11.7*X^2)/17.7
  return(y)
}

## Grafica
Xg<-seq(0,5,0.1)
plot(Xg,ecuacion(Xg),type="l",col="blue",main="Punto Fijo"
     ,xlab = "x",ylab = "F(x)")
grid()

## Algoritmo
PF<-function(x0, g, iMax, es){
  iteracion<-0
  ea<-100
  xr<-x0
  
  while (ea>es&&iteracion<iMax) {
    xrOld<-xr
    xr<-g(xrOld)
    iteracion<-iteracion+1
    
    if (g(xr)!=0) {
      ea<-abs((xr-xrOld)/xr)*100 #%
    }
  }
  abline(v=xr,h=0,col="red")
  return(cat("Valor aproximado de la ecuacion fue: ", xr,
             " con un error porcentual relativo de: ", ea,
             "% y ",iteracion," iteraciones"))
}

PF(x0 = 3,g = g,iMax = 3,es = 0)
