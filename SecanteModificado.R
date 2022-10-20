# METODO DE LAS SECANTES (MODIFICADO)
rm(list = ls())

## F(X)
f<-function(x){-80+x^35}

## Grafica
Xg<-seq(-5,5,0.01)
plot(Xg,f(Xg),type="l",col="blue",main="Secantes (Modificado)"
     ,xlab = "x",ylab = "F(x)")
grid()

## Algoritmo
SC<-function(x0,delta,f,iMax,es){
  iteracion<-0
  ea<-100
  xr<-x0
  
  while (ea>es&&iteracion<iMax) {
    xrOld<-xr #xi+1=xr
    m<-delta/(f(xrOld+delta)-f(xrOld)) #Pendiente
    xr<-xrOld-m*f(xrOld) 
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

SC(x0 = 3.5, delta = 0.01, f = f, iMax = 1000, es = 0.01)
