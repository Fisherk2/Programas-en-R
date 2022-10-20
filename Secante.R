# METODO DE LAS SECANTES
rm(list = ls())

## F(X)
f<-function(x){x^2-6}

## Grafica
Xg<-seq(-5,5,0.01)
plot(Xg,f(Xg),type="l",col="blue",main="Secantes"
     ,xlab = "x",ylab = "F(x)")
grid()

## Algoritmo
SC<-function(x0,x1,f,iMax,es){
  iteracion<-0
  ea<-100
  xr<-x1
  xrOld1<-x0
  
  while (ea>es&&iteracion<iMax) {
    xrOld0<-xrOld1 #xi-1 = xi+1
    xrOld1<-xr #xi+1=xr
    m<-(xrOld1-xrOld0)/(f(xrOld1)-f(xrOld0)) #Pendiente
    xr<-xrOld1-m*f(xrOld1) 
    iteracion<-iteracion+1
    
    if (f(xr)!=0) {
      ea<-abs((xr-xrOld1)/xr)*100 #%
    }
  }
  abline(v=xr,h=0,col="red")
  cat("Valor aproximado de la ecuacion fue: ", xr,
      " con un error porcentual relativo de: ", ea,
      "% y ",iteracion," iteraciones \n")
  
  return(sprintf(xr, fmt = '%#.10f')) #Imprimir valores con 10 decimales de precision
}

SC(x0 = 3, x1 = 2, f = f, iMax = 100, es = 0.00)
