# METODO DE LA FALSA POSICION

## parametros iniciales
rm(list = ls())
ecuacion<-function(X){y<-X^2-6
return(y)
}

##Grafica
Xg<-seq(-5,5,0.1)
plot(Xg,ecuacion(Xg),type="l",col="blue",main="Falsa Posicion"
     ,xlab = "x",ylab = "F(x)")
grid()


##Algoritmo
  FP<-function(xi,xu,ecuacion,errorPorcentualRelativo,topeIteraciones){
    iterador<-0
    ea<-100 #%
    xr<-0
    while (ea>errorPorcentualRelativo&&iterador<topeIteraciones) {
      xrOld<-xr
      xr<-xu-ecuacion(xu)*(xi-xu)/(ecuacion(xi)-ecuacion(xu))
      iterador<-iterador+1
      
      if(ecuacion(xr)!=0){
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
    return(cat("Valor aproximado de la ecuacion fue: ", xr,
               " con un error porcentual relativo de: ", ea,
               "% y ",iterador," iteraciones"))
  }
  
  FP(xi = 3,xu = 2,ecuacion = ecuacion,errorPorcentualRelativo = 0.0,topeIteraciones = 1000)
  
  