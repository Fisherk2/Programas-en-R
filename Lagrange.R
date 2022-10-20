# METODO DE DERIVACION POR MEDIO DE INTERPOLACION DE LAGRANGE (no hay funcion, 3 puntos)

## parametros iniciales
rm(list = ls())

##Interpolacion de Lagrange de segundo grado
f<-function(x,x0,x1,x2,y0,y1,y2){
  y<-y0*(x-x1)*(x-x2)/((x0-x1)*(x0-x2))+y1*(x-x0)*(x-x2)/((x1-x0)*(x1-x2))+y2*(x-x0)*(x-x1)/((x2-x0)*(x2-x1))
  return(y)
}

#DERIVADA DE LA INTERPOLACION DE LAGRANGE DE SEGUNDO GRADO
DRLG<-function(x,x0,x1,x2,y0,y1,y2){
  f0<-f(x0,x0,x1,x2,y0,y1,y2)
  f1<-f(x1,x0,x1,x2,y0,y1,y2)
  f2<-f(x2,x0,x1,x2,y0,y1,y2)
  
  fx<-f0*(2*x-x1-x2)/((x0-x1)*(x0-x2))+f1*(2*x-x0-x2)/((x1-x0)*(x1-x2))+f2*(2*x-x0-x1)/((x2-x0)*(x2-x1))
  
  #Grafica
  Suelo<-c(x0,x1,x2)
  Aire<-c(y0,y1,y2)
  with(trees,plot(Aire,Suelo))
  
  cat("La derivada evaluada en ", x,
      " da como resultado: ", fx,
      "\n")
  return(fx) #Imprimir valores
}

DRLG(0,0,1.25,3.75,13.5,12,10)


