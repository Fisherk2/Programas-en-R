# Mi Primer programa (Comentarios con almohadilla)

# Operaciones aritmeticas
3+2
3*5
1/2
9-3
2^4
4%%3

# Asignacion de valores u objetos (<- ó ->)
A<-10
3->B

# VECTORES
A<-c(1,2,6,8,6)
B<-c(4,1,-5,3,2)

## Longitud del vector
n1<-length(A)
n2<-length(B)

## Operaciones con vectores
A+B
A*B
A/B
### Producto punto
A%*%B

## Vectores especiales
### Entradas del 1 al 25
A1<-1:25
### Repetir un valor (x,n elementos)
A2<-rep(1,10)
### Repetir vectores
A3<-rep(c(A1,A2),3)
### Secuencias
A4<-seq(0,1,1/10)

## Subconjuntos
A[5]
A[c(1,5)]

# MATRICES
M1<-matrix(1:6,nrow=3,ncol=2,byrow=FALSE)
M2<-matrix(1:6,nrow=3,ncol=2,byrow=TRUE)

## Transpuesta
M3<-t(M2)

## Operaciones con matrices
M1+M2
M1*M2
M1%*%M3

## Subconjuntos
M1[1,2]
M1[,1]
M1[c(1,2)]

# CREACION DE FUNCIONES
## Funcion norma vectorial
norma<-function(x){
  A<-sqrt(x%*%x)
  return(A) #Es la salida de la funcion
}

## Funcion producto cruz
PCruz<-function(x,y){
  i<-x[2]*y[3]-x[3]*y[2]
  j<-x[1]*y[3]-x[3]*y[1]
  k<-x[1]*y[2]-x[2]*y[1]
  B<-c(i,-j,k)
  return(B)
}
x<-c(2,1,0)
y<-c(-1,0,2)

## Graficas de funciones
X<-rnorm(50)
Y<-runif(50)
plot(X,Y,type="l") #Graficadora

### Ejemplo: Funcion lineal
X<-seq(0,10,0.01)
Y<-2*X+1
plot(X,Y,type="l",col="blue",main="Funcion Lineal"
     ,xlab = "Dominio",ylab = "Rango")
grid()

## Presentacion de varios graficos en una ventana
par(mfrow=c(1,2))
X<-seq(0,10,0.01)
Y<-2*X+1
Z<-3*X^2+1

plot(X,Y,type="l",col="blue",main="Funcion Lineal"
     ,xlab = "Dominio",ylab = "Rango")
grid()

plot(X,Z,type="l",col="red",main="Funcion Cuadratica"
     ,xlab = "Dominio",ylab = "Rango")
grid()

par(mfrow=c(1,1))

# CONTROL DE FLUJO (Help - Control Flow)
for(i in 1:5) print(1:i)

for(n in c(2,5,10,20,50)) {
  x <- stats::rnorm(n)
  cat(n, ": ", sum(x^2), "\n", sep = "")
}

##Ejemplo: Realice una funcion que tenga como entrada un vector y sume todos sus elementos
suma<-function(X){
  n<-length(X)
  S<-0
  for (i in 1:n) {
    S<-S+X[i]
  }
  
  return(S)
}
Z<-1:5
suma(Z)

# DERIVADAS

## Funcion D(), se ocupa solo en expresiones
f<-expression(x^2+2*x)
### Derivando respecto a la variable x
D(f,"x") #Nos arrojara el resultado en la consola
### Para calcular la segunda derivada.
f<-expression(x^2+2*x+x*y+3*z*x+4*y)
D(D(f,"x"),"x")#Segunda derivada respecto a x.
### Segunda derivada respecto a "x" y a "y".
f<-expression(x^2+2*x+x*y+3*z*x+4*y)
D(D(f,"x"),"y")#Derivando respeto a x (d[f'']/dxdy)

##Funcion deriv(), devuelve un objeto tipo function()
### Instalando el paquete.
install.packages("Deriv")
library(Deriv)#Cargando el paquete.

### Derivando respecto a la variable x
f<-function(x,y,z){
  x^2+2*x+x*y+3*z*x+4*y
}
Deriv(f,"x")#Nos devuelve un objeto de tipo function()

### Para calcular la segunda derivada con respecto de x
f<-function(x,y,z){
  x^2+2*x+x*y+3*z*x+4*y
}
Deriv(f,"x", nderiv = 2)#nderiv es el nivel de derivacion, en este caso, es la segunda f''(x)
