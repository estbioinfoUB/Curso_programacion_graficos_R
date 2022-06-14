# 1. Ejercicio 1: LOTERIA PRIMITIVA. Crear un script que haga lo siguiente:
# a) Asigne una combinación de la primitiva (6 números entre 1 y 49) a una variable
comb<-sample(1:49,6)
# b) Vaya simulando de forma aleatoria sorteos de la primitiva y no pare hasta que tengamos 
# al menos 3 aciertos en nuestra combinación
encert=0
while (encert<3){
 comb.sort<-sample(1:49,6)
 encert<-sum(comb %in% comb.sort)
}
# c) Debe mostrar en cada paso la combinaciónn ganadora y al final el número de sorteos necesarios
encert=0
sorteo=0
while (encert<3){
 sorteo<-sorteo+1
 comb.sort<-sample(1:49,6)
 cat('Combinación ganadora: ',paste(comb.sort,collapse='-'),'\n')
 encert<-sum(comb %in% comb.sort)
 cat('Sorteo: ',sorteo,'\t','Número de aciertos: ',encert,'\n')
}
cat('Número total de sorteos:',sorteo)
# d) Os atreveis a hacerlo 10.000 veces para estimar el promedio de sorteos necesarios para conseguir 3 aciertos? 
# (no querais escribir cada vez las combinaciones)
sorteos<-0
for (i in 1:10000){
 comb<-sample(1:49,6)
 encert=0
 sorteo=0
 while (encert<3){
  sorteo<-sorteo+1
  comb.sort<-sample(1:49,6)
  encert<-sum(comb %in% comb.sort)
 }
 sorteos<-sorteos+sorteo
 }
cat('Promedio de sorteos:',sorteos/10000)

# 
# 
# 2. Ejercicio 2: Utilización de la función apply 
# a) Generar una matriz de números aleatorios con 10.000 filas y 1.000 columnas
mat<-matrix(runif(10000*1000),ncol=1000)
# b) Computar el tiempo que se tarda en sumar las 1.000 columnas
#  b.1) Utilizando la función apply
system.time(x1<-apply(mat,2,sum))
#  b.2) Utilizando un bucle for sobre las columnas
system.time({
 x2<-c()
 for (i in 1:1000)  x2[i]<-sum(mat[,i])
 })
sum(x1==x2)
# Nota: generar los valores aleatorios con la función runif y utilizar
# la funciónn system.time para calcular los tiempos de ejecución
# c) Obtener una estadística descriptiva básica de las variables del conjunto de datos iris.
#  c.1) Globalmente
apply(iris[,1:4],2,summary)
#  c.2) Separando por especies
by(iris[,1:4],iris[,5],summary)
# 
# 3. Ejercicio 3. Funciones
# a) Escribir una función MobAver(x,k) que dado un vector x=(x_1,...,x_n) devuelva el vector con las medias móviles de orden k.
# Por ejemplo si k=3 debe devolver (x_1+x_2+x_3)/3 , (x_2+x_3+x_4)/3 , ..., (x_n-2,x_n-1,x_n)/3
x<-c(1:5,6:1)
MobAver<-function(x,k=3){
 n<-length(x)
 y<-c()
 for(i in 1:(n-k+1))  y[i]<-mean(x[i:(i+k-1)])
 return(y)
}
MobAver(x)

# b) Incorporar mensajes de error si k es mayor que n
x<-c(1:5,6:1)
MobAver<-function(x,k=3){
 n<-length(x)
 if (k>n) stop('Valor de k mayor que la longitud de los datos')
 y<-c()
 for(i in 1:(n-k+1))  y[i]<-mean(x[i:(i+k-1)])
 return(y)
}
MobAver(x,10)
MobAver(x,12)
