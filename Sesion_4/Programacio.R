## ------------------------------------------------------------------------------------------------------------------
summary(mtcars)


## ------------------------------------------------------------------------------------------------------------------
mtcars$cyl<-as.factor(mtcars$cyl)
mtcars$gear<-as.factor(mtcars$gear)
mtcars$carb<-as.factor(mtcars$carb)
mtcars$am<-as.factor(mtcars$am)
summary(mtcars)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
fibo<-c(1,1)
for (i in 3:14) {
 aux<-fibo[i-2]+fibo[i-1]
 fibo<-c(fibo,aux)
}
fibo


## ------------------------------------------------------------------------------------------------------------------
 data(mtcars) # Recargamos el dataframe original
index<-c(2,9:11)
for (i in index) {
 mtcars[,i]<-as.factor(mtcars[,i])
 print(paste('Transformada la variable',colnames(mtcars)[i]))
}
summary(mtcars)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
 fibo<-c(1,1)
while (length(fibo)<=12){
 aux<-fibo[length(fibo)-1]+fibo[length(fibo)]
 fibo<-c(fibo,aux)
}
fibo


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
x<-c(1,2,3,3,2,1)
if (is.factor(x)) table(x) else mean(x)
x<-factor(x)
if (is.factor(x)) table(x) else mean(x)


## ----echo=T--------------------------------------------------------------------------------------------------------
for (i in 1:10){
 x<-runif(1,-1,1)
 if (x>0) y<-log(x) else next()
 cat(x,' logaritme ',y,'\n')
}


## ----eval=F--------------------------------------------------------------------------------------------------------
##  colMeans(mtcars)


## ------------------------------------------------------------------------------------------------------------------
 for (i in 1:ncol(mtcars)) {
  print(mean(mtcars[,i]))
 }


## ------------------------------------------------------------------------------------------------------------------
for (i in 1:ncol(mtcars)) {
if (!is.factor(mtcars[,i])) print(mean(mtcars[,i]))
}

for (i in 1:ncol(mtcars)) {
 if (!is.factor(mtcars[,i])) {
  cat('Variable:',colnames(mtcars)[i],'\t Promig:',mean(mtcars[,i]),'\n')
 }  else cat('Variable:',colnames(mtcars)[i],'\t Es una variable factor.\n')
}


## ------------------------------------------------------------------------------------------------------------------
for (i in 1:ncol(mtcars)) {
 if (!is.factor(mtcars[,i])) {
  cat('Variable:',colnames(mtcars)[i],'\n Promig:',mean(mtcars[,i]),'\n\n')
 } else{
  cat('Variable:',colnames(mtcars)[i],'\n Taula:')
  print(table(mtcars[,i]))
  cat('\n\n')
 }
}


## ----echo=T--------------------------------------------------------------------------------------------------------
resumen<-function(x){
  for (i in 1:ncol(x)) {
 if (!is.factor(x[,i])) {
  cat('Variable:',colnames(x)[i],'\n Promig:',mean(x[,i],na.rm=T),',sd:',sd(x[,i],na.rm=T),'\n\n')
 } else{
  cat('Variable:',colnames(x)[i],'\n Taula:')
  print(table(x[,i]))
  cat('\n\n')
 }
}
}


## ----echo=T--------------------------------------------------------------------------------------------------------
resumen(iris)



## ----echo=TRUE-----------------------------------------------------------------------------------------------------
  m<-matrix(c(1:10,11:20),nrow=10,ncol=2)
 # promedio de las filas
 apply(m,1,mean)
 # promedio de las columnas
 apply(m,2,mean)



## ----echo=TRUE-----------------------------------------------------------------------------------------------------
 l<-list(a=1:10,b=11:20)
# suma de los valores de cada elemento
sapply(l,sum)
sapply(l,sum)[1]


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
sapply(iris[,1:4],mean)
sapply(iris,is.numeric)
index<-sapply(iris,is.numeric)
sapply(iris[,index],mean)



## ----echo=TRUE-----------------------------------------------------------------------------------------------------
iris[1:2,]
tapply(iris$Sepal.Length,iris$Species,mean)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
by(iris[,1:4],iris$Species,colMeans)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------

by(iris,iris$Species,summary)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
aggregate(iris[,1:4],list(iris$Species),mean)
aggregate(iris[,1:4],list(iris$Species),summary)


## ----eval=F--------------------------------------------------------------------------------------------------------
## by(mtcars[,1],mtcars$cyl,mean)
## aggregate(mtcars[,1],list(cilindres=mtcars$cyl),mean)
## aggregate(mtcars,list(cilindres=mtcars$cyl),mean)
## #by(mtcars,mtcars$cyl,mean)
## aggregate(mtcars,list(cilindres=mtcars$cyl,
##                       marxes=mtcars$gear),mean)
## aggregate(mpg~cyl+gear,data=mtcars,mean)
## aggregate(cbind(mpg,hp)~cyl+gear,data=mtcars,mean)
## aggregate(.~cyl+gear,data=mtcars,mean)


## ------------------------------------------------------------------------------------------------------------------

t1<-aggregate(mpg~cyl+carb,FUN=mean,data=mtcars)
t2<-aggregate(mpg~cyl+carb,FUN=length,data=mtcars)
t1[,3]<-paste(t1[,3],' (n=',t2[,3],')',sep='')
t1




## ----echo=T,eval=F-------------------------------------------------------------------------------------------------
##  function.name <- function(argumentos){
##    # Secuencia de instrucciones
##    # que generan un resultado
##     return(resultado)
##  }


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent <- function (x){
   porcentaje<-round(x*100,digits=2)
   porcentaje<-paste(porcentaje,'%')
   return (porcentaje)
  }


## ----echo=T--------------------------------------------------------------------------------------------------------
 ls()


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent(0.236478)
prueba<-c(0.24,0.893443,1.254,0.6)
funPercent(prueba)


## ----echo=T--------------------------------------------------------------------------------------------------------
  funPercent <- function (x,dig=2){
   porcentaje<-round(x*100,digits=dig)
   porcentaje<-paste(porcentaje,'%')
   return (porcentaje)
  }


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent(0.34767778377626)
funPercent(0.34767778377626,6)
funPercent(dig=4,0.34767778377626)


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent <- function (x,dig=2,...){
   porcentaje<-round(x*100,digits=dig)
   porcentaje<-paste(porcentaje,'%',...)
   return (porcentaje)
}


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent(prueba)
funPercent(prueba,sep='')
funPercent(prueba,sep='',collapse='-')


## ----echo=T--------------------------------------------------------------------------------------------------------
m<-matrix(runif(5*5),ncol=5)
m


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
apply(m,c(1,2),function(x) paste(round(x,digits=3)*100,'%'))


## ----interplot,echo=T,fig=T,include=FALSE--------------------------------------------------------------------------
prop.test(100,230,p=0.5)


## ----echo=T,eval=F-------------------------------------------------------------------------------------------------
## funPercent('a')


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent.character <- function (x){
 porcentaje<-paste(x,'%')
 return (porcentaje)
}


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent.character('a')


## ----echo=T--------------------------------------------------------------------------------------------------------
funPercent.numeric<-funPercent


## ----echo=T--------------------------------------------------------------------------------------------------------
 funPercent <- function(x,...){
  UseMethod('funPercent')
 }
funPercent(0.34)


## ----echo=T--------------------------------------------------------------------------------------------------------
 funPercent('Hola')


## ----echo=T--------------------------------------------------------------------------------------------------------
 methods(funPercent)


## ----echo=T--------------------------------------------------------------------------------------------------------
 print.methods<-methods(print)
length(print.methods)
head(print.methods,10)


## ----echo=T--------------------------------------------------------------------------------------------------------
 al<-list(nombre='Pilar',facultad='Biologia',edad=25,mujer=T)
class(al)<-'alumno'
al
al[[1]]


## ----echo=T--------------------------------------------------------------------------------------------------------
 print.alumno<-function(obj){
  cat(obj$nombre,'\n')
  cat('Facultad:',obj$facultad,'\n')
  cat('Edad:',obj$edad,'\n')
  sexo<-ifelse(obj$mujer,'Mujer','Varón')
  cat('Sexo:',sexo,'\n')
 }


## ----echo=T--------------------------------------------------------------------------------------------------------
 al


## ----echo=T--------------------------------------------------------------------------------------------------------
 head(methods(print))


## ----echo=T--------------------------------------------------------------------------------------------------------
 setClass('alumno',
  representation(
   nombre='character',
   facultad='character',
   edad='numeric',
   mujer='logical')
 )


## ----echo=T--------------------------------------------------------------------------------------------------------
 pili<-new('alumno',nombre='Pilar',facultad='Biologia',edad=23,mujer=T)
pili


## ----echo=T--------------------------------------------------------------------------------------------------------
for (i in 1:10){
 x<-runif(1,-1,1)
 if (x>0) y<-log(x) else{ 
  warning(paste('Ha aparecido un número negativo:',x,' en la iteración',i))
  next()
 }
 cat(x,' logaritme ',y,'\n')
}


## ----echo=T,eval=F-------------------------------------------------------------------------------------------------
## for (i in 1:10){
##  x<-runif(1,-1,1)
##  if (x>0) y<-log(x) else{
##   stop(paste('Ha aparecido un número negativo:',x,' en la iteración',i))
##   # break() no necesario
##  }
##  cat(x,' logaritme ',y,'\n')
## }


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
 # Función principal
 geom<-function(x){
  mos<-mostra(x)
  geomean<-geomean(mos)
  res<-paste('La media geométrica es:',geomean)
  return(res)
 }
# Función que genera una muestra de tamaño x
mostra<-function(x){
 mos<-rnorm(x)
 return(mos)
}
# Función que calcula la media geométrica
geomean<-function(x){
 meangeo<-exp(mean(log(x)))
 if(meangeo>1) result<-'GM > 1' else result<-'GM < 1'
 return(result)
}


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
 debug(geomean)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
 m<-matrix(runif(200000),20000)
system.time(apply(m,1,sum))


## ----eval=FALSE,echo=TRUE------------------------------------------------------------------------------------------
## install.packages('rbenchmark')

## ----echo=T--------------------------------------------------------------------------------------------------------
library(rbenchmark)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
f0<-function(x) apply(x,1,sum)
f1<-function(x) rowSums(x)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
benchmark(f0(m),f1(m),columns=c('test','elapsed','relative'),replications=50)


## ----echo=TRUE-----------------------------------------------------------------------------------------------------
identical(f0(m),f1(m))
identical(c(1,1),c(1,0.9999))
all.equal(c(1,1),c(1,0.9999))
all.equal(c(1,1),c(1,0.9999),tolerance=0.0001)
all.equal(c(x=1,y=1),c(z=1,g=1))
all.equal(c(x=1,y=1),c(z=1,g=1),check.attributes=F)

