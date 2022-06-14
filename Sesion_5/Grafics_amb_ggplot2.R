## ----echo=F,results=hide-------------------------------------------------------------------------------------------
 Sys.setlocale("LC_TIME", "Cat")


## ----echo=T,eval=FALSE---------------------------------------------------------------------------------------------
## install.packages("tidyverse")


## ----echo=T,eval=FALSE---------------------------------------------------------------------------------------------
## install.packages("ggplot2")
## library(ggplot2)


## ----dades1,echo=F,results=tex-------------------------------------------------------------------------------------
library(xtable)
library(gridExtra)
print(xtable(head(mtcars)),scalebox=0.55)


## ----dades2,echo=F,results=tex-------------------------------------------------------------------------------------
print(xtable(head(iris)),scalebox=0.75)


## ----dades3,echo=T,results=tex,size='tiny'-------------------------------------------------------------------------
data(airquality)
attach(airquality)
airquality$Obsdata<-paste('2016',Month,Day,sep='-')
detach(airquality)
print(xtable(head(airquality)),scalebox=0.75)


## ----dades3b,echo=T,results=tex,size='tiny'------------------------------------------------------------------------
data(ChickWeight)
print(xtable(head(ChickWeight)),scalebox=0.75)


## ----results=verbatim----------------------------------------------------------------------------------------------
aux<-as.data.frame(t(WorldPhones))
aux$cont<-rownames(aux)
telef<-reshape(aux,direction='long',idvar='cont',
        varying = list(1:7),times=colnames(aux)[1:7]
        ,timevar='year',v.names='telef')
head(telef)


## ----eval=F--------------------------------------------------------------------------------------------------------
## objeto<-ggplot(dataframe)


## ----eval=F--------------------------------------------------------------------------------------------------------
## ggplot(dataframe)


## ----eval=F,echo=T-------------------------------------------------------------------------------------------------
## library(ggplot2)
## ggplot(mtcars)


## ----fig10,include=TRUE,fig=TRUE,height=3--------------------------------------------------------------------------
library(ggplot2)
ggplot(data=mtcars)+geom_point(aes(x=wt,y=mpg))


## ----include=FALSE,fig=TRUE,height=3-------------------------------------------------------------------------------
ggplot(data=mtcars,aes(x=wt,y=mpg))+geom_point()


## ----include=FALSE,fig=TRUE,height=3-------------------------------------------------------------------------------
ggplot()+geom_point(data=mtcars,aes(x=wt,y=mpg))


## ----include=TRUE,fig=TRUE,height=3--------------------------------------------------------------------------------
ggplot(data=ChickWeight)+geom_line(aes(x=Time,y=weight,col=Diet,group=Chick))


## ----include=T,fig=TRUE,height=2-----------------------------------------------------------------------------------
ggplot(mtcars)+geom_bar(aes(x=as.factor(cyl)))

## ----include=T,fig=TRUE,height=2-----------------------------------------------------------------------------------
ggplot(mtcars)+geom_histogram(aes(x=mpg),bins=5)


## ----fig=TRUE,height=3---------------------------------------------------------------------------------------------
ggplot(data=iris) + 
 geom_point(aes(x=Sepal.Length, y=Sepal.Width,col=Species))


## ----fig=TRUE,height=3---------------------------------------------------------------------------------------------
ggplot(data=mtcars) + 
 geom_point(aes(x=wt, y=mpg,col=as.factor(cyl),shape=as.factor(am)))


## ----include=TRUE,fig=TRUE,height=3--------------------------------------------------------------------------------
ggplot(data=ChickWeight)+geom_line(aes(x=Time,y=weight,col=Diet,group=Chick))


## ----fig=TRUE,height=1.5-------------------------------------------------------------------------------------------
ggplot(data=mtcars) + geom_point(aes(x=wt, y=mpg,col='am'),col='red')


## ----fig=TRUE,height=1.5-------------------------------------------------------------------------------------------
ggplot(data=mtcars) + geom_point(aes(x=wt, y=mpg,col='red'))


## ----fig=TRUE,height=1.5-------------------------------------------------------------------------------------------
p<-ggplot(data=iris,aes(x=Sepal.Length, y=Sepal.Width))
p+geom_point()


## ----fig=TRUE,height=1.5-------------------------------------------------------------------------------------------
p+geom_point()+geom_smooth(method='lm')


## ----eval=F--------------------------------------------------------------------------------------------------------
## p<-ggplot(data=iris)
## p+geom_point(aes(x=Sepal.Length, y=Sepal.Width))
## p+geom_point()+geom_smooth(method='lm')


## ----eval=F--------------------------------------------------------------------------------------------------------
## ggplot(data=mtcars) + geom_point(aes(x=wt,
##                       y=mpg,col=as.factor(cyl)))
## ggplot(data=mtcars) + geom_point(aes(x=wt, y=mpg,col=cyl))


## ----echo=F,eval=F-------------------------------------------------------------------------------------------------
## ggplot(data=mtcars) +
##  geom_point(aes(x=wt, y=mpg,col=as.factor(cyl),shape=as.factor(cyl)))


## ----echo=F,eval=F-------------------------------------------------------------------------------------------------
## ggplot(data=mtcars) +
##  geom_point(aes(x=wt, y=mpg,col=mpg>20))


## ----fig=TRUE,height=1.5-------------------------------------------------------------------------------------------
p<-ggplot(mtcars,aes(x=wt,y=mpg))
p+geom_point()


## ----fig=TRUE,height=1.5-------------------------------------------------------------------------------------------
p+geom_smooth()+geom_smooth(aes(linetype=as.factor(am)),col='red')


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p+geom_point(aes(color=as.factor(am)))+
 geom_smooth(aes(linetype=as.factor(am),col=as.factor(am)))+
 geom_hline(yintercept=22)+
 geom_label(aes(x=4,y=24,label='mpg=22'))


## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(airquality,aes(x=as.Date(Obsdata)))+
 geom_line(aes(y=Temp),col='red')+
 geom_line(aes(y=Ozone),color='blue')


## ----eval=F--------------------------------------------------------------------------------------------------------
## p1<-ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width))


## ----fig=T,height=2.5----------------------------------------------------------------------------------------------
ggplot(mtcars)+geom_boxplot(aes(x=as.factor(am),y=mpg))


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(mtcars)+geom_density(aes(mpg))


## ----fig=T,height=1.8----------------------------------------------------------------------------------------------
p1<-ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))
p1+geom_point()+geom_boxplot()+geom_density2d()

## ----fig=T,height=1.8----------------------------------------------------------------------------------------------
p2<-ggplot(telef,aes(x=year,y=cont))
p2+geom_tile(aes(fill=telef))


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(iris)+geom_point(aes(x=Sepal.Length,
   y=Petal.Length))+ facet_wrap(~Species)


## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+
 geom_smooth()+
 facet_grid(as.factor(am)~as.factor(cyl))


## ----eval=F--------------------------------------------------------------------------------------------------------
## ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+
##  geom_smooth()+
##  facet_grid(as.factor(am)~as.factor(gear))


## ----eval=F,echo=FALSE---------------------------------------------------------------------------------------------
## ggplot(mtcars)+geom_point(aes(x=wt,
##    y=mpg))+ facet_wrap(~cut(hp,3))


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(mtcars)+geom_bar(aes(x=gear))


## ----echo=F,results=tex--------------------------------------------------------------------------------------------
print(xtable(table(mtcars$gear)),scalebox=0.6)


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(mtcars)+stat_count(aes(x=gear))


## ------------------------------------------------------------------------------------------------------------------
aux=data.frame(x=c('a','b','c'),y=c(10,20,15))

## ----eval=F--------------------------------------------------------------------------------------------------------
## ggplot(aux)+geom_bar(aes(x,y))

## ----fig=T,height=1.75---------------------------------------------------------------------------------------------
ggplot(aux)+geom_bar(aes(x,y),stat='identity')


## ----fig=T,height=1.75---------------------------------------------------------------------------------------------
ggplot(aux)+geom_col(aes(x,y))


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(mtcars)+geom_bar(aes(gear,y=..prop..))

## ----fig=T,height=2------------------------------------------------------------------------------------------------
ggplot(airquality,aes(x=as.Date(Obsdata)))+
 geom_line(aes(y=Temp),col='red')+
 geom_bar(aes(y=Wind),color='blue',stat='identity')


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p<-ggplot(ChickWeight,aes(x=Time,y=weight))
p+stat_summary(fun=mean)


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p+stat_summary(fun=mean,geom='line')


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p+stat_summary(fun=mean,geom='line',aes(col=Diet))


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p+stat_summary(fun=mean,fun.min=function(x) mean(x)-sd(x),fun.max=function(x) mean(x)+sd(x))


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p+ stat_summary(fun=mean,geom='line',aes(col=Diet))+
 stat_summary(fun=mean,fun.min=function(x) mean(x)-sd(x),fun.max=function(x) mean(x)+sd(x),aes(col=Diet))


## ----echo=F,results=verbatim---------------------------------------------------------------------------------------
head(telef)


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(telef,aes(x=year,y=telef,col=cont))+geom_line()


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(telef,aes(x=year,y=telef,col=cont))+geom_line(aes(group=cont))


## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(mtcars)+geom_bar(aes(x=as.factor(cyl),
  fill=as.factor(gear)))


## ----fig=T,height=3------------------------------------------------------------------------------------------------
library(gridExtra)
p1<-ggplot(mtcars,aes(x=as.factor(cyl),fill=as.factor(gear)))
grid.arrange(p1+geom_bar(position='stack'), 
p1+geom_bar(position='fill'),
p1+geom_bar(position='dodge'), 
p1+geom_bar(position='identity'),ncol=2)


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p<-ggplot(ChickWeight,aes(x=Time,y=weight))
grid.arrange(p+geom_point(),
p+geom_jitter(),ncol=2)



## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
p<-ggplot(iris,aes(x=Species,y=Sepal.Length))+geom_boxplot()
p+coord_flip()


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
p<-ggplot(mtcars,aes(x=as.factor(carb),fill=as.factor(carb)))
grid.arrange(p+geom_bar(),
p+geom_bar()+coord_polar(),ncol=2)

## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
sp<-map_data('world','Spain')
p<-ggplot(sp,aes(long,lat,group=group))
grid.arrange(p+geom_polygon(fill='red'),
p+geom_polygon(fill='red')+coord_quickmap(),ncol=2)


## ----fig=T,height=2.5----------------------------------------------------------------------------------------------
p1<-ggplot(mtcars,aes(x=hp,y=mpg))
grid.arrange(p1+geom_point(),p1+geom_point()+
coord_cartesian(xlim=c(50,150),ylim=c(15,25)),ncol=2)

