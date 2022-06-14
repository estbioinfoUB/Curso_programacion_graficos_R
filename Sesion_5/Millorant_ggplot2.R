## ----echo=F--------------------------------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(xtable)

## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(mtcars,aes(wt,mpg))+
 geom_point(aes(color=as.factor(am)))+
 labs(title='Consumo según el peso del vehículo')


## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(mtcars,aes(wt,mpg))+
 geom_point(aes(color=as.factor(am),size=carb))+
 labs(title='Consumo según el peso del vehículo',
      subtitle='Principales marcas americanas',
      caption='Datos procedentes de R',
      x=expression(paste('peso (lbs x', 10^3,')')),
      y='consumo (mpg)',
      color='automático',
      size='carburadores'
 )


## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(mtcars,aes(wt,mpg))+
  geom_text(aes(label=cyl,color=as.factor(am)))+
 labs(caption='Los números representan el número de cilindros')


## ----fig=T,height=3------------------------------------------------------------------------------------------------
sub.mtcars<-subset(mtcars,substr(rownames(mtcars),1,3)=='Mer')
ggplot(mtcars,aes(wt,mpg))+
 geom_point(aes(color=as.factor(am)))+
 geom_label(aes(label=rownames(sub.mtcars)),data=sub.mtcars)


## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(mtcars,aes(wt,mpg))+
 geom_point(aes(color=as.factor(am)))+
 geom_point(size=3,shape=1,data=sub.mtcars)+
 ggrepel::geom_label_repel(aes(label=rownames(sub.mtcars))
                           ,data=sub.mtcars)


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p<-ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+
 geom_smooth(method='lm')
r2<-round(summary(lm(mpg~wt,mtcars))$r.squared,2)
label<-paste('R^2==',r2)
p+geom_text(x=4,y=30,label=label,parse=T)+
 geom_segment(x=4,y=29,xend=3.25,yend=20,arrow=arrow())


## ----fig=T,height=3------------------------------------------------------------------------------------------------
ggplot(mtcars)+geom_bar(aes(gear,y=..prop..),fill='orange')+
 geom_text(aes(x=gear,y=..prop..,label=round(..prop..,2)),
           stat='count',vjust=2,color='blue')


## ----echo=F,results=tex--------------------------------------------------------------------------------------------
summaryAmp<-function(x,colvar,colfac){
 fact<-aggregate(x[,colvar],list(x[,colfac]),mean)$Group.1
 means<-aggregate(x[,colvar],list(x[,colfac]),mean)$x
 sds<-aggregate(x[,colvar],list(x[,colfac]),sd)$x
 len<-aggregate(x[,colvar],list(x[,colfac]),length)$x
 ses<-sds/sqrt(len)
 cis<-ses*qnorm(0.975)
 df<-data.frame(factor=fact,mean=means,sd=sds,se=ses,ci=cis)
 colnames(df)[1]<-colfac
 return(df)
}
car.new<-summaryAmp(mtcars,'mpg','cyl')
print(xtable(car.new,caption='Resumen del consumo (mpg)'))

## ----fig=T,height=3------------------------------------------------------------------------------------------------
p1<-ggplot(car.new,aes(x=as.factor(cyl),y=mean))
p2<-p1+geom_bar(stat='identity',fill=c('cyan','green','orange'))
p2+geom_text(aes(label=round(mean,2)),vjust=2,col='blue')


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p<-ggplot(mtcars,aes(x=am,y=mpg))
grid.arrange(p+geom_point(),
p+geom_point()+scale_x_continuous(breaks=seq(0,1,1)),
p+geom_point()+scale_x_continuous(breaks=seq(0,1,1),
              labels=c('no','si')),ncol=3)


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p<-ggplot(mtcars,aes(x=wt,y=mpg))
grid.arrange(p+geom_point(),
p+geom_point()+scale_y_continuous(breaks=c(10,seq(15,25,2),30,35)),
p+geom_point()+scale_x_continuous(labels=NULL)+
 scale_y_continuous(limits=c(15,25)),ncol=3)


## ----fig=T,height=3------------------------------------------------------------------------------------------------
p<-ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))
grid.arrange(p+geom_point(aes(color=Sepal.Length,shape=Species)),
             p+geom_point(aes(color=Sepal.Length,shape=Species))+
         scale_color_gradient(limits=c(5,6),low='green',high='red'), ncol=2)


## ----fig=T,height=3.75---------------------------------------------------------------------------------------------
p<-ggplot(mtcars,aes(x=wt,y=mpg))
p1<-p+geom_point(aes(color=hp,size=cyl))
grid.arrange(
 p1+scale_size('Número de \n cilindros')+scale_color_continuous('Potencia (HP)'),
 p1+scale_color_continuous('Potencia (HP)',low='green',high='red')
 ,ncol=2
)


## ----echo=F,eval=F-------------------------------------------------------------------------------------------------
## p<-ggplot(mtcars,aes(x=wt,y=mpg))
## p1<-p+geom_point(aes(color=as.factor(cyl),size=hp))
## p1+scale_size('Potencia (HP)')+scale_color_brewer('Número de \n  cilindros',palette='Pastel1')


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
set.seed(12244)
x<-runif(30,0,5)
y<-exp(x)+rnorm(30,0,0.5)
dades<-data.frame(x,y)
grid.arrange(
 ggplot(dades,aes(x,y))+geom_point(),
 ggplot(dades,aes(x,log(y)))+geom_point(),
 ncol=2)


## ----fig=T,height=1.5----------------------------------------------------------------------------------------------
ggplot(dades,aes(x,y))+geom_point()+scale_y_continuous(,breaks=c(0,1,5,10,50,100),trans='log')


## ----fig=T,height=3.5----------------------------------------------------------------------------------------------
p<-ggplot(mtcars,aes(wt,mpg,color=hp))+geom_point()
grid.arrange(p,p+theme_bw()+theme(legend.position='bottom'),
ncol=2)


## ----fig=T,height=3.5----------------------------------------------------------------------------------------------
qplot(wt,mpg,data=mtcars,facets=~am,col=cyl,size=hp,
      xlab=expression(paste('peso (lbs x', 10^3,')')),
      ylab='Consumo (mpg)')


## ----echo=FALSE,results=hide---------------------------------------------------------------------------------------
HDR<-read.table('HDR2013.csv',sep=';',dec=',',header=T)
pc1 <- ggplot(HDR, aes(x = HDI, y = Life_expec, color = Region))
pc1 <- pc1 + geom_point(shape = 1)
label.these<-c('Spain','Denmark','Netherlands','China',
                 'India','Russian Federation')
pc2 <- pc1+ggrepel::geom_label_repel(aes(label = Country),
       color = "black", size = 3,
       data = HDR[HDR$Country %in% label.these, ])+
 geom_point(size=3,shape=1,show.legend=F,
            data=HDR[HDR$Country %in% label.these, ])
pc3 <- pc2 +
geom_smooth(aes(group = 1),
               method = "lm",
               color = "black",
               se = FALSE)
pc4 <- pc3 + theme_bw() +
labs(x="Human Development Index, 2012\n(1 = best)",
        y="Life Expectay in years (2012)")+
 theme(legend.position = "top")+
 theme(legend.text=element_text(size=7))

## ----echo=F,fig=TRUE,height=4--------------------------------------------------------------------------------------
pc4

