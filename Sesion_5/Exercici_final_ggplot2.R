###################################################
### code chunk number 20: Millorant_ggplot2.Rnw:320-340
###################################################
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


###################################################
### code chunk number 21: Millorant_ggplot2.Rnw:342-343
###################################################
pc4


