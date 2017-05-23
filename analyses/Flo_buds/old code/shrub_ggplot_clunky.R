shrubsum<-summarise(shrubgroup, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
View(shrubsum)
library(gridExtra)
grid.arrange(ilex2plot+geom_point(aes(treatcode,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(treatcode,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("I. mucronata"),pruplot+geom_point(aes(treatcode,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(treatcode,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("P. pensylvanica"),corplot+geom_point(aes(treatcode,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(treatcode,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("C. coranuta"),ncol=3)
### now new graphs showing the offset
ilexspread<-ggplot(ILEMUC2, aes(x=treatcode, y=meanl))
ilexspread+geom_pointrange(aes(ymin=meanl, ymax=meanf))+ggtitle("I. mucronata")
pruspread<-ggplot(PRUPEN, aes(x=treatcode, y=meanl))
pruspread+geom_pointrange(aes(ymin=meanl, ymax=meanf))+ggtitle("P. pensyvanica")
corspread<-ggplot(CORCOR, aes(x=treatcode, y=meanl))
corspread+geom_pointrange(aes(ymin=meanl, ymax=meanf))+ggtitle("C. coranuta")
grid.arrange(ilexspread+geom_pointrange(aes(ymin=meanl, ymax=meanf))+ggtitle("I. mucronata"),pruspread+geom_pointrange(aes(ymin=meanl, ymax=meanf))+ggtitle("P. pensyvanica"), corspread+geom_pointrange(aes(ymin=meanl, ymax=meanf))+ggtitle("C. coranuta"), ncol=3)
###lets make sure outlyers aren't driving corcor
View(subset(goodshrubs,treatcode=="WL0"))
##################################################all this plotting was tempermental because i did combine the phenophase. here's it all again
###My old graph worked but weren't ideal. I've redone them in a more proper ggplot fashion
shrubsum<-summarise(shrubgroup, meanf=mean(fday,na.rm=TRUE), meanl=mean(lday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
shrubsum2<-gather(shrubsum,phenophase,phen_mean, meanf:meanl)
head(shrubsum2)
shrubsum3<-gather(shrubsum2,phen_classSE,phen_SE, sef:sel)
shrubsum3<-filter(shrubsum3, treatcode %in% c( "CL0","CS0", "WL0","WS0"))

#plot for CORCOR
CORCOR<-filter(shrubsum3, sp=="CORCOR")
ILEMUC<-filter(shrubsum3, sp=="ILEMUC")
#now prunus
PRUPEN<-filter(shrubsum3, sp=="PRUPEN")
ILEMUC2<-filter(ILEMUC, treatcode %in% c( "CL0","CS0", "WL0","WS0"))

grid.arrange(      
  ggplot(data=CORCOR, aes(x=treatcode, y=phen_mean, color=phenophase)) +
    geom_pointrange(aes(ymin=phen_mean-phen_SE, ymax=phen_mean+phen_SE))+labs(title="Corylus cornuta", x="Treatment", y="Days since initiation")+ theme(legend.position = "none"),
  
  ggplot(data=ILEMUC2, aes(x=treatcode, y=phen_mean, color=phenophase)) +
    geom_pointrange(aes(ymin=phen_mean-phen_SE, ymax=phen_mean+phen_SE))+labs(title="Ilex mucronata", x="Treatment", y="Days since initiation")+ theme(legend.position = "none"),
  
  ggplot(data=PRUPEN, aes(x=treatcode, y=phen_mean, color=phenophase)) +
    geom_pointrange(aes(ymin=phen_mean-phen_SE, ymax=phen_mean+phen_SE))+labs(title="Prunus pensylvanica", x="Treatment", y="Days since initiation")+ theme(legend.position = "right"),ncol=3)
##A better way that puts them all on the same axis
p<-ggplot(data=shrubsum3, aes(x=treatcode, y=phen_mean, color=phenophase)) +
  geom_pointrange(aes(ymin=phen_mean-phen_SE, ymax=phen_mean+phen_SE))++ theme(legend.position = "right")      
p+facet_wrap(~sp)