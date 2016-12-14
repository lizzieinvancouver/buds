## Dan B- initial data visualization based on Dan F, "buds" data
##safetues
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()
library(plotrix)
library(gdata)
library(nlme)
library(scales)
library(arm)
library(picante)
library(ade4)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Documents/git/buds/analyses/input")
fb<-read.csv("Budburst By Day.csv", header = TRUE)
#make subset for the good shurbs leftover from Mag 7
goodshrubs<-filter(fb, sp %in% c( "CORCOR","ILEMUC", "PRUPEN"))
shrubgroup<-group_by(goodshrubs,sp,treatcode)
shrubsum<-summarise(shrubgroup, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
View(shrubsum)
#plot for CORCOR
CORCOR<-filter(shrubsum, sp=="CORCOR")
View(CORCOR)
corplot<-ggplot(CORCOR, aes(x=treatcode, y=meanl))
corplot+geom_point(aes(treatcode,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(treatcode,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("C. coranuta")
#now for Ilex
ILEMUC<-filter(shrubsum, sp=="ILEMUC")
ilexplot<-ggplot(ILEMUC, aes(x=treatcode, y=meanl))
ilexplot+geom_point(aes(treatcode,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(treatcode,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("I. mucronata")
#now prunus
PRUPEN<-filter(shrubsum, sp=="PRUPEN")
pruplot<-ggplot(PRUPEN, aes(x=treatcode, y=meanl))
pruplot+geom_point(aes(treatcode,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(treatcode,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("P. pensylvanica")
## now try a subset of ILEMUC without chilling
View(ILEMUC)
ILEMUC2<-filter(ILEMUC, treatcode %in% c( "CL0","CS0", "WL0","WS0"))
View(ILEMUC2)
ilex2plot<-ggplot(ILEMUC2, aes(x=treatcode, y=meanl))
ilex2plot+geom_point(aes(treatcode,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(treatcode,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("I. mucronata")
#plot the next to each other
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
