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
View(fb)

#make subset for only species with 10+ flowering episodes
mag7<-filter(fb, sp %in% c("ACEPEN", "ACERUB", "CORCOR","ILEMUC", "POPGRA", "PRUPEN", "VIBLAN"))
View(mag7)
nrow(mag7) #800
##now lets plot the results for 1 treatment WL0
mag7_WL0<-filter(mag7,treatcode=="WL0")
View(mag7_WL0)
##### THe code broke here it worked before but the grouping doesnt seem to be working.
grouped<-group_by(mag7_WL0,sp)

##calculate mean ## decided to try making each an object instead
fdaysum<-summarise(grouped, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
View(fdaysum)
try<-ggplot(fdaysum, aes(x=sp, y=meanf))
try+geom_point(aes(sp,meanf, color="blue"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef, color="blue"))

###plot WLO
plot<-ggplot(fdaysum, aes(sp,day))
plot+geom_point(aes(sp,meanf, color="blue"))+ geom_point(aes(sp,meanl,color="red"))+ggtitle("Treatment WL0")
##now do this for CS0
mag7_CS0<-filter(mag7,treatcode=="CS0")
grouped2<-group_by(mag7_CS0, sp)
fdaysum2<-summarise(grouped2, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
plot2<-ggplot(fdaysum2, aes(sp,day))
plot2+geom_point(aes(sp,meanf, color="blue"))+ geom_point(aes(sp,meanl,color="red"))+ggtitle("Treatment CS0")
#Now WS0
mag7_WS0<-filter(mag7,treatcode=="WS0")
grouped3<-group_by(mag7_WS0, sp)
fdaysum3<-summarise(grouped3, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
plot3<-ggplot(fdaysum2, aes(sp,day))
plot3+geom_point(aes(sp,meanf, color="blue"))+ geom_point(aes(sp,meanl,color="red"))+ggtitle("Treatment WS0")
