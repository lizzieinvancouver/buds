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
#make subset for only species with 10+ flowering episodes
mag7<-filter(fb, sp %in% c("ACEPEN", "ACERUB", "CORCOR","ILEMUC", "POPGRA", "PRUPEN", "VIBLAN"))

##now lets plot the results for 1 treatment WL0
mag7_WL0<-filter(mag7,treatcode=="WL0")

##### THe code broke here it worked before but the grouping doesnt seem to be working.
grouped<-group_by(mag7_WL0,sp)
##calculate mean ## decided to try making each an object instead
fdaysum<-summarise(grouped, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
View(fdaysum)
##plotWL0
try<-ggplot(fdaysum, aes(x=sp, y=meanl))
try+geom_point(aes(sp,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(sp,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("Treatment WL0")
##now do this for CS0
mag7_CS0<-filter(mag7,treatcode=="CS0")
grouped2<-group_by(mag7_CS0, sp)
fdaysum2<-summarise(grouped2, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
try2<-ggplot(fdaysum2, aes(x=sp, y=meanl))
try2+geom_point(aes(sp,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(sp,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("Treatment CS0")
#Now WS0
mag7_WS0<-filter(mag7,treatcode=="WS0")
grouped3<-group_by(mag7_WS0, sp)
fdaysum3<-summarise(grouped3, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
try3<-ggplot(fdaysum3, aes(x=sp, y=meanl))
try3+geom_point(aes(sp,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(sp,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("Treatment WS0")
#Now CL0
mag7_CL0<-filter(mag7,treatcode=="CL0")
grouped4<-group_by(mag7_CL0, sp)
fdaysum4<-summarise(grouped4, meanf=mean(fday,na.rm=TRUE),sef=sd(fday,na.rm=TRUE)/sqrt(length(fday)), meanl=mean(lday,na.rm=TRUE), sel=sd(lday,na.rm=TRUE)/sqrt(length(lday)))
try4<-ggplot(fdaysum4, aes(x=sp, y=meanl))
try4+geom_point(aes(sp,meanf,color="flowering"))+geom_pointrange(aes(ymin=meanf-sef, ymax=meanf+sef,color="flowering"))+geom_point(aes(sp,meanl, color="leafing"))+geom_pointrange(aes(ymin=meanl-sel, ymax=meanl+sel,color="leafing"))+ggtitle("Treatment CL0")
###let's seem how many flowering and leaving observations there are species in each treatment


##I used this to count how many flowers/leaf/species/treatment. manually changed the names and data sheets (should probaly learn how to make this a function)
call<-filter(mag7_CL0, sp=="VIBLAN")
View(call)
### not alot of data would including cold treatments help
mag7_WL2<-filter(mag7,treatcode=="WL2")
View(mag7_WL2)
##no
###now to sweave out a report
setwd("~/Documents/git/buds/analyses/Flo_buds")
