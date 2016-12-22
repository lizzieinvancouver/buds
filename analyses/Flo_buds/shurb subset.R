## Dan B- initial data visualization based on Dan F, "buds" data
##safetues
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()
library(gdata)
library(nlme)
library(scales)
library(arm)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Documents/git/buds/analyses/input")
fb<-read.csv("Budburst By Day.csv", header = TRUE)
#make subset for the good shurbs leftover from Mag 7
goodshrubs<-filter(fb, sp %in% c( "CORCOR","ILEMUC", "PRUPEN"))
shrubgroup<-group_by(goodshrubs,sp,treatcode)
###########################
#plot the next to each other
redo<-gather(shrubgroup,phenophase,eventday,fday:lday)
redo<-filter(redo, treatcode %in% c( "CL0","CS0", "WL0","WS0"))
q<-ggplot(redo, aes(x=treatcode, y=eventday, color=phenophase)) +
  stat_summary()+labs(title="First flower and leaf out by treatment per species", x="Treatment", y="Days since initiation")
q+facet_wrap(~sp)
####Look at just Ilex because there is chilling
ILEMUC<-filter(redo, sp=="ILEMUC")
View(ILEMUC)
IM<-ggplot(ILEMUC, aes(x=treatcode, y=eventday, color=phenophase)) +
  stat_summary()+labs(title=" Ilex mucronata first flower and leaf out by treatment per species", x="Treatment", y="Days since initiation")
IM
###Anovas
View(goodshrubs)
sapply(goodshrubs, mode)
###make characters
goodshrubs$warm<-as.character(goodshrubs$warm) 
goodshrubs$photo<-as.character(goodshrubs$photo) 

###main anova
anovaflower = aov(fday~warm*photo*sp,data=goodshrubs)
summary(anovaflower)
anovaleaf = aov(lday~warm*photo*sp,data=goodshrubs)
summary(anovaleaf)
par(mfrow=c(1,2)) 
plot(anovaflower,which=c(1,2),"Flo") 
plot(anovaleaf,which=c(1,2),"Leaf") 

####now with species
CORCOR<-filter(goodshrubs, sp=="CORCOR")
PRUPEN<-filter(goodshrubs, sp=="PRUPEN")
ILEMUC<-filter(goodshrubs, sp=="ILEMUC")
ILEMUC2<-filter(ILEMUC, treatcode %in% c( "CL0","CS0", "WL0","WS0"))
###make ANOVA by species
CORflo = aov(fday~warm*photo,data=CORCOR)
summary(CORflo)
CORleaf = aov(lday~warm*photo,data=CORCOR)
summary(CORleaf)
#####
PRUflo = aov(fday~warm*photo,data=PRUPEN)
summary(PRUflo)
PRUleaf = aov(lday~warm*photo,data=PRUPEN)
summary(PRUleaf)
###
ILEflo = aov(fday~warm*photo,data=ILEMUC2)
summary(ILEflo)
ILEleaf = aov(lday~warm*photo,data=ILEMUC2)
summary(ILEleaf)
### if interactions are non-significant, do you run them again without interactions

par(mfrow=c(1,2)) 
plot(CORflo,which=c(1,2),"CORflo") 
plot(CORleaf,which=c(1,2),"CORleaf") 
plot(PRUflo,which=c(1,2),"PRUflo") 
plot(PRUleaf,which=c(1,2),"PRUleaf") 
plot(ILEflo,which=c(1,2),"ILEflo") 
plot(ILEleaf,which=c(1,2),"ILEleaf") 