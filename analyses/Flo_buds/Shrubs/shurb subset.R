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
library(car)
library(mass)
setwd("~/Documents/git/buds/analyses/input")
fb<-read.csv("Budburst By Day.csv", header = TRUE)
#make subset for the good shurbs leftover from Mag 7
goodshrubs<-filter(fb, sp %in% c( "CORCOR","ILEMUC", "PRUPEN"))
############################ploting the data########################
shrubgroup<-group_by(goodshrubs,sp,treatcode)
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
###################################################
###Anovas#########################################
sapply(goodshrubs, mode)
###make warm and photo characters so anova can be used. Is this correct? 
###the results make more sense to me when I do this, but I should ask Lizze
goodshrubs$warm<-as.character(goodshrubs$warm) 
goodshrubs$photo<-as.character(goodshrubs$photo) 
####now with species## 
CORCOR<-filter(goodshrubs, sp=="CORCOR")
PRUPEN<-filter(goodshrubs, sp=="PRUPEN")
ILEMUC<-filter(goodshrubs, sp=="ILEMUC")
ILEMUC2<-filter(ILEMUC, treatcode %in% c( "CL0","CS0", "WL0","WS0"))
###make ANOVA by species
CORflo=aov(fday~warm*photo,data=CORCOR)
summary.lm(CORflo)
CORleaf = aov(lday~warm*photo,data=CORCOR)
summary.lm(CORleaf)
PRUflo = aov(fday~warm*photo,data=PRUPEN)
summary.lm(PRUflo)
PRUleaf = aov(lday~warm*photo,data=PRUPEN)
summary.lm(PRUleaf)
ILEflo = aov(fday~warm*photo,data=ILEMUC2)
summary.lm(ILEflo)
ILEleaf = aov(lday~warm*photo,data=ILEMUC2)
summary.lm(ILEleaf)
###interactions are non-significant, run them again without interactions
CORflo2<-aov(fday~warm+photo,data=CORCOR)
summary.lm(CORflo2)
CORleaf2<- aov(lday~warm+photo,data=CORCOR)
summary.lm(CORleaf2)
PRUflo2<-aov(fday~warm+photo,data=PRUPEN)
summary.lm(PRUflo2)
PRUleaf2<-aov(lday~warm+photo,data=PRUPEN)
summary.lm(PRUleaf2)
ILEflo2<-aov(fday~warm+photo,data=ILEMUC2)
summary.lm(ILEflo2)
ILEleaf2<- aov(lday~warm+photo,data=ILEMUC2)
summary.lm(ILEleaf2)


#######diagnostic plots###########################################################################
par(mfrow=c(3,2)) 
plot(CORflo,which=c(1,2),"CORflo") 
plot(CORleaf,which=c(1,2),"CORleaf") 
plot(PRUflo,which=c(1,2),"PRUflo") 
plot(PRUleaf,which=c(1,2),"PRUleaf") 
plot(ILEflo,which=c(1,2),"ILEflo") 
plot(ILEleaf,which=c(1,2),"ILEleaf") 
#######################################
#### now try to combine it into one model-
####not currently using in evaluation because cant figure out how to interpret 
COR<-gather(CORCOR,"phenophase","eventday",lday:fday)
CORfull<-aov(eventday~phenophase*warm*photo, data=COR)
summary.lm(CORfull)
TukeyHSD(CORfull)
### sensativities
-3.055/5
6.328/4
-15.819/5 
-14.021/4

-21.106/5
-10.271/4
-13.935/5
-6.731/4

-11.230/5
-4.761/4
-14.458/5
-7.542/4 


