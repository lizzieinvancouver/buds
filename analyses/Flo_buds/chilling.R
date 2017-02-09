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
###species with 5 or more folowering
mag11<-filter(fb, sp %in% c("ACEPEN", "ACERUB","ALNINC","CORCOR","ILEMUC","LONCAN", "POPGRA", "PRUPEN", "VIBLAN","RHOPRI","VIBCAS"))
group<-group_by(mag11,sp)
groupo<-gather(group,phenophase,eventday,fday:lday)
View(groupo)

p1<-ggplot(groupo, aes(x=treatcode, y=eventday, color=phenophase)) +
  stat_summary()+labs(title="First flower and leaf out by treatment per species", x="Treatment", y="Days since initiation")
p1+facet_wrap(~sp)
###subset to only ones with chilling
mag6<-filter(groupo, sp %in% c("ACEPEN", "ACERUB","ILEMUC", "POPGRA", "VIBLAN","VIBCAS"))
p2<-ggplot(mag6, aes(x=treatcode, y=eventday, color=phenophase)) +
  stat_summary()+labs(title="First flower and leaf out by treatment per species", x="Treatment", y="Days since initiation")
p2+facet_wrap(~sp)
group$warm<-as.character(group$warm) 
group$photo<-as.character(group$photo)

#### ANOVAS
sapply(group, mode)
ACEPEN<-filter(group, sp=="ACEPEN")
ACERUB<-filter(group, sp=="ACERUB")
ILEMUC<-filter(group, sp== "ILEMUC")
POPGRA<-filter(group, sp=="POPGRA")
ApFlo<-aov(fday~photo+warm+chill, data=ACEPEN)
summary.lm(ApFlo)
ApLeaf<-aov(lday~photo+warm+chill, data=ACEPEN)
summary.lm(ApLeaf)
###
AcFlo<-aov(fday~photo+warm+chill, data=ACERUB)
summary.lm(AcFlo)
AcLeaf<-aov(lday~photo+warm+chill, data=ACERUB)
summary.lm(AcLeaf)
###
ImFlo<-aov(fday~photo+warm+chill, data=ILEMUC)
summary.lm(ImFlo)
ImLeaf<-aov(lday~photo+warm+chill, data=ILEMUC)
summary.lm(ImLeaf)
###
PgFlo<-aov(fday~photo+warm+chill, data=POPGRA)
summary.lm(PgFlo)
PgLeaf<-aov(lday~photo+warm+chill, data=POPGRA)
summary.lm(PgLeaf)
  
##from above ANOVA,chilling seems really important for flowering especially in earliers flowers