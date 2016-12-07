## Dan B- initial data visualization based on Dan F, "buds" data
##safetues
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

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
#########################
#metrics for flowering
#########################
nrow(fb)
# total rows= 2136
factor(fb$sp)
totsp<-table(fb$sp)
##28 total species in fb data sheet

##now find how many rows have entries for Fday (day of first flowering)
reduced1<-filter(fb, !is.na(fday))
nrow(reduced1)
#216 so only 216/2136 flowered at all.
##How many species
factor(reduced1$sp)
flosp<-table(reduced1$sp)
##15 species flowered so, 15/28 species flowers
##of those entries how many species had >10 individuals flower?
cnt<-count(reduced1, sp)
cnt
tenplus<-filter(cnt,n>10)
tenplus
#seven species have >10 flowering episodes:
#1 ACEPEN    11
#2 ACERUB    23
#3 CORCOR    23
#4 ILEMUC    60
#5 POPGRA    25
#6 PRUPEN    28
#7 VIBLAN    13
#How many species total leafed out
fb2<-filter(fb,nl==1)
nrow(fb2)
##1704/2136 speices leafed out
##How many burstbuds?
fb3<-filter(fb,!is.na(bday))
nrow(fb3)
#1926/2136 broke buds
#How many flowered but didnt leaf out?
fb4<-filter(fb,is.na(lday) & !is.na(fday))
nrow(fb4)
#22/2136 flowered but didnt leafout
##Did any flower but not break buds?
fb5<-filter(fb,is.na(bday) & !is.na(fday))
nrow(fb5)
#6/2136 flowered without breaking buds
## perhaps how many cutting had no activity
fb5<-filter(fb,is.na(bday) & is.na(fday)&is.na(lday))
nrow(fb5)
##204/2136 showen no activity
### how many flowers/species entries are therem
ace_pentotal<-filter(fb, sp=="ACEPEN")
nrow(ace_pentotal)
ace_rubtotal<-filter(fb, sp=="ACERUB")
nrow(ace_rubtotal)
cortotal<-filter(fb, sp=="CORCOR")
nrow(cortotal)
iltotal<-filter(fb, sp=="ILEMUC")
nrow(iltotal)
poptotal<-filter(fb, sp=="POPGRA")
nrow(poptotal)
prutotal<-filter(fb, sp=="PRUPEN")
nrow(prutotal)
vib_lantotal<-filter(fb, sp=="VIBLAN")
nrow(vib_lantotal)

#1 ACEPEN    11/144
#2 ACERUB    23/132
#3 CORCOR    23/48
#4 ILEMUC    60/144
#5 POPGRA    25/132
#6 PRUPEN    28/56
#7 VIBLAN    13/144

### all metrics are copied into Flo_buds_metrics.csv

#################################
#if you wanted to, you could subset data to include only entries with flowering, budburst and leaf out observations but this is less useful
#reduced<-filter(fb,nl==1 & !is.na(fday))
#write.csv(reduced, "Flo_bud_ds.csv", row.names=FALSE)
#write.csv(reduced1, "Flo_bud_ds_allflo.csv", row.names=FALSE)
###################################
#make subset for only species with 10+ flowering episodes
mag7<-filter(fb, sp %in% c("ACEPEN", "ACERUB", "CORCOR","ILEMUC", "POPGRA", "PRUPEN", "VIBLAN"))
View(mag7)
nrow(mag7) #800
##now lets plot the results for 1 treatment WL0
mag7_WLO<-filter(mag7,treatcode=="WL0")
count(mag7_WLO,!is.na(fday))##make sure there are flowering entries in this subset (FALSE =56 TRUE=28)
###now I want to make a new variable that is mean fday and lday seperated by species, but I am going to eat lunch
grouped<-group_by(mag7_WLO, sp)
summarise(grouped, mean=mean(fday), sd=sd(fday))
summarise(grouped, mean=mean(lday), sd=sd(lday))
