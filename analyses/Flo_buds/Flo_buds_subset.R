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
fb<-read.csv("Budburst By Day.csv",header = TRUE)
nrow(fb)
head(fb)
#subset data to include only entries with flowering, budburst and leaf out observations
reduced<-filter(fb,nl==1 & !is.na(fday))
nrow(reduced)
View(reduced)

write.csv(reduced, "Flo_bud_ds.csv", row.names=FALSE)
