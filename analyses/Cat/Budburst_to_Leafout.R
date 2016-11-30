## Making a Distance from Budburst to Leaf Out Script
# In Buds folder using Dan Flynn's data
# Clear workspace

## 30 November 2016 - Cat
## Attempt to create a timeline chart and eventally use Stan to determine
## the effects of latitude and temperature on risk time
## This is take two with a new datasheet from the USNPN, follows bb.leafout.R and darewearm.R
# Using Dan's Data!!

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set Working Directory
setwd("~/Documents/git/buds/analyses")
timeline<-read.csv("input/Budburst.csv",header=TRUE)

# Convert to Julian day
timeline$DOY<-yday(timeline$Date)
phases<-c("4","7")
tx<-c("CS0","WL2") # may want to use WL1 instead?
timeline<-timeline %>%
  select(id, sp, site, tleaf, DOY, treatcode) %>%
  filter(treatcode %in% tx) %>%
  filter(tleaf %in% phases)
timeline$tleaf<- factor(timeline$tleaf, levels = c(4,7), 
       labels = c("Budburst","Leaves"))
  
hf<- timeline%>%
  filter(site == "HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
hf<-na.omit(hf)
hf$Risk <- hf$Leaves - hf$Budburst
hf<-filter(hf, Risk > 0)

sh<- timeline%>%
  filter(site == "SH") %>%
  group_by(sp, id, tleaf) %>%
  arrange(id) %>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
sh<-na.omit(sh)
sh$Risk <- sh$Leaves - sh$Budburst
sh<-filter(sh, Risk > 0)


## from other script...
#basic$Risk<- basic$Leaves - basic$Budburst

ggplot((hf), aes(x=Budburst, y=id)) + geom_point(aes(x= hf$Budburst)) + 
  geom_segment(aes(y = id, yend = id, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=hf$Leaves)) +
  geom_point(position = position_dodge(.5)) + geom_point(aes(col=treatcode)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")

ggplot((sh), aes(x=Budburst, y=id)) + geom_point(aes(x= sh$Budburst)) + 
  geom_segment(aes(y = id, yend = id, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=sh$Leaves)) +
  geom_point(position = position_dodge(.5)) + geom_point(aes(col=treatcode)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")
