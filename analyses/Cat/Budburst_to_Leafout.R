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

hf.buds <- hf %>%
  filter(treatcode == "CS0") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

hf.leaves <- hf %>%
  filter(treatcode == "CS0") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

hf.Risk <- hf %>%
  filter(treatcode == "CS0") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

cold<-full_join(hf.buds, hf.leaves)
cold<-full_join(cold, hf.Risk)
cold$tx<-"CS0"

warm.buds <- hf %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

warm.leaves <- hf %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

warm.Risk <- hf %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

warm<-full_join(warm.buds, warm.leaves)
warm<-full_join(warm, warm.Risk)
warm$tx<-"WL2"

hf<-bind_rows(warm,cold)
 ######## Saint Hipp Data ########
sh<- timeline%>%
  filter(site == "SH") %>%
  group_by(sp, id, tleaf) %>%
  arrange(id) %>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
sh<-na.omit(sh)
sh$Risk <- sh$Leaves - sh$Budburst
sh<-filter(sh, Risk > 0)

sh.buds <- sh %>%
  filter(treatcode == "CS0") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

sh.leaves <- sh %>%
  filter(treatcode == "CS0") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

sh.Risk <- sh %>%
  filter(treatcode == "CS0") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

cold.sh<-full_join(sh.buds, sh.leaves)
cold.sh<-full_join(cold.sh, sh.Risk)
cold.sh$tx<-"CS0"

warm.buds.sh <- sh %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

warm.leaves.sh <- sh %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

warm.Risk.sh <- sh %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

warm.sh<-full_join(warm.buds.sh, warm.leaves.sh)
warm.sh<-full_join(warm.sh, warm.Risk.sh)
warm.sh$tx<-"WL2"

sh<-bind_rows(warm.sh,cold.sh)


## from other script...
#basic$Risk<- basic$Leaves - basic$Budburst

ggplot((hf), aes(x=Budburst, y=sp)) + geom_point(aes(x= hf$Budburst)) + 
  geom_segment(aes(y = sp, yend = sp, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=hf$Leaves)) + geom_point(aes(col=tx)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")

ggplot((sh), aes(x=Budburst, y=sp)) + geom_point(aes(x= sh$Budburst)) + 
  geom_segment(aes(y = sp, yend = sp, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=sh$Leaves))  + geom_point(aes(col=tx)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")
