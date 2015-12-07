# Calculating chill units

setwd("~/Documents/git/buds/analyses")
print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])
load(file.path("input", toload))


# Dynamic Model

library(chillR)
# Luedeling recommends Dynamic Modle (chill portions)

# HF chilling from Oct 1 2014


htemp <- read.csv("~/Documents/git/buds/analyses/data/hf001-10-15min-m.csv")


# aggregate to hourly averages 

htemp$datetime <- as.POSIXlt(htemp$datetime, format = "%Y-%M-%dT%H:%M")

# select years

htemp <- htemp[htemp$datetime > "2014-09-30" & htemp$datetime < "2015-05-01",]

weather<-make_all_day_table(KA_weather[which(KA_weather$Year>2006),])
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp
 
THourly<-make_hourly_temps(50.4,weather)
 
stack<-stack_hourly_temps(hour_file=THourly)
 
chilling(stack,305,60)