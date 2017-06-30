## Started 30 June 2017 ##
## By Lizzie ##

## Looking at climate data in response to Yann Vitasse's comments ##

## Weather data from treegarden/general/sites/WeatherData_FlynnDropbox ##
## Created in Stationdata Cleanup 2015-03-11.R (same folder) ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(ggplot2)

setwd("~/Documents/git/projects/treegarden/budexperiments/analyses")

# We'll grab the station merged data, then some more data for complete 2014-2015 #
load('input/Weather Data 2015-03-11.RData') # stationmerge

# HF Feb 2001 onward
hfdat <- read.csv("input/hf001-06-daily-m.csv", header=TRUE) # I pulled these data from HF Data archive, Fisher Meteorological Station at Harvard Forest since 2001
hfdat.sm <- subset(hfdat, select=c("date", "airtmax", "airtmin"))
hfdat.sm$date <- as.Date(hfdat.sm$date, format="%Y-%m-%d")
hfdat.sm$year <- format(hfdat.sm$date, "%Y")
names(hfdat.sm) <- c("date", "tmax", "tmin", "year")
hfdat.sm$site <- "hf"
hf.1415 <- subset(hfdat.sm, year=="2014" | year=="2015")
hf.1415 <- hf.1415[,c(1,4,2,3,5)]

## St. Hipp data in 2014-2015 (only)
stemp <- read.csv("..//data/St. Hip Weather Data.csv")
stemp$datetime <- as.POSIXlt(stemp$Date, format = "%Y-%m-%d")
stemp$year <- format(stemp$datetime, format="%Y")
stemp.sm <- subset(stemp, select=c("datetime", "year", "Tmax", "Tmin"))
stemp.sm$site <- "sh"
names(stemp.sm) <- c("date", "year", "tmax", "tmin", "site")

# all the data through Oct-Nov 2014
head(stationmerge)
stationmerge$date <- as.Date(stationmerge$Date, format="%Y-%m-%d")
stationmerge$year <- format(stationmerge$date, format="%Y")

stat.sm <- subset(stationmerge, select=c("date", "year", "AirTMax", "AirTMin", "site"))
stat.sm.00to13 <- subset(stat.sm, year>1999 & year<2014)
names(stat.sm.00to13) <- c("date", "year", "tmax", "tmin", "site")

# merge!
dat.all <- rbind(stat.sm.00to13, stemp.sm, hf.1415)
dat <- subset(dat.all, site=="sh"|site=="hf")
dat$mon <- format(dat$date, "%m")

sh <- subset(dat, site=="sh")
hf <- subset(dat, site=="hf")
unique(sh$year)
unique(hf$year)

janmay <- subset(dat, as.numeric(mon)<6)
janmar <- subset(dat, as.numeric(mon)<4)
aprmay <- subset(dat, as.numeric(mon)>3 & as.numeric(mon)<6)

unique(janmay$mon)
unique(janmar$mon)
unique(aprmay$mon)

# January to March
clim.summ.janmar <-
      ddply(janmar, c("site", "year"), summarise,
      mean.tmin = mean(tmin, na.rm=TRUE),
      mean.tmax = mean(tmax, na.rm=TRUE),
      min.tmin = min(tmin, na.rm=TRUE),
      max.tmin = max(tmin, na.rm=TRUE),
      min.tmax = min(tmax, na.rm=TRUE),
      max.tmax = max(tmax, na.rm=TRUE))

clim.summ.janmar.hf <- subset(clim.summ.janmar, site=="hf")
clim.summ.janmar.sh <- subset(clim.summ.janmar, site=="sh")
range(clim.summ.janmar.hf$mean.tmin)
range(clim.summ.janmar.hf$mean.tmax)
range(clim.summ.janmar.sh$mean.tmin)
range(clim.summ.janmar.sh$mean.tmax)

clim.summ.janmar.overall <-
      ddply(clim.summ.janmar, c("site"), summarise,
      mean.tmin = mean(mean.tmin, na.rm=TRUE),
      mean.tmax = mean(mean.tmax, na.rm=TRUE),
      mean.min.tmin = mean(min.tmin, na.rm=TRUE),
      mean.max.tmin = mean(max.tmin, na.rm=TRUE),
      mean.min.tmin = mean(min.tmax, na.rm=TRUE),
      mean.max.tmax = mean(max.tmax, na.rm=TRUE))

ggplot(clim.summ.janmar, aes(mean.tmin, fill = site)) + geom_density(alpha = 0.2)
ggplot(clim.summ.janmar, aes(mean.tmax, fill = site)) + geom_density(alpha = 0.2)

# March to May
clim.summ.aprmay <-
      ddply(aprmay, c("site", "year"), summarise,
      mean.tmin = mean(tmin, na.rm=TRUE),
      mean.tmax = mean(tmax, na.rm=TRUE),
      min.tmin = min(tmin, na.rm=TRUE),
      max.tmin = max(tmin, na.rm=TRUE),
      min.tmax = min(tmax, na.rm=TRUE),
      max.tmax = max(tmax, na.rm=TRUE))

clim.summ.aprmay.hf <- subset(clim.summ.aprmay, site=="hf")
clim.summ.aprmay.sh <- subset(clim.summ.aprmay, site=="sh")
range(clim.summ.aprmay.hf$mean.tmin)
range(clim.summ.aprmay.hf$mean.tmax)
range(clim.summ.aprmay.sh$mean.tmin)
range(clim.summ.aprmay.sh$mean.tmax)

clim.summ.aprmay.overall <-
      ddply(clim.summ.aprmay, c("site"), summarise,
      mean.tmin = mean(mean.tmin, na.rm=TRUE),
      mean.tmax = mean(mean.tmax, na.rm=TRUE),
      mean.min.tmin = mean(min.tmin, na.rm=TRUE),
      mean.max.tmin = mean(max.tmin, na.rm=TRUE),
      mean.min.tmin = mean(min.tmax, na.rm=TRUE),
      mean.max.tmax = mean(max.tmax, na.rm=TRUE))


# Counting days ... 
janmay.under1.5 <- subset(janmay, as.numeric(tmin)<1.5)
janmay.1.5to4 <- subset(janmay, as.numeric(tmin)>1.5 & as.numeric(tmin)<4)

clim.under1.5 <-
      ddply(janmay.under1.5 , c("site", "year"), summarise,
      days.under1.5 = length(tmin))

# Daylength
# From: http://www.solartopo.com/daylength.htm
# SH on 1 Mar 2015: 11:03
# SH on 1 May: 14:17
# Sh on 15 May: 14:52
# SH on 31 May: 15:24
# HF on 1 Mar: 11:10
# HF on 1 May: 14:00
# HF on 15 May: 14:33
# HF on 31 May: 15:00
# So 4 hours is about 10 weeks in Qc and 12 weeks in MA
