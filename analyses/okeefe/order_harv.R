### Started 14 April 2016 ###
### By Lizzie ###


## housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(ggplot2)
library(tidyr)

setwd("~/Documents/git/projects/treegarden/okeefe/analyses")

dat <- read.csv("output/okeefe.csv", header=TRUE)
dat$latbi <- paste(dat$genus, dat$species)
dat <- subset(dat, select=c("event","year","doy","latbi"))

bbd <- subset(dat, event=="bbd")
ffd <- subset(dat, event=="ffd")

## common spp
bbd.freq <- as.data.frame(table(bbd$latbi))
bbd.com <- subset(bbd.freq, Freq>20)
bbd.comspp <- bbd[which(bbd$latbi %in% bbd.com$Var1),]

## poor attempt at a quick plot to get my answer 
dat3year <- subset(bbd.comspp, year=="2012" | year=="2014" |  year=="1992")

dat3year.long <- spread(dat3year, key=year, value=doy)
names(dat3year.long) <- c("event", "latbi", "yr1992", "yr2012", "yr2014")

dat3year.long.all <- na.omit(dat3year.long)
dim(dat3year.long.all)
dim(dat3year.long)

ggplot(subset(dat3year.long.all, event="bbd"), aes(yr1992, yr2012, color=latbi)) +
    geom_point()

## order ##
bbd.worder <- bbd[1,]
bbd.worder$order <- NA
bbd.worder <- bbd.worder[-1,]

yearz <- unique(bbd$year)

for (i in 1:length(yearz)){
    subby <- subset(bbd, year==yearz[i])
    subby$order <- cut(subby$doy, breaks=length(unique(subby$doy)),
        labels=FALSE)
    bbd.worder <- rbind(bbd.worder, subby)
}

bbd.worder.com <- bbd.worder[which(bbd.worder$latbi %in% bbd.com$Var1),]

ggplot(bbd.worder.com, aes(x=as.numeric(as.character(order)))) +
    geom_histogram(binwidth=2,colour="white") +
    facet_grid(~latbi) # blargg

quickorder <- aggregate(bbd.worder.com["order"], bbd.worder.com["latbi"],
    FUN=median)

quickorder[with(quickorder, order(order)), ]
