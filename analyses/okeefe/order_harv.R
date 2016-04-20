### Started 14 April 2016 ###
### By Lizzie ###


## housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(ggplot2)
library(tidyr)

setwd("~/Documents/git/projects/treegarden/okeefe/analyses")

if(length(grep("danflynn", getwd()))>0){ setwd("~/Documents/git/buds/analyses/okeefe") }
source('../DF General Functions.R')

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

### Comparing with experimental data

dx <- read.csv("../Species x Treat BB LO for OKeefe.csv")
dx$treat = paste(
        factor(dx$warm, labels = c("C","W")),
        factor(dx$photo, labels = c("S","L")),
        dx$chill1,
        dx$chill2, sep="")

bbd$sp <- unlist(lapply(strsplit(bbd$latbi, split = " "), 
              function(x) toupper(paste(substr(x[1],1,3), substr(x[2],1,3), sep = "") )) )

lo <- vector()

for(i in unique(bbd$year)){ # i = '1990'
  dxx <- bbd[bbd$year == i,]
  lox <- rank(dxx$doy, na.last = T)
  lo = c(lo, lox)
}

bbd <- data.frame(bbd, leafout.order = lo)

ggplot(bbd, aes(year, leafout.order, group = sp)) + geom_line(aes(color = sp)) 
# match to our experimental species
# bbd <- bbd[!is.na(match(bbd$sp, unique(dx$sp))),]
ggplot(bbd[!is.na(match(bbd$sp, unique(dx$sp))),], aes(year, leafout.order, group = sp)) + geom_line(aes(color = sp)) 

mean.order.1 <- aggregate(leafout.order ~ sp, mean, data = bbd[bbd$year < 2003,])
se.order.1 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = bbd[bbd$year < 2003,])
mean.order.2 <- aggregate(leafout.order ~ sp, mean, data = bbd[bbd$year >= 2003,])
se.order.2 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = bbd[bbd$year >= 2003,])

# are the orders even internally consistent?
xx <- merge(mean.order.1, mean.order.2, by = 'sp', all=T)
with(xx, plot(leafout.order.x, leafout.order.y)) # not so much! So...

# use just last 5 years maybe
mean.order.3 <- aggregate(leafout.order ~ sp, mean, data = bbd[bbd$year >= 2011,])
se.order.3 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = bbd[bbd$year >= 2011,])

ok <- data.frame(mean.order.3, hf.bbd.order.se = se.order.3$leafout.order)
#quartz()
pdf("leafout exp obs corr.pdf", width = 12, height = 8)

par(mfrow = c(3, 4), xpd=T, mar = c(4,4,2,1))

counter = 1
for(i in unique(dx$treats)){ # i = "1 1 0 0"
  dxx <- dx[dx$treats == i,]
  
  dxx <- dxx[!is.na(match(dxx$sp, ok$sp)),]
  ok2 <- ok[!is.na(match(ok$sp, dxx$sp)),]
  identical(dxx$sp, ok2$sp)
  
  plot(ok2$leafout.order, dxx$leafout.order, pch = "+", 
       xlim = c(min(c(ok2$leafout.order, dxx$leafout.order),na.rm=T)*0.9,
               max(c(ok2$leafout.order, dxx$leafout.order),na.rm=T)*1.1),
      ylim = c(min(c(ok2$leafout.order, dxx$leafout.order),na.rm=T)*0.9,
                max(c(ok2$leafout.order, dxx$leafout.order),na.rm=T)*1.1),
    xlab = "Harvard Forest Observed",
    ylab = "Experimental"
      )
       
  text(ok2$leafout.order, dxx$leafout.order, labels =ok2$sp, cex = 0.8, pos = 3)
  
  
  #do.lm(ok2$leafout.order, dxx$leafout.order)
  
  
  print(summary.aov(lm.result <- lm(dxx$leafout.order ~ ok2$leafout.order)))
  
  if(counter < 5)   legend("bottomright",legend = c(unique(dxx$treat),
                                                    paste("r2 =", round(summary(lm.result)$r.squared, 3))), inset=0)
  else legend("topleft",legend = c(unique(dxx$treat),
                                   paste("r2 =", round(summary(lm.result)$r.squared, 3))), inset=0)
  
  counter = counter + 1
} 

dev.off();system("open 'leafout exp obs corr.pdf' -a /Applications/Preview.app")