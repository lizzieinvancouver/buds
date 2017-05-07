### Started 14 April 2016 ###
### By Lizzie and Dan Flynn ###

# okeefe.csv = date of first event for any individual of that species, in that year

## housekeeping
#rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(ggplot2)
library(tidyr)

if(length(grep("danflynn", getwd()))==0){ setwd("~/Documents/git/projects/treegarden/budexperiments/analyses/okeefe") }

# if(length(grep("danflynn", getwd()))>0){ setwd("~/Documents/git/buds/analyses/okeefe") }


d <- read.csv("../../data/hf003-06-mean-spp.csv")
splist <- read.csv("input/specieslist_bylizzie.csv")
d$latbi <- splist[match(d$sp, splist$code), "genusspecies"]

# old version, using 'first' data file
# dat <- read.csv("output/okeefe.csv", header=TRUE)
# dat$latbi <- paste(dat$genus, dat$species)
# dat <- subset(dat, select=c("event","year","doy","latbi"))
# 
# bbd <- subset(dat, event=="bbd")
# ffd <- subset(dat, event=="ffd")
# 
# ## common spp
# bbd.freq <- as.data.frame(table(bbd$latbi))
# bbd.com <- subset(bbd.freq, Freq>20)
# bbd.comspp <- bbd[which(bbd$latbi %in% bbd.com$Var1),]
# 
# ## poor attempt at a quick plot to get my answer 
# dat3year <- subset(bbd.comspp, year=="2012" | year=="2014" |  year=="1992")
# 
# dat3year.long <- spread(dat3year, key=year, value=doy)
# names(dat3year.long) <- c("event", "latbi", "yr1992", "yr2012", "yr2014")
# 
# dat3year.long.all <- na.omit(dat3year.long)
# dim(dat3year.long.all)
# dim(dat3year.long)
# 
# ggplot(subset(dat3year.long.all, event="bbd"), aes(yr1992, yr2012, color=latbi)) +
#     geom_point()
# 
# ## order ##
# bbd.worder <- bbd[1,]
# bbd.worder$order <- NA
# bbd.worder <- bbd.worder[-1,]
# 
# yearz <- unique(bbd$year)
# 
# for (i in 1:length(yearz)){
#     subby <- subset(bbd, year==yearz[i])
#     subby$order <- cut(subby$doy, breaks=length(unique(subby$doy)),
#         labels=FALSE)
#     bbd.worder <- rbind(bbd.worder, subby)
# }
# 
# bbd.worder.com <- bbd.worder[which(bbd.worder$latbi %in% bbd.com$Var1),]
# 
# ggplot(bbd.worder.com, aes(x=as.numeric(as.character(order)))) +
#     geom_histogram(binwidth=2,colour="white") +
#     facet_grid(~latbi) # blargg
# 
# quickorder <- aggregate(bbd.worder.com["order"], bbd.worder.com["latbi"],
#     FUN=median)
# 
# quickorder[with(quickorder, order(order)), ]

### Comparing with experimental data

dx <- read.csv("../Species x Treat BB LO for OKeefe.csv")
dx$treat = paste(
        factor(dx$warm, labels = c("C","W")),
        factor(dx$photo, labels = c("S","L")),
        dx$chill1,
        dx$chill2, sep="")

d$sp <- unlist(lapply(strsplit(d$latbi, split = " "), 
              function(x) toupper(paste(substr(x[1],1,3), substr(x[2],1,3), sep = "") )) )

lo <- bb <- sp <- yr <- vector()

for(i in unique(d$year)){ # i = '2000'
  dxx <- d[d$year == i,]
  lox <- rank(dxx$l75.jd)
  lox[is.na(dxx$l75.jd)] = NA
  box <- rank(dxx$bb.jd, na.last = T)
  box[is.na(dxx$bb.jd)] = NA
  lo = c(lo, lox)
  bb = c(bb, box)
  sp = c(sp, dxx$sp)
  yr = c(yr, rep(i, nrow(dxx)))
  }

d1 <- data.frame(year = yr, sp, leafout.order = lo, budburst.order = bb, l75 = d$l75.jd)

# ggplot(d1, aes(year, leafout.order, group = sp)) + geom_line(aes(color = sp)) 
# ggplot(d1, aes(year, budburst.order, group = sp)) + geom_line(aes(color = sp)) 
# 
# ggplot(d1, aes(leafout.order, budburst.order, group = sp)) + geom_point(aes(color = sp)) #  later lo species are also later bb species, but early ones vary a lot


# match to our experimental species
# d <- d[!is.na(match(d$sp, unique(dx$sp))),]
ggplot(d1[!is.na(match(d1$sp, unique(dx$sp))),], aes(year, leafout.order, group = sp)) + geom_line(aes(color = sp)) 

mean.order.1 <- aggregate(leafout.order ~ sp, mean, data = d1[d1$year < 2003,])
se.order.1 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year < 2003,])
mean.order.2 <- aggregate(leafout.order ~ sp, mean, data = d1[d1$year >= 2003,])
se.order.2 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year >= 2003,])

bmean.order.1 <- aggregate(budburst.order ~ sp, mean, data = d1[d1$year < 2003,])
bse.order.1 <- aggregate(budburst.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year < 2003,])
bmean.order.2 <- aggregate(budburst.order ~ sp, mean, data = d1[d1$year >= 2003,])
bse.order.2 <- aggregate(budburst.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year >= 2003,])


# are the orders even internally consistent?
par(mfrow =c(1,2))
xx <- merge(mean.order.1, mean.order.2, by = 'sp', all=T)
bxx <- merge(bmean.order.1, bmean.order.2, by = 'sp', all=T)
with(xx, plot(leafout.order.x, leafout.order.y)) # Yes, not bad!
with(bxx, plot(budburst.order.x, budburst.order.y)) # Quite good actually

# use just last 10 years maybe -- would omit vibernums and others. So use whole data set
mean.order.3 <- aggregate(leafout.order ~ sp, mean, data = d1)
se.order.3 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1)

bmean.order.3 <- aggregate(budburst.order ~ sp, mean, data = d1)
bse.order.3 <- aggregate(budburst.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1)


mean.lo.3 <- aggregate(l75 ~ sp, mean, data = d1)
se.lo.3 <- aggregate(l75 ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1)



ok <- data.frame(mean.order.3, hf.d1.order.se = se.order.3$leafout.order, budburst.order = bmean.order.3$budburst.order, hf.b.order.se = bse.order.3$budburst.order,
                 hf.lo = mean.lo.3, hf.lo.se = se.lo.3)
#quartz()
pdf("leafout_exp_obs_corr.pdf", width = 15, height =12)

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
     #xlim = c(2, 34),
     # ylim = c(2, 34),
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

dev.off()#;system("open 'leafout_exp_obs_corr.pdf' -a /Applications/Preview.app")

#### now just by day, not by order

pdf("leafout_exp_obs_corr_day.pdf", width = 15, height =12)

par(mfrow = c(3, 4), xpd=T, mar = c(4,4,2,1))

counter = 1
for(i in unique(dx$treats)){ # i = "1 1 0 0"
  dxx <- dx[dx$treats == i,]
  
  dxx <- dxx[!is.na(match(dxx$sp, ok$sp)),]
  ok2 <- ok[!is.na(match(ok$sp, dxx$sp)),]
  stopifnot(identical(dxx$sp, ok2$sp))
  
  plot(ok2$hf.lo.l75, dxx$lday, pch = "+", 
       xlim = c(145, 158),
       ylim = c(18, 85),
       xlab = "Harvard Forest Observed",
       ylab = "Experimental"
  )
  
  text(ok2$hf.lo.l75, dxx$lday, labels = ok2$sp, cex = 0.8, pos = 3)
  
  #do.lm(ok2$leafout.order, dxx$leafout.order)
  
  
  print(summary.aov(lm.result <- lm(dxx$lday ~ ok2$hf.lo.l75)))
  
  if(counter < 5)   legend("bottomright",legend = c(unique(dxx$treat),
                                                    paste("r2 =", round(summary(lm.result)$r.squared, 3))), inset=0)
  else legend("topleft",legend = c(unique(dxx$treat),
                                   paste("r2 =", round(summary(lm.result)$r.squared, 3))), inset=0)
  
  counter = counter + 1
} 

dev.off()#;system("open 'leafout_exp_obs_corr_day.pdf' -a /Applications/Preview.app")



# is ACESAC really that early??

ggplot(d1[d1$year > 2002,], aes(year, leafout.order, group = sp)) + geom_line(aes(color = sp)) 

ggplot(d[d$year > 2002,], aes(year, l75.jd, group = sp)) + geom_line(aes(color = sp)) 

