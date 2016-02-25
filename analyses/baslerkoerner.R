# Checking Basler & Koerner
library(scales)
library(ggplot2)
rm(list=ls()) 
options(stringsAsFactors=FALSE)

setwd("~/Documents/git/budreview")


dat<-read.csv("growthchambers_litreview_clean1.csv") # after response variables cleaned

load("Days to BB.Rdata")

dat$sp = paste(dat$genus, dat$species)

dat$response = as.numeric(dat$response)
dat$response.time = as.numeric(dat$response.time)

dat$forcetemp = as.numeric(dat$forcetemp)
dat$photoperiod_day = as.numeric(dat$photoperiod_day)

# what other studies manipulate both photoperiod and temperature, and have daystobudburst as a response?
# Loop through review database. Find studies where there are multiple values in both forcetemp and photoperiod_day, across mutiple species.

looksee <-vector()

for(i in unique(dat$datasetID)){ # i = 'basler14'
   dx <- dat[dat$datasetID == i,]
  
   if(length(unique(dx$forcetemp)) > 1 & length(unique(dx$photoperiod_day)) > 1 
      & length(unique(dx$sp)) > 1 
      #& dx$respvar == "daystobudburst"
      ){
     looksee = c(looksee, i)   
     }
  
  }

dat[match(looksee, dat$datasetID),'respvar']

# Match with days to budburst calcs

dbb <- dbb[!is.na(match(dbb$datasetID, looksee)),]
    
# First work with Basler & Koerner 2012
#bk <- dat[grep("basler", dat$datasetID),]

bk <- dat[dat$datasetID == 'basler12',]

# Calculate sensitivity to elevation vs sensistivty to photoperiod

wa = la = oa = vector()

for(i in unique(bk$sp)){ # i="Betula pendula"
  dxx <- bk[bk$sp == i & bk$respvar == 'daystobudburst',]
  
  overallm = mean(dxx$response.time, na.rm=T)
  # mean across all cool
  cm <- mean(dxx[dxx$population == 'high elevation','response.time'], na.rm=T)
  # advance from warming
  wm <- mean(dxx[dxx$population == 'low elevation','response.time'], na.rm=T)
  
  warmadv = cm - wm    
  
  # mean across all short
  sm <- mean(dxx[dxx$photoperiod_day == 'shortday','response.time'], na.rm=T)
  # advance from long day
  lm <- mean(dxx[dxx$photoperiod_day == 'longday','response.time'], na.rm=T)
  
  longadv = sm - lm   
  
  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
}
adv=data.frame(sp=unique(bk$sp), warm=wa, photo=la, overall=oa)

par(mfrow=c(1,2))

plot(warm ~ photo, data = adv, xlim = c(-2, 10),ylim=c(-18,8),
     xlab = "Advance in leafout due to photoperiod",
     ylab = "Advance in leafout due to elevation of origin",
     pch = 1, col = alpha("midnightblue",0.5), lwd = 3,
     cex = overall/5
)
text(adv$photo,adv$warm,
     labels = adv$sp, cex = 0.8, adj = 0.5,
     col = alpha('grey20', 0.9))

title(main="Days to leafout")

#dev.print(pdf, "figures/BK12 Advance plot days.pdf", width = 10, height = 12)
#system("open 'figures/BK12 Advance plot days.pdf' -a /Applications/Preview.app")


# now same, for degree days

wa = la = oa = vector()

for(i in unique(bk$sp)){ # i="Betula pendula"
  dxx <- bk[bk$sp == i & bk$respvar == 'degreedaystobudburst',]
  
  overallm = mean(dxx$response.time, na.rm=T)
  # mean across all cool
  cm <- mean(dxx[dxx$population == 'high elevation','response.time'], na.rm=T)
  # advance from warming
  wm <- mean(dxx[dxx$population == 'low elevation','response.time'], na.rm=T)
  
  warmadv = cm - wm    
  
  # mean across all short
  sm <- mean(dxx[dxx$photoperiod_day == 'shortday','response.time'], na.rm=T)
  # advance from long day
  lm <- mean(dxx[dxx$photoperiod_day == 'longday','response.time'], na.rm=T)
  
  longadv = sm - lm   
  
  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
}
adv=data.frame(sp=unique(bk$sp), warm=wa, photo=la, overall=oa)


plot(warm ~ photo, data = adv, xlim = c(-5, 62),ylim=c(-120,40),
     xlab = "Advance in leafout due to photoperiod",
     ylab = "Advance in leafout due to elevation of origin",
     pch = 1, col = alpha("midnightblue",0.5), lwd = 3,
     cex = overall/15
)
text(adv$photo,adv$warm,
     labels = adv$sp, cex = 0.8, adj = 0.5,
     col = alpha('grey20', 0.9))

title(main="Degree days to leafout")


dev.print(pdf, "/Users/danflynn/Documents/git/buds/analyses/graphs/BK12 Advance plot degreedays.pdf", width = 15, height = 10)
system("open '/Users/danflynn/Documents/git/buds/analyses/graphs/BK12 Advance plot degreedays.pdf' -a /Applications/Preview.app")

################################################################################################################
# Now B & K 2014

bk <- dat[dat$datasetID == 'basler14',]

# Calculate sensitivity to elevation vs sensistivty to photoperiod

wa = la = oa = vector()

for(i in unique(bk$sp)){ # i="Fagus sylvatica"
  dxx <- bk[bk$sp == i & bk$respvar == 'daystobudburst',]
  
  overallm = mean(dxx$response.time, na.rm=T)
  # mean across all cool
  cm <- mean(dxx[dxx$forcetemp == 6,'response.time'], na.rm=T)
  # advance from warming
  wm <- mean(dxx[dxx$forcetemp == 9,'response.time'], na.rm=T)
  
  warmadv = cm - wm    
  
  # mean across all short
  sm <- mean(dxx[dxx$photoperiod_day == 9.2,'response.time'], na.rm=T)
  # advance from long day
  lm <- mean(dxx[dxx$photoperiod_day == 10.2,'response.time'], na.rm=T)
  
  longadv = sm - lm   
  
  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
}
adv=data.frame(sp=unique(bk$sp), warm=wa, photo=la, overall=oa)

plot(warm ~ photo, data = adv, xlim = c(-2, 12),ylim=c(8,20),
     xlab = "Advance in leafout due to photoperiod",
     ylab = "Advance in leafout due to warming",
     pch = 1, col = alpha("midnightblue",0.5), lwd = 3,
     cex = overall/5
)
text(adv$photo,adv$warm,
     labels = adv$sp, cex = 0.8, adj = 0.5,
     col = alpha('grey20', 0.9))

title(main="Days to leafout")

dev.print(pdf, "/Users/danflynn/Documents/git/buds/analyses/graphs/BK14 Advance plot.pdf", width = 8, height = 8)
system("open '/Users/danflynn/Documents/git/buds/analyses/graphs/BK14 Advance plot.pdf' -a /Applications/Preview.app")

################################################################################################################

# loop across multiple studies -- right now there are none worth doing! Need to do pct bb to days conversion for 3 papers.
# 
# for(i in looksee) { # i = "caffarra11a"
#   bk <- dat[dat$datasetID == i,]
#   
#   # Calculate sensitivity to elevation vs sensistivty to photoperiod
#   
#   wa = la = oa = vector()
#   
#   for(i in unique(bk$sp)){ # i="Fagus sylvatica"
#     dxx <- bk[bk$sp == i & bk$respvar == 'daystobudburst',]
#     
#     overallm = mean(dxx$response.time, na.rm=T)
#     # mean across all cool
#     cm <- mean(dxx[dxx$forcetemp == 6,'response.time'], na.rm=T)
#     # advance from warming
#     wm <- mean(dxx[dxx$forcetemp == 9,'response.time'], na.rm=T)
#     
#     warmadv = cm - wm    
#     
#     # mean across all short
#     sm <- mean(dxx[dxx$photoperiod_day == 9.2,'response.time'], na.rm=T)
#     # advance from long day
#     lm <- mean(dxx[dxx$photoperiod_day == 10.2,'response.time'], na.rm=T)
#     
#     longadv = sm - lm   
#     
#     wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
#   }
#   adv=data.frame(sp=unique(bk$sp), warm=wa, photo=la, overall=oa)
#   
#   plot(warm ~ photo, data = adv, xlim = c(-2, 12),ylim=c(8,20),
#        xlab = "Advance in leafout due to photoperiod",
#        ylab = "Advance in leafout due to warming",
#        pch = 1, col = alpha("midnightblue",0.5), lwd = 3,
#        cex = overall/5
#   )
#   text(adv$photo,adv$warm,
#        labels = adv$sp, cex = 0.8, adj = 0.5,
#        col = alpha('grey20', 0.9))
#   
#   title(main="Days to leafout")
#   
#   dev.print(pdf, "/Users/danflynn/Documents/git/buds/analyses/graphs/BK14 Advance plot.pdf", width = 8, height = 8)
#   system("open '/Users/danflynn/Documents/git/buds/analyses/graphs/BK14 Advance plot.pdf' -a /Applications/Preview.app")
#   
