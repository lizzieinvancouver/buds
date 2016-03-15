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

#dat$forcetemp = as.numeric(dat$forcetemp) # clean up for B&K
#dat$photoperiod_day = as.numeric(dat$photoperiod_day)

# what other studies manipulate both photoperiod and temperature, and have daystobudburst as a response?
# Loop through review database. Find studies where there are multiple values in both forcetemp and photoperiod_day, across mutiple species.

dat$studyID = paste(dat$datasetID, dat$study, sep="-")

looksee <-vector()

for(i in unique(dat$studyID)){ # i = 'basler14-exp1'
   dx <- dat[dat$studyID == i,]
  
   if(length(unique(dx$forcetemp)) > 1 & length(unique(dx$photoperiod_day)) > 1 
      & length(unique(dx$sp)) > 1 
      #& dx$respvar == "daystobudburst"
      ){
     looksee = c(looksee, i)   
     }
  
  }
looksee

resps <- dat[match(looksee, dat$studyID),'respvar']

looksee[resps == "daystobudburst"] # can directly analyze
# Match with days to budburst calcs

dbb <- dbb[!is.na(match(dbb$studyID, looksee)),]

# convert pct bb to days to bb for these studies. Ailene's code not working for these studies yet
(tofix <- looksee[resps == "percentbudburst"])

dat_final = vector()

for(i in tofix){
 dat1 <- dat[dat$studyID==i & dat$respvar == 'percentbudburst',]
 
 treatm = unique(paste(dat1$forcetemp, dat1$photoperiod_day, sep = "_"))
 
 # go by treatment combination   
  for(treat in treatm){ # treat = "19_16"
    
      dat2 = dat1[dat1$forcetemp == unlist(strsplit(treat, "_"))[1] & dat1$photoperiod_day == unlist(strsplit(treat, "_"))[2],]
      
      # loop over species in this study
      for(sp in unique(dat2$sp)){ # sp = 'Quercus faginea' #'Betula pubescens'
        dat3 <- dat2[dat2$sp == sp,] 
        
        maxperc_bb <- max(as.numeric(dat3$response)) # maxpercent budburst
        minperc_bb <- min(as.numeric(dat3$response)) # minpercent budburst - hopefully zero
        
 # Check if this can be estimated at all.
 if(length(dat3$response[!is.na(dat3$response)])>1 & is.numeric(dat3$response.time) & length(unique(dat3$response.time))>1) {
   
   if(length(dat3[which(dat3$response==50),]$response.time)==1){ 
      dbb <- dat3[which(dat3$response==50),]$response.time } else #days to 50% bb if there is exactly one measurement at 50% bb
     
     if(length(dat3[which(dat3$response==50),]$response.time)==0){ # If there are no 50's
       
       dbb <- dat3[which(abs(dat3$response - 50) == min(abs(dat3$response-50))),]$response.time#choose closest value to 50 for percent 
       
       dbb_perc<-dat3[which(abs(dat3$response-50)==min(abs(dat3$response-50))),]$response
       # will be all zeros if both max and min are zero
     }
   
   if(dbb_perc > 55 | dbb_perc < 45 & length(dat3$response) > 1){ ##need to fix this so that it uses the latest 0 and the earliest max percent as start and end points for percent                      
     dat3$response=round(dat3$response, digits=0)
     dat4 <- dat3[c(which(dat3$response==min(dat3$response, na.rm=TRUE)), min(which(dat3$response==max(dat3$response,na.rm=TRUE)),na.rm=TRUE)),]
     
     mod <- lm(as.numeric(dat4$response.time)~as.numeric(dat4$response))  #only use days to bb estimate if the percent is >45 or <55. If not, then use regression to get estimate for days to 50 percent
     dbb <- as.numeric(coef(mod)[1]+coef(mod)[2]*50)
     
   }
   
   if(dbb_perc > 55 | dbb_perc < 45 & length(na.omit(dat3$response)) < 2){ ### If fewer than two   
     dat3$response=round(dat3$response, digits=0)
     dat4<-dat3[c(which(dat3$response==min(dat3$response, na.rm=TRUE)), min(which(dat3$response==max(dat3$response,na.rm=TRUE))),na.rm=TRUE),]
     mod <-lm(as.numeric(dat4$response.time)~as.numeric(dat4$response))  #only use days to bb estimate if the percent is >45 or <55. If not, then use regression to get estimate for days to 50 percent
     dbb <- as.numeric(coef(mod)[1]+coef(mod)[2]*50)
     
   } 
   
 } else dbb = NA # end check of estimatability
    
    sum_dbb<-cbind(i, sp, dbb, maxperc_bb, minperc_bb, treat)
    
    colnames(sum_dbb)[1] <- "studyID"
    
    dat_final<- rbind(dat_final,sum_dbb)  
      } # end sp loop
  } # end treatment loop
 } # end study loop


dat_final <- as.data.frame(dat_final)

    
################################################################################################################
# B & K 2014

bk <- dat[dat$studyID == 'basler14-exp1',]

# Calculate sensitivity to elevation vs sensistivty to photoperiod

wa = la = oa = vector()

for(i in unique(bk$sp)){ # i="Fagus sylvatica"
  dxx <- bk[bk$sp == i & bk$respvar == 'daystobudburst',]
  
  overallm = mean(dxx$response.time, na.rm=T)
  # mean across all cool
  cm <- mean(dxx[dxx$forcetemp == 6,'response.time'], na.rm=T)
  # advance from warming
  wm <- mean(dxx[dxx$forcetemp == 9,'response.time'], na.rm=T)
  
  warmadv = wm - cm  
  
  # mean across all short
  sm <- mean(dxx[dxx$photoperiod_day == 9.2,'response.time'], na.rm=T)
  # advance from long day
  lm <- mean(dxx[dxx$photoperiod_day == 10.2,'response.time'], na.rm=T)
  
  longadv = lm - sm 
  
  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
}
adv=data.frame(study = "BK14", sp=unique(bk$sp), warm=wa, photo=la, overall=oa)

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
# 
# loop across multiple studies -- right now only Sanz-Perez worth doing! 

  bk <- dat_final[dat_final$studyID == 'Sanz-Perez09-exp1',]
  bk$forcetemp = substr(bk$treat, 1,2)
  bk$photoperiod_day = substr(bk$treat, 4,5)
  bk$dbb <- as.numeric(bk$dbb)
  # Calculate sensitivity to elevation vs sensistivty to photoperiod
  
  wa = la = oa = vector()
  
  for(i in unique(bk$sp)){ # i="Quercus faginea"
    dxx <- bk[bk$sp == i,]
    
    overallm = mean(dxx$dbb, na.rm=T)
    # mean across all cool
    cm <- mean(dxx[dxx$forcetemp == 12,'dbb'], na.rm=T)
    # advance from warming
    wm <- mean(dxx[dxx$forcetemp == 19,'dbb'], na.rm=T)
    
    warmadv = wm-cm
    
    # mean across all short
    sm <- mean(dxx[dxx$photoperiod_day == 10,'dbb'], na.rm=T)
    # advance from long day
    lm <- mean(dxx[dxx$photoperiod_day == 16,'dbb'], na.rm=T)
    
    longadv = lm - sm
    
    wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
  }
  
  adv = rbind(adv, data.frame(study = "SP09", sp=unique(bk$sp), warm=wa, photo=la, overall=oa))
  
  #ggplot(adv, aes(warm, photo, group = study, size = overall)) + geom_point(aes(color = study)) + 
      
  ggplot(adv, aes(warm, photo)) + geom_point(pch = 16, size = 5) + facet_grid(.~study) + xlab("Advance due to warming") + ylab("Advance due to photoperiod") + geom_text(aes(label = sp, size = 8), nudge_y = 1) + theme_bw() + xlim(-65, -5) + theme(legend.position = 'none')
    
  dev.print(pdf, "/Users/danflynn/Documents/git/buds/analyses/graphs/Cross Study Advance plot.pdf", width = 14, height = 7)
  system("open '/Users/danflynn/Documents/git/buds/analyses/graphs/Cross Study Advance plot.pdf' -a /Applications/Preview.app")
  
  
  
## previous
  
  
  # First work with Basler & Koerner 2012
  #bk <- dat[grep("basler", dat$datasetID),]
  
  bk <- dat[dat$studyID == 'basler12',]
  
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
  
  #par(mfrow=c(1,2))
  
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
  
  dev.print(pdf, "figures/BK12 Advance plot days.pdf", width = 10, height = 12)
  system("open 'figures/BK12 Advance plot days.pdf' -a /Applications/Preview.app")
  
  
  # now same, for degree days
  # 
  # wa = la = oa = vector()
  # 
  # for(i in unique(bk$sp)){ # i="Betula pendula"
  #   dxx <- bk[bk$sp == i & bk$respvar == 'degreedaystobudburst',]
  #   
  #   overallm = mean(dxx$response.time, na.rm=T)
  #   # mean across all cool
  #   cm <- mean(dxx[dxx$population == 'high elevation','response.time'], na.rm=T)
  #   # advance from warming
  #   wm <- mean(dxx[dxx$population == 'low elevation','response.time'], na.rm=T)
  #   
  #   warmadv = cm - wm    
  #   
  #   # mean across all short
  #   sm <- mean(dxx[dxx$photoperiod_day == 'shortday','response.time'], na.rm=T)
  #   # advance from long day
  #   lm <- mean(dxx[dxx$photoperiod_day == 'longday','response.time'], na.rm=T)
  #   
  #   longadv = sm - lm   
  #   
  #   wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
  # }
  # adv=data.frame(sp=unique(bk$sp), warm=wa, photo=la, overall=oa)
  # 
  # 
  # plot(warm ~ photo, data = adv, xlim = c(-5, 62),ylim=c(-120,40),
  #      xlab = "Advance in leafout due to photoperiod",
  #      ylab = "Advance in leafout due to elevation of origin",
  #      pch = 1, col = alpha("midnightblue",0.5), lwd = 3,
  #      cex = overall/15
  # )
  # text(adv$photo,adv$warm,
  #      labels = adv$sp, cex = 0.8, adj = 0.5,
  #      col = alpha('grey20', 0.9))
  # 
  # title(main="Degree days to leafout")
  
  
  dev.print(pdf, "/Users/danflynn/Documents/git/buds/analyses/graphs/BK12 Advance plot degreedays.pdf", width = 15, height = 10)
  system("open '/Users/danflynn/Documents/git/buds/analyses/graphs/BK12 Advance plot degreedays.pdf' -a /Applications/Preview.app")
  