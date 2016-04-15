# Additional plotting for lizzie 2016-04-04
# also bunch of extra stuff in general
#library(plotrix)#for draw.ellipse
# From loaded data 
groups = treeshrub
  
#exclude non-chill sp from chill parameter plotting. easiest to give a big number (otherwise have to also subset other parameters when plotting pairwise)
sumerb <- summary(doym.b)$summary
gotchill <- tapply(dx$spn, dx$chill2, unique)$'1'
nochill <- unique(dx$spn)[is.na(match(unique(dx$spn), gotchill))]
sumerb[!is.na(match(rownames(sumerb), paste("b_chill1[", nochill, "]", sep=""))),] = 99
sumerb[!is.na(match(rownames(sumerb), paste("b_chill2[", nochill, "]", sep=""))),] = 99

sumerl <- summary(doym.l)$summary
sumerl[!is.na(match(rownames(sumerl), paste("b_chill1[", nochill, "]", sep=""))),] = 99
sumerl[!is.na(match(rownames(sumerl), paste("b_chill2[", nochill, "]", sep=""))),] = 99


## budburst

pdf(file.path(figpath, "stanbb_1.pdf"), width = 8, height = 8)  

plotlet("b_warm","b_photo",
        group = treeshrub,
        xlab = "Advance due to 5° warming",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerb,
        xlim = c(-14, 0.5),
        ylim = c(-11, 0.5)
        )

legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

dev.off();system(paste("open", file.path(figpath, "stanbb_1.pdf"), "-a/Applications/Preview.app"))

### 3-way with chilling

pdf(file.path(figpath, "stanbb_3.pdf"), width = 10, height = 8)  

par(mfrow=c(2,3))

plotlet("b_warm","b_photo",
        group = treeshrub,
        xlab = "Advance due to 5° warming",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerb,
        xlim = c(-14, 0.5),
        ylim = c(-11, 0.5))

plotlet("b_chill1","b_photo",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 4°C",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerb,
        xlim = c(-33, -13),
        ylim = c(-11, 0.5))

plotlet("b_chill2","b_photo",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 1.5°C",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerb,
        xlim = c(-33, -13),
        ylim = c(-11, 0.5))

plot(1:10,type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="n")

plotlet("b_chill1","b_warm",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 4°C",
        ylab = "Advance due to 5° warming",
        data = sumerb,
        xlim = c(-33, -13),
        ylim = c(-14, 0.5))
        
plotlet("b_chill2","b_warm",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 1.5°C",
        ylab = "Advance due to 5° warming",
        data = sumerb,
        xlim = c(-33, -13),
        ylim = c(-14, 0.5))
        
dev.off();system(paste("open", file.path(figpath, "stanbb_3.pdf"), "-a/Applications/Preview.app"))



pdf(file.path(figpath, "stanbb_sitechill.pdf"), width = 8, height = 5)  
par(mfrow=c(1,2))

plotlet("b_chill1","b_site",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 4°C",
        ylab = "Change due to site differences",
        data = sumerb,
        xlim = c(-33, -13),
        ylim = c(-1, 8)
)

plotlet("b_chill2","b_site",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 1.5°C",
        ylab = "Change due to site differences",
        data = sumerb,
        xlim = c(-33, -13),
        ylim = c(-1, 8)
)

dev.off();system(paste("open", file.path(figpath, "stanbb_sitechill.pdf"), "-a/Applications/Preview.app"))

pdf(file.path(figpath, "stanlo_1.pdf"), width = 8, height = 8)  

plotlet("b_warm","b_photo",
        group = treeshrub,
        xlab = "Advance due to 5° warming",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerl,
        xlim = c(-28, -16),
        ylim = c(-16, -11))
        
  
  legend("bottomright",
         pch = 16,
         col = colz,
         legend = c("Shrubs","Trees"),
         inset = 0.02)
  
dev.off();system(paste("open", file.path(figpath, "stanlo_1.pdf"), "-a/Applications/Preview.app"))

### 3-way with chilling

pdf(file.path(figpath, "stanlo_3.pdf"), width = 10, height = 8)  

par(mfrow=c(2,3))

plotlet("b_warm","b_photo",
        group = treeshrub,
        xlab = "Advance due to 5° warming",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerl,
        xlim = c(-28, -16),
        ylim = c(-16, -11))


plotlet("b_chill1","b_photo",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 4°C",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerl,
        xlim = c(-33, -20),
        ylim = c(-16, -11))

plotlet("b_chill2","b_photo",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 1.5°C",
        ylab = "Advance due to 4 hr longer photoperiod",
        data = sumerl,
        xlim = c(-33, -20),
        ylim = c(-16, -11))

plot(1:10,type="n",xlab="",ylab="",xaxt="",yaxt="",bty="n")

plotlet("b_chill1","b_warm",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 4°C",
        ylab = "Advance due to 5° warming",
        data = sumerl,
        xlim = c(-33, -20),
        ylim = c(-28, -16))

plotlet("b_chill2","b_warm",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 1.5°C",
        ylab = "Advance due to 5° warming",
        data = sumerl,
        xlim = c(-33, -20),
        ylim = c(-28, -16))

dev.off();system(paste("open", file.path(figpath, "stanlo_3.pdf"), "-a/Applications/Preview.app"))

pdf(file.path(figpath, "stanlo_sitechill.pdf"), width = 8, height = 5)  

par(mfrow=c(1,2))

plotlet("b_chill1","b_site",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 4°C",
        ylab = "Change due to site differences",
        data = sumerl,
        xlim = c(-33, -20),
        ylim = c(-3, 12)
        )

plotlet("b_chill2","b_site",
        group = treeshrub,
        xlab = "Advance due to 30d chilling at 1.5°C",
        ylab = "Change due to site differences",
        data = sumerl,
        xlim = c(-33, -20),
        ylim = c(-3, 12)
)

dev.off();system(paste("open", file.path(figpath, "stanlo_sitechill.pdf"), "-a/Applications/Preview.app"))



######## Trait plots

# SLA, wood density, and N vs warmeff, photoeff, chilleff, budburst and leafout
tr <- tr[!duplicated(tr$code),]
tr <- tr[!is.na(match(tr$code, unique(dx$sp))),]

identical(as.character(tr$code), as.character(unique(dx$sp)))
identical(as.numeric(as.factor(tr$code)), unique(dx$spn))


warmeff <- sumerb[grep("b_warm\\[", rownames(sumerb)),'mean']
photoeff <- sumerb[grep("b_photo\\[", rownames(sumerb)),'mean']
chill1eff <- sumerb[grep("b_chill1\\[", rownames(sumerb)),'mean']
chill2eff <- sumerb[grep("b_chill2\\[", rownames(sumerb)),'mean']

plot(tr$sla, warmeff)
plot(tr$wd, warmeff)

plot(tr$sla, warmeff)
plot(tr$wd, warmeff)


#############   #############   #############   #############   #############
#############   More additional plotting        #############   #############
#############   #############   #############   #############   #############

# Alternative for fig 1 with species means plotted behind 

# count up directionality of effects for species

speff.bb <- speff.lo <- vector()
params <- c("b_warm","b_photo","b_chill1","b_chill2","b_site",
            "b_inter_wp","b_inter_ws","b_inter_ps",
            "b_inter_wc1","b_inter_wc2",
            "b_inter_pc1","b_inter_pc2",
            "b_inter_sc1","b_inter_sc2")

pdf(file.path(figpath, "Fig1_bb_lo+sp.pdf"), width = 7, height = 8)

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))
# Upper panel: budburst
plot(seq(-30, #min(meanz[,'mean']*1.1),
         12, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb)), 
     seq(1, 5*nrow(meanzb), length.out = nrow(meanzb)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x = -32, y = 12, bty="n", legend = "a. Budburst", text.font = 2)
rasterImage(bbpng, -28, 1, -22, 8)

axis(2, at = 5*(nrow(meanzb):1), labels = rownames(meanzb), las = 1, cex.axis = 0.8)

# Plot species levels for each predictor
for(i in 1:length(unique(dx$sp))){
  sp.est <- sumerb[!is.na(match(rownames(sumerb), paste(params, "[", i, "]", sep=""))),col4table]
  
  jt <- jitter(0, factor = 50)

  arrows(sp.est[,"mean"]-sp.est[,"sd"],  jt+(5*(nrow(meanzb):1)-1), sp.est[,"mean"]+sp.est[,"sd"],  jt+(5*(nrow(meanzb):1)-1),
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'mean'],
         jt+(5*(nrow(meanzb):1)-1),
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.bb = rbind(speff.bb, t(sp.est[,1]))
    }

arrows(meanzb[,"mean"]-meanzb[,"sd"], (5*(nrow(meanzb):1))+1, meanzb[,"mean"]+meanzb[,"sd"], (5*(nrow(meanzb):1))+1,
       len = 0, col = "black", lwd = 3)

points(meanzb[,'mean'],
       (5*(nrow(meanzb):1))+1,
       pch = 16,
       cex = 2,
       col = "midnightblue")
abline(v = 0, lty = 2)


# leafout

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))

# Upper panel: budburst
plot(seq(-30, #min(meanz[,'mean']*1.1),
         12, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzl)), 
     seq(1, 5*nrow(meanzl), length.out = nrow(meanzl)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x = -32, y = 12, bty="n", legend = "b. Leafout", text.font = 2)
rasterImage(lopng, -28, 1, -22, 8)

axis(2, at = 5*(nrow(meanzl):1), labels = rownames(meanzl), las = 1, cex.axis = 0.8)

# Plot species levels for each predictor
for(i in 1:length(unique(dx$sp))){
  sp.est <- sumerl[!is.na(match(rownames(sumerl), paste(params, "[", i, "]", sep=""))),col4table]
  
  jt <- jitter(0, factor = 50)
  
  arrows(sp.est[,"mean"]-sp.est[,"sd"],  jt+(5*(nrow(meanzl):1)-1), sp.est[,"mean"]+sp.est[,"sd"],  jt+(5*(nrow(meanzl):1)-1),
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'mean'],
         jt+(5*(nrow(meanzl):1)-1),
         pch = 16,
         col = alpha("firebrick", 0.5))
  
  speff.lo = rbind(speff.lo, t(sp.est[,1]))
  
}

arrows(meanzl[,"mean"]-meanzl[,"sd"], (5*(nrow(meanzl):1))+1, meanzl[,"mean"]+meanzl[,"sd"], (5*(nrow(meanzl):1))+1,
       len = 0, col = "black", lwd = 3)

points(meanzl[,'mean'],
       (5*(nrow(meanzl):1))+1,
       pch = 16,
       cex = 2,
       col = "midnightblue")
abline(v = 0, lty = 2)


dev.off();system(paste("open", file.path(figpath, "Fig1_bb_lo+sp.pdf"), "-a /Applications/Preview.app"))

###### How many species have pos/neg of each response?

# count if postive
apply(speff.bb, 2, function(x) length(x[x > 0]))

apply(speff.lo, 2, function(x) length(x[x > 0]))


#### Order of leafout by treatment

mo <- orders <- vector()

treats <- 
  c("CS0", "WS0", "CL0", "WL0", 
  "CS1", "WS1", "CL1", "WL1",
  "CS2", "WS2", "CL2", "WL2")

for(i in treats){ # i = "CL2"
  dxx <- dx[dx$treatcode == i,]
  md = tapply(dxx$lday, dxx$sp, mean, na.rm=T)
  
  raworder <- rank(md, na.last = T)
  raworder[is.na(md)] = NA
  
  orders = rbind(orders, raworder)
  mo = rbind(mo, md)
  }
rownames(orders) = rownames(mo) = treats
colnames(orders) = colnames(mo) = unique(dx$sp)

# within each chill treatment, ranks are very stable
apply(orders[1:4,], 2, mean, na.rm=T)
apply(orders[1:4,], 2, sd, na.rm=T); mean(apply(orders[1:4,], 2, sd, na.rm=T)) # sd of rank order within chill0 = 1.95 ranks

apply(orders[5:8,], 2, mean, na.rm=T)
apply(orders[5:8,], 2, sd, na.rm=T); mean(apply(orders[5:8,], 2, sd, na.rm=T),na.rm=T) # sd of rank order within chill1 = 0.75 ranks

apply(orders[9:12,], 2, mean, na.rm=T)
apply(orders[9:12,], 2, sd, na.rm=T); mean(apply(orders[9:12,], 2, sd, na.rm=T),na.rm=T) # sd of rank order within chill1 = 1.06 ranks


#### Order of budburst by treatment

mo <- orders <- vector()

for(i in treats){ # i = "CL2"
  dxx <- dx[dx$treatcode == i,]
  md = tapply(dxx$bday, dxx$sp, mean, na.rm=T)
  
  raworder <- rank(md, na.last = T)
  raworder[is.na(md)] = NA
  
  orders = rbind(orders, raworder)
  mo = rbind(mo, md)
}
rownames(orders) = rownames(mo) = treats
colnames(orders) = colnames(mo) = unique(dx$sp)

# within each chill treatment, ranks are again very stable
apply(orders[1:4,], 2, mean, na.rm=T)
apply(orders[1:4,], 2, sd, na.rm=T); mean(apply(orders[1:4,], 2, sd, na.rm=T)) # sd of rank order within chill0 = 2.08 ranks

apply(orders[5:8,], 2, mean, na.rm=T)
apply(orders[5:8,], 2, sd, na.rm=T); mean(apply(orders[5:8,], 2, sd, na.rm=T),na.rm=T) # sd of rank order within chill1 = 0.79 ranks

apply(orders[9:12,], 2, mean, na.rm=T)
apply(orders[9:12,], 2, sd, na.rm=T); mean(apply(orders[9:12,], 2, sd, na.rm=T),na.rm=T) # sd of rank order within chill1 = 1.02 ranks
