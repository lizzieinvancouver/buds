# Additional plotting for lizzie 2016-04-04
# also bunch of extra stuff in general
#library(plotrix)#for draw.ellipse
# From loaded data 
groups = treeshrub
  
#exclude non-chill sp from chill parameter plotting. easiest to give a big number (otherwise have to also subset other parameters when plotting pairwise). Also non-site sp from site plotting

gotchill <- tapply(dx$spn, dx$chill2, unique)$'1'
nochill <- unique(dx$spn)[is.na(match(unique(dx$spn), gotchill))]

gotsite <- tapply(dx$spn, dx$site, unique)$'2' # 19 species at St. hipp
nosite <- unique(dx$spn)[is.na(match(unique(dx$spn), gotsite))]
#unique(dx$sp)[is.na(match(unique(dx$spn), gotsite))] # just checking: these are indeed the sp only at HF.

sumerb <- summary(doym.b)$summary

sumerb[!is.na(match(rownames(sumerb), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_chill2[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_wc1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_wc2[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_pc1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_pc2[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc2[", nochill, "]", sep=""))),] = NA


sumerb[!is.na(match(rownames(sumerb), paste("b_site[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_ws[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_ps[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc1[", nosite, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_inter_sc2[", nosite, "]", sep=""))),] = NA

sumerl <- summary(doym.l)$summary

sumerl[!is.na(match(rownames(sumerl), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill2[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_wc1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_wc2[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_pc1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_pc2[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_sc1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_sc2[", nochill, "]", sep=""))),] = NA

sumerl[!is.na(match(rownames(sumerl), paste("b_site[", nosite, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_ws[", nosite, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_ps[", nosite, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_sc1[", nosite, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_inter_sc2[", nosite, "]", sep=""))),] = NA

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

dev.off();#system(paste("open", file.path(figpath, "stanbb_1.pdf"), "-a/Applications/Preview.app"))

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
        
dev.off();#system(paste("open", file.path(figpath, "stanbb_3.pdf"), "-a/Applications/Preview.app"))



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

dev.off();#system(paste("open", file.path(figpath, "stanbb_sitechill.pdf"), "-a/Applications/Preview.app"))

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
  
dev.off();#system(paste("open", file.path(figpath, "stanlo_1.pdf"), "-a/Applications/Preview.app"))

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

plot(1:10,type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="n")

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

dev.off();#system(paste("open", file.path(figpath, "stanlo_3.pdf"), "-a/Applications/Preview.app"))

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

dev.off();#system(paste("open", file.path(figpath, "stanlo_sitechill.pdf"), "-a/Applications/Preview.app"))



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

identical(tr$code, as.character(adv$sp))

tr <- data.frame(tr, adv$overall, treeshrub) # 1 = shrub

tr2 <- rbind(tr[c(1:3,19:ncol(tr))],
             tr[c(1:3,19:ncol(tr))],
             tr[c(1:3,19:ncol(tr))],
             tr[c(1:3,19:ncol(tr))])
tr2$effect = gl(4, nrow(tr), labels = c("Temperature","Photoperiod","Chilling 4°","Chilling 1.5°"))
tr2$sensitivity = c(warmeff, photoeff, chill1eff, chill2eff)

pdf(file.path(figpath, "Traits_vs_sensitivity.pdf"), width = 12, height = 5)  

ggplot(tr2, aes(sla, sensitivity, group= effect)) + 
  geom_point(aes(col = adv.overall), cex=3, alpha=0.8) + 
  facet_grid(.~effect) + 
  xlab("Specific leaf area") + ylab("Days earlier") + 
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout")

ggplot(tr2, aes(wd, sensitivity, group= effect)) + 
  geom_point(aes(col = adv.overall), cex=3, alpha=0.8) + 
  facet_grid(.~effect) + 
  xlab("Stem density") + ylab("Days earlier") + 
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout")

ggplot(tr2, aes(X.N, sensitivity, group= effect)) + 
  geom_point(aes(col = adv.overall), cex=3, alpha=0.8) + 
  facet_grid(.~effect) + 
  xlab("Percent nitrogen") + ylab("Days earlier") + 
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout")

dev.off();#system(paste("open", file.path(figpath, "Traits_vs_sensitivity.pdf"), "-a/Applications/Preview.app"))

#############

# plot of traits vs traits, for trees and shrubs
tr2$treeshrub[tr2$treeshrub == 1] = "Shrubs"
tr2$treeshrub[tr2$treeshrub == 2] = "Trees"

pdf(file.path(figpath, "Tree_shrub_traits.pdf"), width = 8, height = 4)
ggplot(tr2, aes(sla, wd)) + 
  geom_point(aes(col = adv.overall), cex=3, alpha=0.8) + 
  facet_grid(.~treeshrub) + 
  xlab("Specific leaf area") + ylab("Stem density") + 
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout")

ggplot(tr2, aes(sla, X.N)) + 
  geom_point(aes(col = adv.overall), cex=3, alpha=0.8) + 
  facet_grid(.~treeshrub) + 
  xlab("Specific leaf area") + ylab("Percent nitrogen") + 
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout")


ggplot(tr2, aes(wd, X.N)) + 
  geom_point(aes(col = adv.overall), cex=3, alpha=0.8) + 
  facet_grid(.~treeshrub) + 
  xlab("Stem density") + ylab("Percent nitrogen") + 
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout")

dev.off();#system(paste("open", file.path(figpath, "Tree_shrub_traits.pdf"), "-a/Applications/Preview.app"))

#############   #############   #############   #############   #############
#############   More additional plotting        #############   #############
#############   #############   #############   #############   #############

# BB to LO distance, by treatment, colored by overall leafout

lod <- bbd <- sp <- treat <- vector()
  treats <- 
    c("CS0", "WS0", "CL0", "WL0", 
      "CS1", "WS1", "CL1", "WL1",
      "CS2", "WS2", "CL2", "WL2")
  
  for(i in treats){ # i = "CL2"
    dxx <- dx[dx$treatcode == i,]
    md = tapply(dxx$lday, dxx$sp, mean, na.rm=T)
    mdbb = tapply(dxx$bday, dxx$sp, mean, na.rm=T)
    
    lod <- c(lod, md)
    bbd <- c(bbd, mdbb)
    sp <- c(sp, as.character(unique(dx$sp)))
    treat <- c(treat, rep(i, length(unique(dx$sp))))
    }

  lobbdist <- data.frame(sp, treat, lod, bbd)
  lobbdist$dist <- lobbdist$lod - lobbdist$bbd
  lobbdist$overall <- adv[match(lobbdist$sp, adv$sp), "overall"]
  lobbdist$overallb <- adv[match(lobbdist$sp, adv$sp), "overallb"]

pdf(file.path(figpath, "LOBB_dist.pdf"), width = 12, height = 15)
  
ggplot(lobbdist, aes(overall, dist)) + 
  geom_point(aes(col = lod), cex=3, alpha=0.8) + 
  facet_wrap(~treat, ncol = 3) + 
  ylab("Distance between leafout and budburst") +
  xlab("Overall leafout day") +
  geom_smooth(method = "lm", se = F) +
  theme_bw() + scale_color_continuous(low = "dodgerblue3", high = "goldenrod1", name="Overall day \nof leafout") 

dev.off()#;system(paste("open", file.path(figpath, "LOBB_dist.pdf"), "-a/Applications/Preview.app"))

######## ######## ######## ######## ######## ######## ######## ######## 
######## Intercepts of species by leafout and bb day
######## ######## ######## ######## ######## ######## ######## ######## 

adv$bb_intercept <-  sumerb[grep("a_sp", rownames(sumerb)), "mean"]
adv$lo_intercept <-  sumerl[grep("a_sp", rownames(sumerl)), "mean"]

# yes, the intercepts for each model are very closely related to the overall day of event. Not 1:1 though.
pdf(file.path(figpath, "Intercepts_overall.pdf"), width = 6, height = 6)

ggplot(adv, aes(overall, lo_intercept)) +
  geom_point(aes(size = n), alpha = 0.8) +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

ggplot(adv, aes(overallb, bb_intercept)) + 
  geom_point(aes(size = n), alpha = 0.8) +
  geom_smooth(method = "lm", se=F) +
  theme_bw()

dev.off()#;system(paste("open", file.path(figpath, "Intercepts_overall.pdf"), "-a/Applications/Preview.app"))


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
plot(seq(-33, #min(meanz[,'mean']*1.1),
         13, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb)), 
     seq(1, 5*nrow(meanzb), length.out = nrow(meanzb)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x = -32, y = 12, bty="n", legend = "a. Budburst", text.font = 2)
rasterImage(bbpng, -28, 1, -20, 8)

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


# Lower panel: leafout

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))

plot(seq(-33, #min(meanz[,'mean']*1.1),
         13, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzl)), 
     seq(1, 5*nrow(meanzl), length.out = nrow(meanzl)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x = -32, y = 12, bty="n", legend = "b. Leafout", text.font = 2)
rasterImage(lopng, -29, 1, -20, 8)

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


dev.off();#system(paste("open", file.path(figpath, "Fig1_bb_lo+sp.pdf"), "-a /Applications/Preview.app"))

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


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Adding any bonus stuff down here, for purposes of Sweaving

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>


### O'Keefe work -- see okeefe > order_harv for analysis

# BB and LO by treatment, by species
spxtreat <- aggregate(cbind(lday, bday) ~ sp + warm + photo + chill1 + chill2, data = dx, FUN = mean)

#### Order of leafout by treatment
lo <- bo <- vector()

spxtreat$treats = with(spxtreat, paste(warm, photo, chill1, chill2))

for(i in unique(spxtreat$treats)){ # i = spxtreat$treats[1]
  dxx <- spxtreat[spxtreat$treats == i,]
  lox <- rank(dxx$lday, na.last = T)
  box <- rank(dxx$bday, na.last = T)
  lo = c(lo, lox)
  bo = c(bo, box)
}

spxtreat <- data.frame(spxtreat, leafout.order = lo, budburst.order = bo)
write.csv(spxtreat, file="Species x Treat BB LO for OKeefe.csv",row.names=F)


### Correlations between main effects and interactions
par(mfrow=c(3,2))
plotlet("b_warm", "b_inter_wp", data=sumerb, xlim = c(-20, 5), ylim = c(-2, 0))
plotlet("b_photo", "b_inter_wp", data=sumerb, xlim = c(-10, 0), ylim = c(-2, 0))

plotlet("b_warm", "b_inter_wc1", data=sumerb, xlim = c(-20, 5), ylim = c(5, 12))
plotlet("b_chill1", "b_inter_wc1", data=sumerb, xlim =c(-35, -10), ylim = c(5, 12))

plotlet("b_photo", "b_inter_pc1", data=sumerb, xlim = c(-10, 0), ylim = c(-1, 1))
plotlet("b_chill1", "b_inter_pc1", data=sumerb, xlim =c(-35, -10), ylim = c(-1, 1))


plotlet("b_warm", "b_inter_wp", data=sumerl, xlim = c(-30, -15), ylim = c(2, 5))
plotlet("b_photo", "b_inter_wp", data=sumerl, xlim = c(-15, -10), ylim = c(2, 5))

plotlet("b_warm", "b_inter_wc1", data=sumerl, xlim = c(-30, -15), ylim = c(5, 12))
plotlet("b_chill1", "b_inter_wc1", data=sumerl, xlim =c(-35, -10), ylim = c(5, 12))

plotlet("b_photo", "b_inter_pc1", data=sumerl, xlim = c(-15, -10), ylim = c(0, 1.5))
plotlet("b_chill1", "b_inter_pc1", data=sumerl, xlim =c(-35, -10), ylim = c(0, 1.5))

## understanding the interactions
# warm x photo
tapply(dx$lday, list(dx$warm, dx$photo), mean, na.rm=T)
# warm as rows, photo as columns.
diff(tapply(dx$lday, list(dx$warm, dx$photo), mean, na.rm=T))
# 13 days earlier effect of warming under short days, only 10 days earlier under long days. So a positive interaction (delay of leafout under the combination of both factors).

tapply(dx$lday, list(dx$warm, dx$chill), mean, na.rm=T)
diff(tapply(dx$lday, list(dx$warm, dx$chill), mean, na.rm=T))
# effect of warming was largest without additional chilling. When additional chilling was present, effect of warming was much more muted (replacement of warming effect by chilling effect).
tapply(dx$lday, list(dx$warm, dx$chill), mean, na.rm=T)
diff(tapply(dx$lday, list(dx$warm, dx$chill), mean, na.rm=T))



############# chilling plot
# Only look at species with variation in chilling treatment
chillsp = aggregate(chill ~ sp, FUN = function(x) length(x)>100, data = dx)
chillsp = chillsp[chillsp$chill==TRUE,"sp"]
dx.chill <-  dx[!is.na(match(dx$sp, chillsp)),] # now only have chilled species
dx.chill <- dx.chill[!is.na(dx.chill$lday),]

cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)

# advanced by chill1, delayed by chill2: ilemuc, popgra. delayed: acesac, faggra
densplot <- function(sp, response = 'lday', ylim = 0.1){
  
  #	cols = hcl(h = seq(120, by=360 / 3, length = 3), l = 75, alpha = 0.7) 
  cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)
  df0 <- density(dx.chill[dx.chill$sp == sp & dx.chill$chill == 1,response], adjust = 2.2)
  df1 <- density(dx.chill[dx.chill$sp == sp & dx.chill$chill == 2,response], adjust = 2.2)
  df2 <- density(dx.chill[dx.chill$sp == sp & dx.chill$chill == 3,response], adjust = 2.2)
  
  plot(
    seq(0, ylim, length.out = 100) ~ seq(0, 90, length.out = 100),
    type = "n", xlab = "", ylab ="", yaxt="n", xaxs ="r", bty = "n"
  )
  polygon(df0$x, df0$y, col = cols[1], border = NA)
  polygon(df1$x, df1$y,col = cols[2], border = NA)
  polygon(df2$x, df2$y, col = cols[3], border = NA)	
  
  abline(v = mean(dx.chill[dx.chill$sp == sp & dx.chill$chill == 1,response]), col = cols[1], lty = 3, lwd = 2)
  abline(v = mean(dx.chill[dx.chill$sp == sp & dx.chill$chill == 2,response]), col = cols[2], lty = 3, lwd = 2)	
  abline(v = mean(dx.chill[dx.chill$sp == sp & dx.chill$chill == 3,response]), col = cols[3], lty = 3, lwd = 2)	
  
  #axis(1, at = c(0, 0.5, 1), labels = c(1, 0.5, 0), cex.axis = 0.7)
}

pdf(file.path(figpath, "Chillplot.pdf"), width = 7, height = 8)

par(mfrow =c(2, 2), mar = c(5, 2, 2, 1), xpd = F)

densplot("POPGRA", ylim = 0.05); title(xlab = "Days to leafout")
par(xpd=T); mtext("Populus grandidentata", 3, at = 100, font = 3); par(xpd=F)
legend("topleft", fill=cols, legend = c("0", "4°C", "1.5°C"), title = "Chilling treatment", bg="white")

densplot("POPGRA", 'bday'); title(xlab = "Days to budburst")

densplot("FAGGRA", ylim = 0.05); title(xlab = "Days to leafout")
par(xpd=T); mtext("Fagus grandifolia", 3, at = 100, font = 3); par(xpd=F)

densplot("FAGGRA", 'bday', ylim = 0.05); title(xlab = "Days to budburst")
dev.off();#system(paste("open", file.path(figpath, "Chillplot.pdf"), "-a /Applications/Preview.app"))

on.exit(setwd("~/Documents/git/buds/docs/ms")) # for manuscript, so it can find the .tex file

