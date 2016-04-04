# Additional plotting for lizzie 2016-04-04
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

pdf(file.path(figpath, "stanbb.pdf"), width = 8, height = 8)  

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

dev.off();system(paste("open", file.path(figpath, "stanbb.pdf"), "-a/Applications/Preview.app"))

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


pdf(file.path(figpath, "stanlo.pdf"), width = 8, height = 8)  

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
  
dev.off();system(paste("open", file.path(figpath, "stanlo.pdf"), "-a/Applications/Preview.app"))

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

#the basic traits figures of  TRAIT ~ warmeff with shrubs and trees on one figure, color coded and slopes for each group from simple regressions added? I only need woody density and/or SLA.


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

