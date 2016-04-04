
library(ggplot2)
# Additional trait stuff -- run analysis script first.

# Plot traits by sp x site

dxt <- merge(dx, tr, by.x = "sp", by.y = "code")
#ggpairs(dxt[c("wd","sla","X.N","Pore.anatomy","lday","bday")])

dxt$fg = "shrub"
dxt$fg[!is.na(match(dxt$sp, trees))] = "tree"

dxt$site <- factor(dxt$site, labels = c("HF","SH"))

dxt.agg <- aggregate(dxt[aggcol], by = list(dxt$sp,dxt$site,dxt$fg), FUN = mean, na.rm=T)

dxt.agg2 <- aggregate(dxt[aggcol], by = list(dxt$site,dxt$fg), FUN = mean, na.rm=T)

names(dxt.agg)[1:3] = c("sp","site","fg")


aggcol <- c("lday", "bday","wd","sla","X.N","Pore.anatomy")

dxt$site <- factor(dxt$site, labels = c("HF","SH"))

dxt.agg <- aggregate(dxt[aggcol], by = list(dxt$sp,dxt$site,dxt$fg), FUN = mean, na.rm=T)

dxt.agg2 <- aggregate(dxt[aggcol], by = list(dxt$site,dxt$fg), FUN = mean, na.rm=T)

names(dxt.agg)[1:3] = c("sp","site","fg")
names(dxt.agg2)[1:2] = c("site","fg")

# Analyze effect sizes vs traits
dxt.agg <- dxt.agg[order(dxt.agg$site, dxt.agg$sp),]

dbb <- summary(doym.b)$summary
dbb[!is.na(match(rownames(dbb), paste("b_chill1[", nochill, "]", sep=""))),] = 99
dbb[!is.na(match(rownames(dbb), paste("b_chill2[", nochill, "]", sep=""))),] = 99

warmeff <- dbb[grep("b_warm\\[",rownames(dbb)),"mean"]
photoeff <- dbb[grep("b_photo\\[",rownames(dbb)),"mean"]
chill1eff <- dbb[grep("b_chill1\\[",rownames(dbb)),"mean"]
chill2eff <- dbb[grep("b_chill2\\[",rownames(dbb)),"mean"]

effs <- data.frame(sp = levels(dxt$sp), warmeff, photoeff, chill1eff, chill2eff)

dxt2bb <- merge(dxt.agg, effs, by = "sp", keep.x = T)

dxt2bb <- dxt2bb[!duplicated(dxt2bb$sp),]
dxt2bb <- dxt2bb[!is.na(match(dxt2bb$sp, unique(dx$sp))),]

dlo <- summary(doym.l)$summary
dlo[!is.na(match(rownames(dlo), paste("b_chill1[", nochill, "]", sep=""))),] = 99
dlo[!is.na(match(rownames(dlo), paste("b_chill2[", nochill, "]", sep=""))),] = 99


warmeff <- dlo[grep("b_warm\\[",rownames(dlo)),"mean"]
photoeff <- dlo[grep("b_photo\\[",rownames(dlo)),"mean"]
chill1eff <- dlo[grep("b_chill1\\[",rownames(dlo)),"mean"]
chill2eff <- dlo[grep("b_chill2\\[",rownames(dlo)),"mean"]

effs <- data.frame(sp = levels(dxt$sp), warmeff, photoeff, chill1eff, chill2eff)

dxt2 <- merge(dxt.agg, effs, by = "sp", keep.x = T)

dxt2 <- dxt2[!duplicated(dxt2$sp),]
dxt2 <- dxt2[!is.na(match(dxt2$sp, unique(dx$sp))),]

identical(as.character(dxt2$sp), as.character(unique(dx$sp)))
identical(as.numeric(as.factor(dxt2$sp)), unique(dx$spn))

######## Manually for control
colz = c("brown",#"brown3", 
         "blue3")#"midnightblue")
ccolz = rep(colz[1], length(unique(dxt2$sp)))
ccolz[dxt2$fg == 'tree'] = colz[2]
col.pch = ccolz
col.lines = alpha(ccolz, 0.4)

# <><><><><><><><><><><><><><><><><><><><><><><>
# Budburst
# <><><><><><><><><><><><><><><><><><><><><><><>


pdf(file.path(figpath, "traiteff_bb.pdf"), width = 10, height = 7)  

par(mfrow = c(1, 2))

plot(warmeff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 5°C warming",
     type = "n",
     data = dxt2bb,
     xlim = c(190, 400)
     )

points(
  dxt2bb[,"sla"],
  dxt2bb[,"warmeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(warmeff ~ sla, data = dxt2bb[dxt2bb$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(warmeff ~ sla, data = dxt2bb[dxt2bb$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)
# greatest advance for lower SLA = relatively thicker leaves.
text(dxt2bb[,"sla"],
     dxt2bb[,"warmeff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)

plot(warmeff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 5°C warming",
     type = "n",
     data = dxt2bb,
     xlim = c(0.45, 0.7))

points(
  dxt2bb[,"wd"],
  dxt2bb[,"warmeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(warmeff ~ wd, data = dxt2bb[dxt2bb$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(warmeff ~ wd, data = dxt2bb[dxt2bb$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2bb[,"wd"],
     dxt2bb[,"warmeff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')


# dev.off();system(paste("open", file.path(figpath, "traiteff_warm.pdf"), "-a/Applications/Preview.app"))

### photo
# pdf(file.path(figpath, "traiteff_photo.pdf"), width = 8, height = 6)  

par(mfrow = c(1, 2))
plot(photoeff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 4h longer days",
     type = "n",
     data = dxt2bb,
     xlim = c(190, 400))

points(
  dxt2bb[,"sla"],
  dxt2bb[,"photoeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(photoeff ~ sla, data = dxt2bb[dxt2bb$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(photoeff ~ sla, data = dxt2bb[dxt2bb$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)
# greatest advance for lower SLA = relatively thicker leaves.
text(dxt2bb[,"sla"],
     dxt2bb[,"photoeff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)

plot(photoeff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 4h longer days",
     type = "n",
     data = dxt2bb,
     xlim = c(0.45, 0.7))

points(
  dxt2bb[,"wd"],
  dxt2bb[,"photoeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(photoeff ~ wd, data = dxt2bb[dxt2bb$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(photoeff ~ wd, data = dxt2bb[dxt2bb$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2bb[,"wd"],
     dxt2bb[,"photoeff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')
# dev.off();system(paste("open", file.path(figpath, "traiteff_photo.pdf"), "-a/Applications/Preview.app"))

# chill1


par(mfrow = c(1, 2))
plot(chill1eff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 30d chilling at 4°C",
     type = "n",
     data = dxt2bb,
     ylim = c(-33, -10),
     xlim = c(190, 400))

points(
  dxt2bb[,"sla"],
  dxt2bb[,"chill1eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill1eff ~ sla, data = dxt2bb[dxt2bb$fg == 'shrub' & dxt2bb$chill1eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill1eff ~ sla, data = dxt2bb[dxt2bb$fg == 'tree'& dxt2bb$chill1eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2bb[,"sla"],
     dxt2bb[,"chill1eff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)

plot(chill1eff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 30d chilling at 4°C",
     type = "n",
     data = dxt2bb,
     ylim = c(-33, 10),
     xlim = c(0.45, 0.7))

points(
  dxt2bb[,"wd"],
  dxt2bb[,"chill1eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill1eff ~ wd, data = dxt2bb[dxt2bb$fg == 'shrub'& dxt2bb$chill1eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill1eff ~ wd, data = dxt2bb[dxt2bb$fg == 'tree'& dxt2bb$chill1eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2bb[,"wd"],
     dxt2bb[,"chill1eff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

### chill2 

par(mfrow = c(1, 2))
plot(chill2eff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 30d chilling at 1.5°C",
     type = "n",
     data = dxt2bb,
     ylim = c(-33, -10),
     xlim = c(190, 400))

points(
  dxt2bb[,"sla"],
  dxt2bb[,"chill2eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill2eff ~ sla, data = dxt2bb[dxt2bb$fg == 'shrub' & dxt2bb$chill2eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill2eff ~ sla, data = dxt2bb[dxt2bb$fg == 'tree'& dxt2bb$chill2eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2bb[,"sla"],
     dxt2bb[,"chill2eff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)

plot(chill2eff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 30d chilling at 1.5°C",
     type = "n",
     data = dxt2bb,
     ylim = c(-33, -10),
     xlim = c(0.45, 0.7))

points(
  dxt2bb[,"wd"],
  dxt2bb[,"chill2eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill2eff ~ wd, data = dxt2bb[dxt2bb$fg == 'shrub'& dxt2bb$chill2eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill2eff ~ wd, data = dxt2bb[dxt2bb$fg == 'tree'& dxt2bb$chill2eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2bb[,"wd"],
     dxt2bb[,"chill2eff"],
     dxt2bb$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

dev.off();system(paste("open", file.path(figpath, "traiteff_bb.pdf"), "-a/Applications/Preview.app"))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Leafout
# <><><><><><><><><><><><><><><><><><><><><><><><><><><>

pdf(file.path(figpath, "traiteff_lo.pdf"), width = 10, height = 7)  

par(mfrow = c(1, 2))

plot(warmeff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 5°C warming",
     type = "n",
     data = dxt2,
     xlim = c(190, 400))
     
points(
  dxt2[,"sla"],
  dxt2[,"warmeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(warmeff ~ sla, data = dxt2[dxt2$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(warmeff ~ sla, data = dxt2[dxt2$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)
# greatest advance for lower SLA = relatively thicker leaves.
text(dxt2[,"sla"],
     dxt2[,"warmeff"],
     dxt2$sp,
      cex = 0.5, 
      pos = 3,
      col = col.pch)

plot(warmeff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 5°C warming",
     type = "n",
     data = dxt2,
     xlim = c(0.45, 0.7))

points(
  dxt2[,"wd"],
  dxt2[,"warmeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(warmeff ~ wd, data = dxt2[dxt2$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(warmeff ~ wd, data = dxt2[dxt2$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2[,"wd"],
     dxt2[,"warmeff"],
     dxt2$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')


# dev.off();system(paste("open", file.path(figpath, "traiteff_warm.pdf"), "-a/Applications/Preview.app"))

### photo
# pdf(file.path(figpath, "traiteff_photo.pdf"), width = 8, height = 6)  

par(mfrow = c(1, 2))
plot(photoeff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 4h longer days",
     type = "n",
     data = dxt2,
     xlim = c(190, 400))

points(
  dxt2[,"sla"],
  dxt2[,"photoeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(photoeff ~ sla, data = dxt2[dxt2$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(photoeff ~ sla, data = dxt2[dxt2$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)
# greatest advance for lower SLA = relatively thicker leaves.
text(dxt2[,"sla"],
     dxt2[,"photoeff"],
     dxt2$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)

plot(photoeff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 4h longer days",
     type = "n",
     data = dxt2,
     xlim = c(0.45, 0.7))

points(
  dxt2[,"wd"],
  dxt2[,"photoeff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(photoeff ~ wd, data = dxt2[dxt2$fg == 'shrub',]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(photoeff ~ wd, data = dxt2[dxt2$fg == 'tree',]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2[,"wd"],
     dxt2[,"photoeff"],
     dxt2$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')
# dev.off();system(paste("open", file.path(figpath, "traiteff_photo.pdf"), "-a/Applications/Preview.app"))

# chill1


par(mfrow = c(1, 2))
plot(chill1eff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 30d chilling at 4°C",
     type = "n",
     data = dxt2,
     ylim = c(-33, -20),
     xlim = c(190, 400))

points(
  dxt2[,"sla"],
  dxt2[,"chill1eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill1eff ~ sla, data = dxt2[dxt2$fg == 'shrub' & dxt2$chill1eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill1eff ~ sla, data = dxt2[dxt2$fg == 'tree'& dxt2$chill1eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2[,"sla"],
     dxt2[,"chill1eff"],
     dxt2$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)

plot(chill1eff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 30d chilling at 4°C",
     type = "n",
     data = dxt2,
     ylim = c(-33, -20),
     xlim = c(0.45, 0.7))

points(
  dxt2[,"wd"],
  dxt2[,"chill1eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill1eff ~ wd, data = dxt2[dxt2$fg == 'shrub'& dxt2$chill1eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill1eff ~ wd, data = dxt2[dxt2$fg == 'tree'& dxt2$chill1eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2[,"wd"],
     dxt2[,"chill1eff"],
     dxt2$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

### chill2 

par(mfrow = c(1, 2))
plot(chill2eff ~ sla,
     xlab = "Specific leaf area",
     ylab = "Advance due to 30d chilling at 1.5°C",
     type = "n",
     data = dxt2,
     ylim = c(-33, -20),
     xlim = c(190, 400))

points(
  dxt2[,"sla"],
  dxt2[,"chill2eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill2eff ~ sla, data = dxt2[dxt2$fg == 'shrub' & dxt2$chill2eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill2eff ~ sla, data = dxt2[dxt2$fg == 'tree'& dxt2$chill2eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2[,"sla"],
     dxt2[,"chill2eff"],
     dxt2$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)

plot(chill2eff ~ wd,
     xlab = "Wood density",
     ylab = "Advance due to 30d chilling at 1.5°C",
     type = "n",
     data = dxt2,
     ylim = c(-33, -20),
     xlim = c(0.45, 0.7))

points(
  dxt2[,"wd"],
  dxt2[,"chill2eff"],
  pch = "+",
  col = col.pch)

lms <- coef(lm(chill2eff ~ wd, data = dxt2[dxt2$fg == 'shrub'& dxt2$chill2eff != 99,]))
curve(lms[1]+x*lms[2], add = T, col = colz[1], lwd = 1.5)
lmt <- coef(lm(chill2eff ~ wd, data = dxt2[dxt2$fg == 'tree'& dxt2$chill2eff != 99,]))
curve(lmt[1]+x*lmt[2], add = T, col = colz[2], lwd = 1.5)

text(dxt2[,"wd"],
     dxt2[,"chill2eff"],
     dxt2$sp,
     cex = 0.5, 
     pos = 3,
     col = col.pch)
legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

dev.off();system(paste("open", file.path(figpath, "traiteff_lo.pdf"), "-a/Applications/Preview.app"))


# pdf("Trait Plots.pdf", width = 20, height = 3)
# 
# 
# ggplot(dxt.agg, aes(reorder(sp, wd), wd)) + geom_point() + theme_bw() 
# ggplot(dxt.agg, aes(reorder(sp, sla), sla)) + geom_point() + theme_bw() 
# ggplot(dxt.agg, aes(reorder(sp, X.N), X.N)) + geom_point() + theme_bw() 
# ggplot(dxt.agg, aes(reorder(sp, Pore.anatomy), Pore.anatomy)) + geom_point() + theme_bw() 
# 
# # ggplot(dxt.agg, aes(site, wd)) + geom_point() + facet_grid(.~sp) + theme_bw() 
# # ggplot(dxt.agg, aes(site, sla)) + geom_point() + facet_grid(.~sp)+ theme_bw() + xlim(c(0, 3)) 
# # ggplot(dxt.agg, aes(site, X.N)) + geom_point() + facet_grid(.~sp)+ theme_bw()  + xlim(c(0, 3))
# # ggplot(dxt.agg, aes(site, Pore.anatomy)) + geom_point() + facet_grid(.~sp)+ theme_bw()  + xlim(c(0, 3))
# 
# ggplot(dxt.agg2, aes(site, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()
# ggplot(dxt.agg2, aes(site, sla)) + geom_point() + facet_grid(.~fg)+ theme_bw()
# ggplot(dxt.agg2, aes(site, X.N)) + geom_point() + facet_grid(.~fg)+ theme_bw()
# ggplot(dxt.agg2, aes(site, Pore.anatomy)) + geom_point() + facet_grid(.~fg)+ theme_bw()

# ggplot(dxt2, aes(warmeff, sla, group = fg, color = fg)) + geom_point() + theme_bw()   + geom_smooth(method="lm", se = F)

# ggplot(dxt.agg2, aes(site, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()  + xlim(c(0, 3))
# ggplot(dxt.agg2, aes(site, sla)) + geom_point() + facet_grid(.~fg)+ theme_bw()  + xlim(c(0, 3))
# ggplot(dxt.agg2, aes(site, X.N)) + geom_point() + facet_grid(.~fg)+ theme_bw()  + xlim(c(0, 3))
# ggplot(dxt.agg2, aes(site, Pore.anatomy)) + geom_point() + facet_grid(.~fg)+ theme_bw()  + xlim(c(0, 3))

# dev.off();system("open 'Trait Plots.pdf' -a/Applications/Preview.app")

# ggplot(dxt2, aes(warmeff, X.N)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# ggplot(dxt2, aes(warmeff, Pore.anatomy)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# 
# 
# ggplot(dxt2, aes(photeff, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# ggplot(dxt2, aes(photeff, sla)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# ggplot(dxt2, aes(photeff, X.N)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# ggplot(dxt2, aes(photeff, Pore.anatomy)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# 
# ggplot(dxt2, aes(lday, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# 
# ggplot(dxt2, aes(lday, sla)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# ggplot(dxt2, aes(lday, X.N)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 
# ggplot(dxt2, aes(lday, Pore.anatomy)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")
# 



