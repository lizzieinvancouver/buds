## Started 7 May 2017 ##
## By Lizzie ##

## Code from Pheno Budburst analysis.R #
## That is edited to flip the effects (so negatives become positive)
## I started this, but decided it was better to stick with the regular version ## 


#################
# Flipping the negatives and positives to ease interpretation
# There must be an R f(x) for what I do below, but I am in a cabin on Saturna
# So I am just doing it this way.
#################

## budburst, changing just what we use for plotting!
meanzb.alt <- meanzb
meanzb.mod <- meanzb # not necessary, but useful for clarity!
meanzb.alt[,"mean"][meanzb.mod[,"mean"]<0] <- abs(meanzb.mod[,"mean"][meanzb.mod[,"mean"]<0])
meanzb.alt[,"mean"][meanzb.mod[,"mean"]>0] <- (0-meanzb.mod[,"mean"][meanzb.mod[,"mean"]>0])

meanzb.alt[,"25%"][meanzb.mod[,"25%"]<0] <- abs(meanzb.mod[,"25%"][meanzb.mod[,"25%"]<0])
meanzb.alt[,"25%"][meanzb.mod[,"25%"]>0] <- (0-meanzb.mod[,"25%"][meanzb.mod[,"25%"]>0])

meanzb.alt[,"75%"][meanzb.mod[,"75%"]<0] <- abs(meanzb.mod[,"75%"][meanzb.mod[,"75%"]<0])
meanzb.alt[,"75%"][meanzb.mod[,"75%"]>0] <- (0-meanzb.mod[,"75%"][meanzb.mod[,"75%"]>0])

## leafout, changing just what we use for plotting!
meanzl.alt <- meanzl
meanzl.mod <- meanzl # not necessary, but useful for clarity!
meanzl.alt[,"mean"][meanzl.mod[,"mean"]<0] <- abs(meanzl.mod[,"mean"][meanzl.mod[,"mean"]<0])
meanzl.alt[,"mean"][meanzl.mod[,"mean"]>0] <- (0-meanzl.mod[,"mean"][meanzl.mod[,"mean"]>0])

meanzl.alt[,"25%"][meanzl.mod[,"25%"]<0] <- abs(meanzl.mod[,"25%"][meanzl.mod[,"25%"]<0])
meanzl.alt[,"25%"][meanzl.mod[,"25%"]>0] <- (0-meanzl.mod[,"25%"][meanzl.mod[,"25%"]>0])

meanzl.alt[,"75%"][meanzl.mod[,"75%"]<0] <- abs(meanzl.mod[,"75%"][meanzl.mod[,"75%"]<0])
meanzl.alt[,"75%"][meanzl.mod[,"75%"]>0] <- (0-meanzl.mod[,"75%"][meanzl.mod[,"75%"]>0])

################
# Figure 1:
# Stan model effects for bud burst and leaf-out
################

bbpng <- readPNG(file.path(figpath, "Finn_BB.png")) # Illustrations from Finn et al.
lopng <- readPNG(file.path(figpath, "Finn_LO.png"))

pdf(file.path(figpath, "Fig1_bb_lo.pdf"), width = 7, height = 8)
  
  par(mfrow=c(2,1), mar = c(2, 10, 5, 1))
  
  # Upper panel: bud burst
  plot(seq(22, 
           -12,
           length.out = nrow(meanzb.alt)), 
       1:nrow(meanzb.alt),
       type="n",
       xlab = "",
       ylab = "",
       yaxt = "n")
  
  legend(x = 24, y = 6, bty="n", legend = "a. Bud burst", text.font = 2)
  rasterImage(bbpng, 20, 1, 14, 4)
  
  axis(2, at = nrow(meanzb.alt):1, labels = rownames(meanzb.alt), las = 1, cex.axis = 0.8)
  points(meanzb.alt[,'mean'],
         nrow(meanzb.alt):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzb.alt[,"75%"], nrow(meanzb.alt):1, meanzb.alt[,"25%"], nrow(meanzb.alt):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)

  par(mar=c(5, 10, 2, 1))
  # Lower panel: leaf-out
  plot(seq(22, 
           -12, 
           length.out = nrow(meanzl.alt)), 
       1:nrow(meanzl.alt),
       type="n",
       xlab = "Model estimate change in day of phenological event",
       ylab = "",
       yaxt = "n")
  
  legend(x = 24, y = -6, bty="n", legend = "b. Leaf-out", text.font = 2)
  rasterImage(lopng, 20, 1, 13, 4)
  
  axis(2, at = nrow(meanzl.alt):1, labels = rownames(meanzl.alt), las = 1, cex.axis = 0.8)
  points(meanzl.alt[,'mean'],
         nrow(meanzl.alt):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzl.alt[,"75%"], nrow(meanzl.alt):1, meanzl.alt[,"25%"], nrow(meanzl.alt):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)
  
dev.off();#system(paste("open", file.path(figpath, "Fig1_bb_lo.pdf"), "-a /Applications/Preview.app"))
