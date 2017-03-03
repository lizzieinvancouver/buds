forlatex = TRUE # set to FALSE if just trying new figures, TRUE if outputting for final
runstan = TRUE # set to TRUE to actually run stan models. FALSE if loading from previous runs

# Analysis of bud burst experiment 2015. 

library(memisc) # for getSummary 
library(xtable)
library(scales) # for alpha
library(ggplot2)

library(caper) # for pgls
library(png) # readPNG for Fig 1

setwd("~/Documents/git/projects/treegarden/budexperiments/analyses")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# get latest .Rdata file
# To run from saved stan output (exclude Fake data output)
if(!runstan) {

  realout <- dir()[grep("Stan Output", dir())[is.na(match(grep("Stan Output", dir()), grep("Fake", dir())))]]
  if(!exists("doym.b")) load(sort(realout, T)[1]) # only run if stan output file is not yet in working memory.
  # launch_shinystan(doym.l)
}

if(runstan){ # things needed only if running the stan models
  library(rstan)
  library(shinystan) 
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  source('stan/savestan.R')
}

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> 

(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))

if(forlatex) figpath = "../docs/ms/images" else figpath = "graphs"

# Prep 

dx$spn <- as.numeric(dx$sp)
levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)

# Chill dummy variables
dx$chill1 = ifelse(dx$chill == 2, 1, 0) 
dx$chill2 = ifelse(dx$chill == 3, 1, 0) 

with(dx, table(chill1, chill2)) # all three levels in here

dxb <- dx[!is.na(dx$bday),]
dxl <- dx[!is.na(dx$lday),]

bdaymean <- t(with(dxb, tapply(bday, list(site, sp), mean, na.rm=T)))
ldaymean <- t(with(dxl, tapply(lday, list(site, sp), mean, na.rm=T)))

leafoutdays <- data.frame(bdaymean, ldaymean)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Utility function to plot 'random effects' from stan output - used now mostly in Fig 3.
plotlet <- function(x, y, xlab=NULL, ylab=NULL, data, groups = NULL, ...){
  if(is.null(xlab)) xlab = x; if(is.null(ylab)) ylab = y
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
    else {
      colz = c("brown", "blue3")
      ccolz = rep(colz[1], length(groups))
      ccolz[groups == 2] = colz[2]
      col.pch = ccolz
      col.lines = alpha(ccolz, 0.4)
    }
  
  plot(
  data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
  data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
  pch = "+",
  ylab = ylab,
  xlab = xlab,
  col = col.pch,
  ...
  )

  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"]-data[grep(paste(y,"\\[",sep=""), rownames(data)),"se_mean"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"]+data[grep(paste(y,"\\[",sep=""), rownames(data)),"se_mean"],
    length = 0, col = col.lines)
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"]-data[grep(paste(x,"\\[",sep=""), rownames(data)),"se_mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"]+data[grep(paste(x,"\\[",sep=""), rownames(data)),"se_mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"],
    length = 0, col = col.lines)
  
  # match with species names
  text( data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
        data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
        sort(unique(dx$sp)),
        cex = 0.5, 
        pos = 3,
        col = col.pch)
}

# Groups
colz = c("brown", "blue3")

shrubs = c("VIBLAN","RHAFRA","RHOPRI","SPIALB","VACMYR","VIBCAS", "AROMEL","ILEMUC", "KALANG", "LONCAN", "LYOLIG")
trees = c("ACEPEN", "ACERUB", "ACESAC", "ALNINC", "BETALL", "BETLEN", "BETPAP", "CORCOR", "FAGGRA", "FRANIG", "HAMVIR", "NYSSYL", "POPGRA", "PRUPEN", "QUEALB" , "QUERUB", "QUEVEL")

treeshrub = levels(dx$sp)
treeshrub[treeshrub %in% shrubs] = 1
treeshrub[treeshrub %in% trees] = 2
treeshrub = as.numeric(treeshrub)
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Analyses:
# 1. Day of bud burst by all factors, stan 
# 2. Day of leaf out by all factors, stan
# 3. Effects on bud burst/leaf-out day for species: 
#  - Traits (wood density, sla, N, stomata when we have it), 
#  - Phylogeny

# Supplemental analyses
# Correlate order of leaf-out/bud burst in chambers to each other
# Correlate order of leaf-out in chambers to O'Keefe observational data

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# 1. Budburst day. 
if(runstan){
  datalist.b <- list(lday = dxb$bday, # bud burst as response 
                     warm = as.numeric(dxb$warm), 
                     site = as.numeric(dxb$site), 
                     sp = as.numeric(dxb$sp), 
                     photo = as.numeric(dxb$photo), 
                     chill1 = as.numeric(dxb$chill1),
                     chill2 = as.numeric(dxb$chill2),
                     N = nrow(dxb), 
                     n_site = length(unique(dxb$site)), 
                     n_sp = length(unique(dxb$sp))
  )
  
    doym.b <- stan('stan/lday_site_sp_chill_inter.stan', 
                 data = datalist.b, iter = 6006, chains = 4,
                 control = list(adapt_delta = 0.9,
                                max_treedepth = 15)) 
  
}
  sumerb <- summary(doym.b)$summary
  sumerb[grep("mu_", rownames(sumerb)),]
  
  #ssm.b <- as.shinystan(doym.b)
  # launch_shinystan(ssm.b) 
  #yb = dxb$bday # for shinystan posterior checks

# plot effects
col4table <- c("mean","sd","25%","50%","75%","Rhat")

# manually to get right order
mu_params <- c("mu_b_warm","mu_b_photo","mu_b_chill1","mu_b_chill2","mu_b_site",
               "mu_b_inter_wp",
               "mu_b_inter_wc1","mu_b_inter_wc2",
               "mu_b_inter_pc1","mu_b_inter_pc2",
               "mu_b_inter_ws","mu_b_inter_ps",
               "mu_b_inter_sc1","mu_b_inter_sc2")

meanzb <- sumerb[mu_params,col4table]

rownames(meanzb) = c("Temperature",
                    "Photoperiod",
                    "Chilling 4°",
                    "Chilling 1.5°C",
                    "Site",
                    "Temperature x Photoperiod",
                    "Temperature x Chilling 4°C",
                    "Temperature x Chilling 1.5°C",
                    "Photoperiod x Chilling 4°C",
                    "Photoperiod x Chilling 1.5°C",
                    "Temperature x Site",
                    "Photoperiod x Site",
                    "Site x Chilling 4°C",
                    "Site x Chilling 1.5°C"
                    )

if(runstan){
  datalist.l <- list(lday = dxl$lday, # leaf-out as respose 
                     warm = as.numeric(dxl$warm), 
                     site = as.numeric(dxl$site), 
                     sp = as.numeric(dxl$sp), 
                     photo = as.numeric(dxl$photo), 
                     chill1 = as.numeric(dxl$chill1),
                     chill2 = as.numeric(dxl$chill2),
                     N = nrow(dxl), 
                     n_site = length(unique(dxl$site)), 
                     n_sp = length(unique(dxl$sp))
  )
  
    doym.l <- stan('stan/lday_site_sp_chill_inter.stan',
                data = datalist.l, iter = 6006, chains = 4,
                control = list(adapt_delta = 0.9,
                               max_treedepth = 15)) 
}
sumerl <- summary(doym.l)$summary

# ssm.l <- as.shinystan(doym.l)
# yl = dxl$lday # for shinystan posterior checks
# launch_shinystan(ssm.l) 

meanzl <- sumerl[mu_params,col4table]
rownames(meanzl) = rownames(meanzb)

gotchill <- tapply(dx$spn, dx$chill2, unique)$'1'
nochill <- unique(dx$spn)[is.na(match(unique(dx$spn), gotchill))]
sumerb[!is.na(match(rownames(sumerb), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_chill2[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill2[", nochill, "]", sep=""))),] = NA

# Figure 1: Stan model effects for bud burst and leaf-out

bbpng <- readPNG(file.path(figpath, "Finn_BB.png")) # Illustrations from Finn et al.
lopng <- readPNG(file.path(figpath, "Finn_LO.png"))

pdf(file.path(figpath, "Fig1_bb_lo.pdf"), width = 7, height = 8)
  
  par(mfrow=c(2,1), mar = c(2, 10, 5, 1))
  
  # Upper panel: bud burst
  plot(seq(-30, 
           12,
           length.out = nrow(meanzb)), 
       1:nrow(meanzb),
       type="n",
       xlab = "",
       ylab = "",
       yaxt = "n")
  
  legend(x = -32, y = 6, bty="n", legend = "a. Bud burst", text.font = 2)
  rasterImage(bbpng, -28, 1, -22, 4)
  
  axis(2, at = nrow(meanzb):1, labels = rownames(meanzb), las = 1, cex.axis = 0.8)
  points(meanzb[,'mean'],
         nrow(meanzb):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzb[,"mean"]-meanzb[,"sd"], nrow(meanzb):1, meanzb[,"mean"]+meanzb[,"sd"], nrow(meanzb):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)

  par(mar=c(5, 10, 2, 1))
  # Lower panel: leaf-out
  plot(seq(-30, 
           12, 
           length.out = nrow(meanzl)), 
       1:nrow(meanzl),
       type="n",
       xlab = "Model estimate change in day of phenological event",
       ylab = "",
       yaxt = "n")
  
  legend(x = -32, y = 6, bty="n", legend = "b. Leaf-out", text.font = 2)
  rasterImage(lopng, -28, 1, -21, 4)
  
  axis(2, at = nrow(meanzl):1, labels = rownames(meanzl), las = 1, cex.axis = 0.8)
  points(meanzl[,'mean'],
         nrow(meanzl):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzl[,"mean"]-meanzl[,"sd"], nrow(meanzl):1, meanzl[,"mean"]+meanzl[,"sd"], nrow(meanzl):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)
  
dev.off();#system(paste("open", file.path(figpath, "Fig1_bb_lo.pdf"), "-a /Applications/Preview.app"))
  
# Figure 2: random effects. Photo x warm and chill1 x warm for bb and lo as 4 panels

pdf(file.path(figpath, "Fig2_4panel.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = T),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Bud burst \n\n Advance due to 5° warming", font = 2, srt = 90)

plotlet( "b_photo", "b_warm",
         #  ylab = "Advance due to 5° warming", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-14, 0.5),
         xlim = c(-11, 0.5),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.05, legend = "A.", text.font=2)

legend("bottomright",
       pch = 16,
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet("b_chill1", "b_warm", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-14, 0.5),
        xlim = c(-33, -13),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -10, by = -5), labels = F)
legend("topleft", bty = "n", inset = 0.05, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leaf-out \n\n Advance due to 5° warming", font = 2, srt = 90)

plotlet("b_photo", "b_warm", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-28, -16),
        xlim = c(-16, -11),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.05, legend = "C.", text.font=2)
plotlet("b_chill1", "b_warm", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-28, -16),
        xlim = c(-33, -20),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(-16, -28, by = -2), labels = F)
legend("topleft", bty = "n", inset = 0.05, legend = "D.", text.font=2)
plotblank()

plotblank()
text(6,5, "Advance due to 4 hr longer photoperiod", font = 2, pos = 3)

plotblank()
text(6,5, "Advance due to 30d 4° chilling", font = 2, pos = 3)

dev.off();#system(paste("open", file.path(figpath, "Fig2_4panel.pdf"), "-a /Applications/Preview.app"))

if(runstan) { savestan("Inter") }

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# 2. Species-specific responses. Not using trait data here, just leaf-out and budburst vs warming and photoperiod. 

dlo <- summary(doym.l)$summary
dlo[!is.na(match(rownames(dlo), paste("b_chill1[", nochill, "]", sep=""))),] = 99
dlo[!is.na(match(rownames(dlo), paste("b_chill2[", nochill, "]", sep=""))),] = 99



# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Phylogeny
# 
# phsp <- ph$tip.label
# phspcode <- unlist(lapply(strsplit(phsp, "_"), function(x) toupper(paste(substr(x[[1]],1,3), substr(x[[2]],1,3), sep=""))))
# 
# ph$tip.label = phspcode
# 
# ph$node.label = NULL # otherwise give duplicated names error, because of multiple "" in node labels.
# 
# sig <- comparative.data(ph, dxt2, names.col = "sp")
# 
# lo.signal.warm <- pgls(lo ~ warm, sig, lambda = 'ML')
# lo.signal.photo <- pgls(lo ~ photoeff, sig, lambda = 'ML')
# lo.signal.chill1 <- pgls(lo ~ chill1eff, sig, lambda = 'ML')
# lo.signal.chill2 <- pgls(lo ~ chill2eff, sig, lambda = 'ML')
# 
# bb.signal.warm <- pgls(bb ~ warmeff, sig, lambda = 'ML')
# bb.signal.photo <- pgls(bb ~ photoeff, sig, lambda = 'ML')
# bb.signal.chill1 <- pgls(bb ~ chill1eff, sig, lambda = 'ML')
# bb.signal.chill2 <- pgls(bb ~ chill2eff, sig, lambda = 'ML')
# 
# 
# signaldat <- data.frame(
#   rbind(summary(bb.signal.warm)$param["lambda"], 
#         summary(bb.signal.photo)$param["lambda"],
#         summary(bb.signal.chill1)$param["lambda"],
#         summary(bb.signal.chill2)$param["lambda"],
#         
#         summary(lo.signal.warm)$param["lambda"], 
#         summary(lo.signal.photo)$param["lambda"],
#         summary(lo.signal.chill1)$param["lambda"],
#         summary(lo.signal.chill2)$param["lambda"]
# ))
#         
# signaldat$var = paste(
#   rep(c("Bud burst","Leaf-out"), each = 4), 
#   rep(c("Temperature", "Photoperiod", "Chilling 4°", "Chilling 1.5°"), 4), 
#   sep = " - ")
# 
# 
# phylosigtable <- xtable(data.frame(Relationship = signaldat[,"var"],Lambda = signaldat[,"lambda"]), digits = 3,
#                         caption = "Phylogenetic signal in timing of bud burst and leaf-out and species specific traits, as estimated in the caper package with simultaneous fitting of lambda.  Pore anatomy (ring- versus diffuse-porous species) was highly clustered phylogenetically, but no other trait examined demonstrated significant phylogenetic signal")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Plot actual change in leaf-out by species, for Figure 1
lday.agg <- aggregate(dx$lday, by=list(sp=dx$sp,
                                       warm=dx$warm, photo=dx$photo
                                       #,chill=dx$chill, site=dx$site,  treatcode=dx$treatcode, 
                                       ), FUN = mean, na.rm=T)

lday.se <- aggregate(dx$lday, list(sp=dx$sp, 
                                   warm=dx$warm, photo=dx$photo
                                   #,chill=dx$chill, site=dx$site, treatcode=dx$treatcode, 
                                   ), function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)])))
lday.agg$se = lday.se$x

# Advance due to each factor
wa = la = ca = oa = nn = vector()
wab = lab = cab = oab = vector()

for(i in unique(dx$sp)){ # i="ACEPEN"
  dxx <- dx[dx$sp == i,]
  
  nn <- c(nn, nrow(dxx[dxx$nl==1,]))
  
  overallm = mean(dxx[dxx$warm == 1 & dxx$photo == 1 & dxx$chill == 1, "lday"], na.rm=T)
  overallmb = mean(dxx[dxx$warm == 1 & dxx$photo == 1 & dxx$chill == 1, "bday"], na.rm=T)
  
  # mean across all cool
  cm <- mean(dxx[dxx$warm == 1,'lday'], na.rm=T)
  cmb <- mean(dxx[dxx$warm == 1,'bday'], na.rm=T)
  
  # advance from warming
  wm <- mean(dxx[dxx$warm == 2, 'lday'], na.rm=T)
  wmb <- mean(dxx[dxx$warm == 2, 'bday'], na.rm=T)
  warmadv = wm - cm    
  warmadvb = wmb - cmb
  
  # mean across all short
  sm <- mean(dxx[dxx$photo == 1,'lday'], na.rm=T)
  smb <- mean(dxx[dxx$photo == 1,'bday'], na.rm=T)
  
  # mean across long
  lm <- mean(dxx[dxx$photo == 2, 'lday'], na.rm=T)
  lmb <- mean(dxx[dxx$photo == 2, 'bday'], na.rm=T)
  
  # advance from photo
  longadv = lm - sm   
  longadvb = lmb - smb   
  
  # mean across chill1 (no additional chill)
  cm <- mean(dxx[dxx$chill == 1,'lday'], na.rm=T)
  cmb <- mean(dxx[dxx$chill == 1,'bday'], na.rm=T)

  # mean across chill2 (chill 4deg)
  wm <- mean(dxx[dxx$chill == 2, 'lday'], na.rm=T)
  wmb <- mean(dxx[dxx$chill == 2, 'bday'], na.rm=T)
  chilladv = wm - cm    
  chilladvb = wmb - cmb
  
  # advance from chill
  
  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm); ca = c(ca, chilladv)
  wab = c(wab, warmadvb); lab =c(lab, longadvb); oab=c(oab, overallmb); cab = c(cab, chilladvb)
  }
adv=data.frame(sp=unique(dx$sp), warm=wa, photo=la, overall=oa, n=nn, chill = ca,
               warmb=wab, photob=lab, overallb=oab, chillb = cab)

# Color classes for early - mid - late fl
#hist(adv$overall)
oa.col <- cut(adv$overall, 3)
levels(oa.col) <- oc <- alpha(c("blue","purple","red"), 0.8)
oa.col = as.character(oa.col)

pdf(file.path(figpath, "Advplot2.pdf"), width = 8, height = 9) # Adv plot is without varying cex by sample size
plot(adv$photo, adv$warm, 
     ylim = c(-30, -2),
     xlim=c(-20,-2),
     xlab = "Advance in leaf-out due to 4h longer photoperiod",
     ylab = "Advance in leaf-out due to 5 °C warmer temperature",
     pch = 16, 
     col = oa.col,
     cex = adv$n/12#log(adv$n)
     )

text(adv$photo, adv$warm,
     labels = adv$sp, cex = 0.7, adj = 0.5, #pos = 3,
     col = alpha('grey20', 0.9))

legend(x = -20, y = -3, bty = "n", legend = c("Early", "Intermediate", "Late"), title = "Leaf-out period", col = oc, pch = 16, 
       pt.cex = 2,
       y.intersp = 1.5,
       x.intersp = 1.5)
legend(x = -16, y = -3, bty = "n", legend = c(25, 75), 
       title = "N leaf-out samples", col = "black", pch = 1, pt.cex = c(25, 75)/12,
       y.intersp = 2,
       x.intersp = 2
)

dev.off();#system(paste("open", file.path(figpath, "Advplot2.pdf"), "-a /Applications/Preview.app"))

## Correlations between main effects and lo/bb
# warm, photo, chill1, chill2 vs. day of lo and day of bb

#bb, warm
bwarm <- sumerb[grep(paste("b_warm","\\[",sep=""), rownames(sumerb)),1]
bphoto <- sumerb[grep(paste("b_photo","\\[",sep=""), rownames(sumerb)),1]
bchill1 <- sumerb[grep(paste("b_chill1","\\[",sep=""), rownames(sumerb)),1]

lwarm <- sumerl[grep(paste("b_warm","\\[",sep=""), rownames(sumerl)),1]
lphoto <- sumerl[grep(paste("b_photo","\\[",sep=""), rownames(sumerl)),1]
lchill1 <- sumerl[grep(paste("b_chill1","\\[",sep=""), rownames(sumerl)),1]

pdf(file.path(figpath, "Sens_vs_day.pdf"), width = 9, height = 7)

par(mfrow=c(2,3))
plot(adv$overallb, bwarm, ylab = "Warming sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of bud burst")
legend("top", legend="Bud burst", text.font=2, inset = 0.05, bty ="n", cex = 2)
plot(adv$overallb, bphoto, ylab = "Photoperiod sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of bud burst")
plot(adv$overallb, bchill1, #ylim = c(-30, -10), 
     ylab = "Chilling sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of bud burst")

plot(adv$overall, lwarm, ylab = "Warming sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leaf-out")
legend("top", legend="Leaf-out", text.font=2, inset = 0.05, bty ="n", cex = 2)
plot(adv$overall, lphoto, ylab = "Photoperiod sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leaf-out")
plot(adv$overall, lchill1, #  ylim = c(-30, -10), 
     ylab = "Chilling sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leaf-out")

dev.off();#system(paste("open", file.path(figpath, "Sens_vs_day.pdf"), "-a /Applications/Preview.app"))


on.exit(setwd("~/Documents/git/projects/treegarden/budexperiments/docs/ms"))
