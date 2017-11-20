forlatex = TRUE # set to FALSE if just trying new figures, TRUE if outputting for final
runstan = FALSE # set to TRUE to actually run stan models. FALSE if loading from previous runs

# Analysis of bud burst experiment 2015. 

library(memisc) # for getSummary 
library(xtable)
library(scales) # for alpha
library(ggplot2)

library(caper) # for pgls
library(png) # readPNG for Fig 1

setwd("~/Documents/git/projects/treegarden/budexperiments/analyses")
source('source/plotletfx.R') # this one shows 50% credible intervals for the bars

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
colnames(leafoutdays) <- c("BB.HF", "BB.SH", "LO.HF", "LO.SH")
# write.csv(leafoutdays, "output/leafoutdays.csv", row.names=TRUE)

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
# Important: Fixing the 1/2 issue to 0/1 here
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

unique(dxb$site)
dxb$site[dxb$site==1] <- 0
dxb$site[dxb$site==2] <- 1

unique(dxb$warm)
dxb$warm[dxb$warm==1] <- 0
dxb$warm[dxb$warm==2] <- 1

unique(dxb$photo)
dxb$photo[dxb$photo==1] <- 0
dxb$photo[dxb$photo==2] <- 1

unique(dxb$chill1)
unique(dxb$chill2)

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
  
    doym.b <- stan('stan/lday_site_sp_chill_inter_poola_ncp.stan', 
                 data = datalist.b, warmup=4000, iter = 7997, chains = 4,
                 control = list(adapt_delta = 0.9))
                 #               , max_treedepth = 15)) 
  
}


# yb = dxb$bday # for shinystan posterior checks
# launch_shinystan(doym.b) 

sumerb <- summary(doym.b)$summary
sumerb[grep("mu_", rownames(sumerb)),]

# For Simon Joly:
range(sumerb[,"n_eff"])
summary(sumerb[,"n_eff"])
length(sumerb[(sumerb[,"n_eff"])<15988,"n_eff"])
length(sumerb[,"n_eff"])

# Also, I think AROMEL and ALNINC (Zohner spp) are 4-5 (double-check photoperiod effect)
# sumerb[grep("b_photo", rownames(sumerb)),]

# Below: Some pairs plots to check out
# pairs(doym.b, pars = c("mu_b_warm", "sigma_b_warm", "lp__"))
# These are very slow!
# pairs(doym.b, pars = c(names(doym.b)[grep("mu_b_inter", names(doym.b))],
#     names(doym.b)[grep("sigma_b_inter", names(doym.b))]))
# pairs(doym.b, pars = c(names(doym.b)[grep("mu_b_inter", names(doym.b))], "lp__"))
# pairs(doym.b, pars = c(names(doym.b)[grep("sigma_b_inter", names(doym.b))], "lp__"))


# save(doym.b, file="stan/output/lday_site_sp_chill_inter_poola_ncp_doymb.Rda")
# load('stan/lday_site_sp_chill_inter_poola_ncp_doymb.Rda')

# plot effects
col4fig <- c("mean","sd","25%","50%","75%","Rhat")
col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat")

# manually to get right order
mu_params <- c("mu_b_warm","mu_b_photo","mu_b_chill1","mu_b_chill2","mu_b_site",
               "mu_b_inter_wp",
               "mu_b_inter_wc1","mu_b_inter_wc2",
               "mu_b_inter_pc1","mu_b_inter_pc2",
               "mu_b_inter_ws","mu_b_inter_ps",
               "mu_b_inter_sc1","mu_b_inter_sc2")

meanzb <- sumerb[mu_params,col4fig]

rownames(meanzb) = c("Forcing Temperature",
                    "Photoperiod",
                    "Chilling 4°",
                    "Chilling 1.5°C",
                    "Site",
                    "Forcing x Photoperiod",
                    "Forcing x Chilling 4°C",
                    "Forcing x Chilling 1.5°C",
                    "Photoperiod x Chilling 4°C",
                    "Photoperiod x Chilling 1.5°C",
                    "Forcing x Site",
                    "Photoperiod x Site",
                    "Site x Chilling 4°C",
                    "Site x Chilling 1.5°C"
                    )



meanzb.table <- sumerb[mu_params,col4table]
row.names(meanzb.table) <- row.names(meanzb)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# ALERT: Fixing the 1/2 issue to 0/1 here
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

unique(dxl$site)
dxl$site[dxl$site==1] <- 0
dxl$site[dxl$site==2] <- 1

unique(dxl$warm)
dxl$warm[dxl$warm==1] <- 0
dxl$warm[dxl$warm==2] <- 1

unique(dxl$photo)
dxl$photo[dxl$photo==1] <- 0
dxl$photo[dxl$photo==2] <- 1

unique(dxl$chill1)
unique(dxl$chill2)

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
  
    doym.l <- stan('stan/lday_site_sp_chill_inter_poola_ncp.stan',
                data = datalist.l, warmup=4000, iter = 7997, chains = 4,
                control = list(adapt_delta = 0.95))
                #               ,max_treedepth = 15))
}

#yl = dxl$lday # for shinystan posterior checks
# launch_shinystan(doym.l)

sumerl <- summary(doym.l)$summary
sumerl[grep("mu_", rownames(sumerl)),]

# For Simon:
range(sumerl[,"n_eff"])
summary(sumerl[,"n_eff"])
length(sumerl[(sumerl[,"n_eff"])<15988,"n_eff"])
length(sumerl[,"n_eff"])

# save(doym.l, file="stan/output/lday_site_sp_chill_inter_poola_ncp_doyl.Rda")

meanzl <- sumerl[mu_params,col4fig]
meanzl.table <- sumerl[mu_params,col4table]

rownames(meanzl) = rownames(meanzb)
rownames(meanzl.table) = rownames(meanzb.table)


gotchill <- tapply(dx$spn, dx$chill2, unique)$'1'
nochill <- unique(dx$spn)[is.na(match(unique(dx$spn), gotchill))]
sumerb[!is.na(match(rownames(sumerb), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_chill2[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill2[", nochill, "]", sep=""))),] = NA


## Look at posteriors of some ranefs (just one chain, that's the [[1]])
# hist(doym.b@sim$samples[[1]]$`b_photo[13]`, breaks=1000)
# hist(doym.l@sim$samples[[1]]$`b_photo[13]`, breaks=1000)


################
# What cue is biggest?
################
df.meaneffs <- data.frame(bb.warm=sumerb[grep("b_warm", rownames(sumerl)),1],
    bb.photo=sumerb[grep("b_photo", rownames(sumerl)),1],
    bb.chill1=sumerb[grep("b_chill1", rownames(sumerl)),1],
    bb.chill2=sumerb[grep("b_chill2", rownames(sumerl)),1],
    lo.warm=sumerl[grep("b_warm", rownames(sumerl)),1],
    lo.photo=sumerl[grep("b_photo", rownames(sumerl)),1],
    lo.chill1=sumerl[grep("b_chill1", rownames(sumerl)),1],
    lo.chill2=sumerl[grep("b_chill2", rownames(sumerl)),1])

df.meaneffs[which(df.meaneffs$bb.warm > df.meaneffs$bb.photo),] # species 19
unique(dxb$sp)[19] # PRUPEN
df.meaneffs[which(df.meaneffs$lo.warm > df.meaneffs$lo.photo),] # none
df.meaneffs[which(df.meaneffs$bb.chill1 > df.meaneffs$bb.warm),] # none
df.meaneffs[which(df.meaneffs$lo.chill1 > df.meaneffs$lo.warm),] # 7 diff species: ACEPEN, ACERUB, BETALL, BETPAP, FAGGRA, ILEMUC, VIBCAS



################
# Figure 1:
# Stan model effects for bud burst and leaf-out
################

## Want to flip the axes?
## Try source("source/figureflipping.R")
## It does the work for Fig. 1 .... would need more work to do for other figures 

bbpng <- readPNG(file.path(figpath, "Finn_BB.png")) # Illustrations from Finn et al.
lopng <- readPNG(file.path(figpath, "Finn_LO.png"))

pdf(file.path(figpath, "Fig1_bb_lo.pdf"), width = 7, height = 8)
  
  par(mfrow=c(2,1), mar = c(2, 10, 5, 1))
  
  # Upper panel: bud burst
  plot(seq(-22, 
           12,
           length.out = nrow(meanzb)), 
       1:nrow(meanzb),
       type="n",
       xlab = "",
       ylab = "",
       yaxt = "n")
  
  legend(x = -24, y = 6, bty="n", legend = "a. Budburst", text.font = 2)
  rasterImage(bbpng, -20, 1, -16, 4)
  
  axis(2, at = nrow(meanzb):1, labels = rownames(meanzb), las = 1, cex.axis = 0.8)
  points(meanzb[,'mean'],
         nrow(meanzb):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzb[,"75%"], nrow(meanzb):1, meanzb[,"25%"], nrow(meanzb):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)
  # add advance/delay arrows
  par(xpd=NA)
  arrows(1, 15.5, 6, 15.5, len=0.1, col = "black")
  legend(5, 16.5, legend="delay", bty="n", text.font = 1, cex=0.75)
  arrows(-1, 15.5, -6, 15.5, len=0.1, col = "black")
  legend(-12, 16.5, legend="advance", bty="n", text.font = 1, cex=0.75)
  legend(-2, 16.5, legend="0", bty="n", text.font = 1, cex=0.75)
  par(xpd=FALSE)

  par(mar=c(5, 10, 2, 1))
  # Lower panel: leaf-out
  plot(seq(-22, 
           12, 
           length.out = nrow(meanzl)), 
       1:nrow(meanzl),
       type="n",
       xlab = "Model estimate change in day of phenological event",
       ylab = "",
       yaxt = "n")
  
  legend(x = -24, y = 6, bty="n", legend = "b. Leafout", text.font = 2)
  rasterImage(lopng, -20, 1, -14, 4)
  
  axis(2, at = nrow(meanzl):1, labels = rownames(meanzl), las = 1, cex.axis = 0.8)
  points(meanzl[,'mean'],
         nrow(meanzl):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzl[,"75%"], nrow(meanzl):1, meanzl[,"25%"], nrow(meanzl):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)

  # add advance/delay arrows
  par(xpd=NA)
  arrows(1, 15.5, 6, 15.5, len=0.1, col = "black")
  legend(5, 16.5, legend="delay", bty="n", text.font = 1, cex=0.75)
  arrows(-1, 15.5, -6, 15.5, len=0.1, col = "black")
  legend(-12, 16.5, legend="advance", bty="n", text.font = 1, cex=0.75)
  legend(-2, 16.5, legend="0", bty="n", text.font = 1, cex=0.75)
  par(xpd=FALSE)
  
dev.off();#system(paste("open", file.path(figpath, "Fig1_bb_lo.pdf"), "-a /Applications/Preview.app"))

###############
# Figure 2: random effects.
# Photo x warm and chill1 x warm for bb and lo as 4 panels
###############

# Tip for checking figure range needs:
# range(sumerb[grep(paste("b_chill1","\\[",sep=""), rownames(sumerb)),1], na.rm=TRUE)
# Adjust sumerb -> sumerl and b_chill1 to other effects

pdf(file.path(figpath, "Fig2_4panel.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = T),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to 5° warming", font = 2, srt = 90) # \n\n add two line breaks

plotlet( "b_photo", "b_warm",
         #  ylab = "Advance due to 5° warming", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-27, 0.5),
         xlim = c(-16, 0.5),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet("b_chill1", "b_warm", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -8),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due to 5° warming", font = 2, srt = 90)

plotlet("b_photo", "b_warm", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-27, 0.5),
        xlim = c(-16, 0.5),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)
plotlet("b_chill1", "b_warm", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -8),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)
plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 4 hr longer photoperiod", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 4° chilling", font = 2, pos = 3)

dev.off();#system(paste("open", file.path(figpath, "Fig2_4panel.pdf"), "-a /Applications/Preview.app"))


###############
# Figure 2* Supp version: random effects.
# Photo x warm and chill1 x warm for bb and lo as 4 panels
# Zoomed in and without the 50% credible intervals so you can see the species names
###############

pdf(file.path(figpath, "Fig2_4panel_ZoomSupp.pdf"), width = 7, height = 7)

par(mar=rep(1.25,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n", bty="n", xaxt="n", yaxt="n", ylab="", xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to 5° warming", font = 2, srt = 90)

plotlet.old( "b_photo", "b_warm",
         #  ylab = "Advance due to 5° warming", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-16, 0.5),
         xlim = c(-12, 0.5),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet.old("b_chill1", "b_warm", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -8),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = TRUE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due to 5° warming", font = 2, srt = 90) 

plotlet.old("b_photo", "b_warm", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-27, -12),
        xlim = c(-14, -7),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)
plotlet.old("b_chill1", "b_warm", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, -12),
        xlim = c(-28, -8),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = TRUE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)
plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 4 hr longer photoperiod", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 4° chilling", font = 2, pos = 3)

dev.off();


###############
# Figure CHILL2 for Supp (similar to Figure 2): random effects.
# photo x chill2 and chill2 x warm for bb and lo 
# Ailene wanted this (Ailene also wanted chill1 x photo, see below)
###############

pdf(file.path(figpath, "FigChill2_4panel.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to 5° warming", font = 2, srt = 90) # \n\n add two line breaks

plotlet("b_chill2", "b_warm",
         #  ylab = "Advance due to  chilling", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-27, 0.5),
         xlim = c(-28, -4),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet("b_chill2", "b_warm", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due 4 hr longer photoperiod", font = 2, srt = 90)

plotlet("b_chill2", "b_photo", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)

plotlet("b_chill2", "b_photo", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)

plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 30d 1.5° chilling", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 1.5° chilling", font = 2, pos = 3)

dev.off();



###############
# Figure CHILLxPHOTO for Supp (similar to Figure 2): random effects.
# photo x chill2 and photo x chill1 for bb and lo 
# Ailene wanted this also, but I am not including it in Supp until someone else wants it
###############

pdf(file.path(figpath, "FigChillPhoto_4panel.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Leafout \n Change (days) due 4 hr longer photoperiod", font = 2, srt = 90) # \n\n add two line breaks

plotlet("b_chill1", "b_photo",
         #  ylab = "Advance due to  chilling", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-16, 0.5),
         xlim = c(-28, -4),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet("b_chill2", "b_photo", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due 4 hr longer photoperiod", font = 2, srt = 90)

plotlet("b_chill1", "b_photo", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)

plotlet("b_chill2", "b_photo", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)

plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 30d 4° chilling", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 1.5° chilling", font = 2, pos = 3)

dev.off();



# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>

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
#   rep(c("Forcing", "Photoperiod", "Chilling 4°", "Chilling 1.5°"), 4), 
#   sep = " - ")
# 
# 
# phylosigtable <- xtable(data.frame(Relationship = signaldat[,"var"],Lambda = signaldat[,"lambda"]), digits = 3,
#                         caption = "Phylogenetic signal in timing of bud burst and leaf-out and species specific traits, as estimated in the caper package with simultaneous fitting of lambda.  Pore anatomy (ring- versus diffuse-porous species) was highly clustered phylogenetically, but no other trait examined demonstrated significant phylogenetic signal")
# 
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
     xlab = "Advance in leafout due to 4h longer photoperiod",
     ylab = "Advance in leafout due to 5°C warmer temperature",
     pch = 16, 
     col = oa.col,
     cex = adv$n/12#log(adv$n)
     )

text(adv$photo, adv$warm,
     labels = adv$sp, cex = 0.7, adj = 0.5, #pos = 3,
     col = alpha('grey20', 0.9))

legend(x = -20, y = -3, bty = "n", legend = c("Early", "Intermediate", "Late"), title = "Leafout period", col = oc, pch = 16, 
       pt.cex = 2,
       y.intersp = 1.5,
       x.intersp = 1.5)
legend(x = -16, y = -3, bty = "n", legend = c(25, 75), 
       title = "N leafout samples", col = "black", pch = 1, pt.cex = c(25, 75)/12,
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
plot(adv$overallb, bwarm, ylab = "Warming sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of budburst")
# legend("top", legend="Budburst", text.font=2, inset = 0.05, bty ="n", cex = 2)
plot(adv$overallb, bphoto, ylab = "Photoperiod sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of budburst")
plot(adv$overallb, bchill1, #ylim = c(-30, -10), 
     ylab = "Chilling sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of budburst")

plot(adv$overall, lwarm, ylab = "Warming sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leafout")
#legend("top", legend="Leafout", text.font=2, inset = 0.05, bty ="n", cex = 2)
plot(adv$overall, lphoto, ylab = "Photoperiod sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leafout")
plot(adv$overall, lchill1, #  ylim = c(-30, -10), 
     ylab = "Chilling sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leafout")

dev.off();#system(paste("open", file.path(figpath, "Sens_vs_day.pdf"), "-a /Applications/Preview.app"))

##
## Added by Lizzie on 30 July 2017 for ESA talk
## Then updated on 24 October 2017 for paper resubmission
## Correlations between main effects and lo/bb
## But with some bells and whistles (it's a long, ugly bit of code, sorry!)
##

df.lsens <- data.frame(sp=adv$sp, lwarm=lwarm, lphoto=lphoto, lchill1=lchill1, group=treeshrub,
    overall=adv$overall)
df.bsens <- data.frame(sp=adv$sp, bwarm=bwarm, bphoto=bphoto, bchill1=bchill1, group=treeshrub,
    overallb=adv$overallb)

pdf(file.path(figpath, "Sens_vs_day_treeshrub.pdf"), width = 9, height = 7)

# params
cex.pch <- 1.25
cex.text <- 0.5

par(mfrow=c(2,3))
par(xpd=NA)
## budburst
xlimbb <- c(12,75)
plot(bwarm~overallb, ylab = "Change (days) due to 5° warming", pch = 16, cex = cex.pch, col = alpha("firebrick3", 0.6),
    xlab = "Day of budburst", data=subset(df.bsens, group==1), 
    ylim=c(-16,-1), xlim=xlimbb)
points(bwarm~overallb, pch = 16, cex = cex.pch, col = alpha("blue3", 0.6), data=subset(df.bsens, group==2),
    ylim=c(-16,-1), xlim=xlimbb)

df.bsens.gr1 <- subset(df.bsens, group==1)
df.bsens.gr2 <- subset(df.bsens, group==2)

text(df.bsens.gr1$overallb, df.bsens.gr1$bwarm, 
    df.bsens.gr1$sp,
    cex = cex.text, 
    pos = 3,
    col = "firebrick3")

text(df.bsens.gr2$overallb, df.bsens.gr2$bwarm, 
    df.bsens.gr2$sp,
    cex = cex.text, 
    pos = 3,
    col = "blue3")

legend("topright",
       pch = 16,
       col = alpha(c("firebrick3", "blue3"), 0.6),
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white') # bty="n" removes the box

plot(bphoto~overallb, ylab = "Change (days) \n due to 4 hour longer photoperiod", pch = 16, cex = cex.pch, col = alpha("firebrick3", 0.6), xlab = "Day of budburst",
    data=subset(df.bsens, group==1), ylim=c(-12,-1), xlim=xlimbb)
points(bphoto~overallb, pch = 16, cex = cex.pch, col = alpha("blue3", 0.6), data=subset(df.bsens, group==2),
    ylim=c(-12,-1), xlim=xlimbb)

text(df.bsens.gr1$overallb, df.bsens.gr1$bphoto, 
    df.bsens.gr1$sp,
    cex = cex.text, 
    pos = 3,
    col = "firebrick3")

text(df.bsens.gr2$overallb, df.bsens.gr2$bphoto, 
    df.bsens.gr2$sp,
    cex = cex.text, 
    pos = 3,
    col = "blue3")

plot(bchill1~overallb, ylab = "Change (days) due to 30d 4° chilling", pch = 16, cex = cex.pch, col = alpha("firebrick3", 0.6), xlab = "Day of budburst",
    data=subset(df.bsens, group==1), ylim=c(-28,-8), xlim=xlimbb)
points(bchill1~overallb, pch = 16, cex = cex.pch, col = alpha("blue3", 0.6), data=subset(df.bsens, group==2),
    ylim=c(-28,-8), xlim=xlimbb)


text(df.bsens.gr1$overallb, df.bsens.gr1$bchill1, 
    df.bsens.gr1$sp,
    cex = cex.text, 
    pos = 3,
    col = "firebrick3")

text(df.bsens.gr2$overallb, df.bsens.gr2$bchill1, 
    df.bsens.gr2$sp,
    cex = cex.text, 
    pos = 3,
    col = "blue3")

## leafout
plot(lwarm~overall, ylab = "Change (days) due to 5° warming", pch = 16, cex = cex.pch, col = alpha("firebrick3", 0.6),
    xlab = "Day of leafout", data=subset(df.lsens, group==1), 
    ylim=c(-28,-11), xlim=c(20,90))
points(lwarm~overall, pch = 16, cex = cex.pch, col = alpha("blue3", 0.6), data=subset(df.lsens, group==2),
    ylim=c(-28,-11), xlim=c(20,90))

df.lsens.gr1 <- subset(df.lsens, group==1)
df.lsens.gr2 <- subset(df.lsens, group==2)

text(df.lsens.gr1$overall, df.lsens.gr1$lwarm, 
    df.lsens.gr1$sp,
    cex = cex.text, 
    pos = 3,
    col = "firebrick3")

text(df.lsens.gr2$overall, df.lsens.gr2$lwarm, 
    df.lsens.gr2$sp,
    cex = cex.text, 
    pos = 3,
    col = "blue3")

plot(lphoto~overall, ylab = "Change (days) \n due to 4 hour longer photoperiod", pch = 16, cex = cex.pch, col = alpha("firebrick3", 0.6), xlab = "Day of leafout",
    data=subset(df.lsens, group==1), ylim=c(-14,-7), xlim=c(20,90))
points(lphoto~overall, pch = 16, cex = cex.pch, col = alpha("blue3", 0.6), data=subset(df.lsens, group==2),
    ylim=c(-14,-7), xlim=c(20,90))

text(df.lsens.gr1$overall, df.lsens.gr1$lphoto, 
    df.lsens.gr1$sp,
    cex = cex.text, 
    pos = 3,
    col = "firebrick3")

text(df.lsens.gr2$overall, df.lsens.gr2$lphoto, 
    df.lsens.gr2$sp,
    cex = cex.text, 
    pos = 3,
    col = "blue3")

plot(lchill1~overall, ylab = "Change (days) due to 30d 4° chilling", pch = 16, cex = cex.pch, col = alpha("firebrick3", 0.6), xlab = "Day of leafout",
    data=subset(df.lsens, group==1), ylim=c(-28,-9), xlim=c(20,90))
points(lchill1~overall, pch = 16, cex = cex.pch, col = alpha("blue3", 0.6), data=subset(df.lsens, group==2),
    ylim=c(-28,-11), xlim=c(20,90))


text(df.lsens.gr1$overall, df.lsens.gr1$lchill1, 
    df.lsens.gr1$sp,
    cex = cex.text, 
    pos = 3,
    col = "firebrick3")

text(df.lsens.gr2$overall, df.lsens.gr2$lchill1, 
    df.lsens.gr2$sp,
    cex = cex.text, 
    pos = 3,
    col = "blue3")

dev.off();#system(paste("open", file.path(figpath, "Sens_vs_day.pdf"), "-a /Applications/Preview.app"))


on.exit(setwd("~/Documents/git/projects/treegarden/budexperiments/docs/ms"))
