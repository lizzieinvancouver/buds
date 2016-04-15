forlatex = T # set to F if just trying new figures, T if outputting for final
runstan = F # set to T to actually run stan models. F if loading from previous runs

# Analysis of budburst experiment. Starting with simple linear models
# 2015-09-16 adding single species models

library(lme4)
library(rstan)
library(shinystan)
library(sjPlot)

library(memisc) # for getSummary (in trait effect plotting)
library(xtable)

library(scales) # for alpha
library(ggplot2)
library(picante)
library(caper) # for pgls
library(png) # readPNG for Fig 1

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("~/Documents/git/buds/analyses")
source('stan/savestan.R')
# get latest .Rdata file

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# To run from saved stan output (exclude Fake data output)
if(!runstan) {
  realout <- dir()[grep("Stan Output", dir())[is.na(match(grep("Stan Output", dir()), grep("Fake", dir())))]]
  load(sort(realout, T)[1])
  ls() 
  #launch_shinystan(doym.l)
}
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> 

print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

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

with(dxb, table(chill1, chill2)) # reductions due to nonbudburst
with(dxl, table(chill1, chill2)) # reductions due to nonleafouts

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Utility function
plotlet <- function(x, y, xlab=NULL, ylab=NULL, data, groups = NULL, xlim=NULL, ylim=NULL,...){
  
  minmax = range(c(data[grep(paste(x,"\\[",sep=""), rownames(data)),1], data[grep(paste(y,"\\[",sep=""), rownames(data)),1]))
  
  if(is.null(xlab)) xlab = x; if(is.null(ylab)) ylab = y
  
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
    else {
      colz = c("brown",#"brown3", 
               "blue3")#"midnightblue")
      ccolz = rep(colz[1], length(groups))
      ccolz[groups == 2] = colz[2]
      col.pch = ccolz
      col.lines = alpha(ccolz, 0.4)
    }
  
  if(is.null(xlim)) xlim = c(floor(minmax)[1], ceiling(minmax)[2])
  if(is.null(ylim)) ylim = c(floor(minmax)[1], ceiling(minmax)[2])
  
  plot(
  data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
  data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
  pch = "+",
  xlim = xlim,
  ylim = ylim,
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

shrubs = c("VIBLAN","RHAFRA","RHOPRI","SPIALB","VACMYR","VIBCAS", "AROMEL","ILEMUC", "KALANG", "LONCAN", "LYOLIG")
trees = c("ACEPEN", "ACERUB", "ACESAC", "ALNINC", "BETALL", "BETLEN", "BETPAP", "CORCOR", "FAGGRA", "FRANIG", "HAMVIR", "NYSSYL", "POPGRA", "PRUPEN", "QUEALB" , "QUERUB", "QUEVEL")

treeshrub = levels(dx$sp)
treeshrub[treeshrub %in% shrubs] = 1
treeshrub[treeshrub %in% trees] = 2
treeshrub = as.numeric(treeshrub)
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Analyses:
# 1. day of budburst by all factors, stan 
# 2. day of leaf out by all factors, stan
# 3. Effects on budburst/leafout day for species: 
#  - Traits (wood density, sla, N, stomata when we have it), 
#  - Phylogeny

# Supplemental analyses
# Correlate order of leafout/budburst in chambers to each other
# Correlate order of leafout in chambers to O'Keefe observational data

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# 1. Budburst day. 
if(runstan){
  datalist.b <- list(lday = dxb$bday, # budburst as response 
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
                 data = datalist.b, iter = 5005, chains = 4,
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
               "mu_b_inter_wp","mu_b_inter_ws","mu_b_inter_ps",
               "mu_b_inter_wc1","mu_b_inter_wc2",
               "mu_b_inter_pc1","mu_b_inter_pc2",
               "mu_b_inter_sc1","mu_b_inter_sc2")

meanzb <- sumerb[mu_params,col4table]

rownames(meanzb) = c("Temperature",
                    "Photoperiod",
                    "Chilling 4°",
                    "Chilling 1.5°C",
                    "Site",
                    "Temperature x Photoperiod",
                    "Temperature x Site",
                    "Photoperiod x Site",
                    "Temperature x Chilling 4°C",
                    "Temperature x Chilling 1.5°C",
                    "Photoperiod x Chilling 4°C",
                    "Photoperiod x Chilling 1.5°C",
                    "Site x Chilling 4°C",
                    "Site x Chilling 1.5°C"
                    )

# 2. leafout

if(runstan){
  datalist.l <- list(lday = dxl$lday, # leafout as respose 
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
                data = datalist.l, iter = 4000, chains = 4,
                control = list(adapt_delta = 0.9,
                               max_treedepth = 15)) 
}
sumerl <- summary(doym.l)$summary

# ssm.l <- as.shinystan(doym.l)
# yl = dxl$lday # for shinystan posterior checks
# launch_shinystan(ssm.l) 

meanzl <- sumerl[mu_params,col4table]
rownames(meanzl) = rownames(meanzb)
  
# Figure 1: Stan model effects for budburst and leafout

bbpng <- readPNG(file.path(figpath, "Finn_BB.png")) # Illustrations from Finn et al. 
lopng <- readPNG(file.path(figpath, "Finn_LO.png"))

pdf(file.path(figpath, "Fig1_bb_lo.pdf"), width = 7, height = 8)
  
  par(mfrow=c(2,1), mar = c(2, 10, 5, 1))
  
  # Upper panel: budburst
  plot(seq(-30, #min(meanz[,'mean']*1.1),
           12, #max(meanz[,'mean']*1.1),
           length.out = nrow(meanzb)), 
       1:nrow(meanzb),
       type="n",
       xlab = "",
       ylab = "",
       yaxt = "n")
  
  legend(x = -32, y = 6, bty="n", legend = "a. Budburst", text.font = 2)
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
  # Lower panel: leafout
  plot(seq(-30, #min(meanzl[,'mean']*1.1),
           12, #max(meanzl[,'mean']*1.1),
           length.out = nrow(meanzl)), 
       1:nrow(meanzl),
       type="n",
       xlab = "Model estimate change in day of phenological event",
       ylab = "",
       yaxt = "n")
  
  legend(x = -32, y = 6, bty="n", legend = "b. Leafout", text.font = 2)
  rasterImage(lopng, -28, 1, -21, 4)
  
  axis(2, at = nrow(meanzl):1, labels = rownames(meanzl), las = 1, cex.axis = 0.8)
  points(meanzl[,'mean'],
         nrow(meanzl):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzl[,"mean"]-meanzl[,"sd"], nrow(meanzl):1, meanzl[,"mean"]+meanzl[,"sd"], nrow(meanzl):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)
  
dev.off();system(paste("open", file.path(figpath, "Fig1_bb_lo.pdf"), "-a /Applications/Preview.app"))
  
# Figure 2: random effects 
pdf(file.path(figpath, "stanbb.pdf"), width = 14, height = 7)
  
  par(mfrow = c(1, 2))
  plotlet("b_warm", "b_photo", 
          xlab = "Advance due to 5° warming", 
          ylab = "Advance due to 4 hr longer photoperiod", 
          group = treeshrub,
          data = sumerb)
  
  plotlet("b_chill1", "b_chill2", 
          xlab = "Advance due to 30d 4° chilling", 
          ylab = "Advance due to 30d 1.5° chilling", 
          group = treeshrub,
          data = sumerb)
  
plotlet("b_warm", "b_photo", 
        xlab = "Advance due to 5° warming", 
        ylab = "Advance due to 4 hr longer photoperiod", 
        group = treeshrub,
        data = sumerl)

plotlet("b_chill1", "b_chill2", 
        xlab = "Advance due to 30d 4° chilling", 
        ylab = "Advance due to 30d 1.5° chilling", 
        group = treeshrub,
        data = sumerl)

dev.off();system(paste("open", file.path(figpath, "stanlo.pdf"), "-a /Applications/Preview.app"))

# Supp table 1: Stan model effects for bb
xtable(meanzb)

# Supp table 2: Stan model effects for leafout 
xtable(meanzl)  

if(runstan) { savestan("Inter") }

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# 2. Species-specific responses

# use trait data

dxt <- merge(dx, tr, by.x = "sp", by.y = "code")

aggcol = c("wd","sla","X.N","Pore.anatomy","lday","bday")

dxt$fg = "shrub"
dxt$fg[!is.na(match(dxt$sp, trees))] = "tree"

dxt$site <- factor(dxt$site, labels = c("HF","SH"))

dxt.agg <- aggregate(dxt[aggcol], by = list(dxt$sp,dxt$site,dxt$fg), FUN = mean, na.rm=T)

dxt.agg2 <- aggregate(dxt[aggcol], by = list(dxt$site,dxt$fg), FUN = mean, na.rm=T)

names(dxt.agg)[1:3] = c("sp","site","fg")
names(dxt.agg2)[1:2] = c("site","fg")

# Analyze leafout not #effect sizes# vs traits
gotchill <- tapply(dx$spn, dx$chill2, unique)$'1'
nochill <- unique(dx$spn)[is.na(match(unique(dx$spn), gotchill))]
dlo <- summary(doym.l)$summary
dlo[!is.na(match(rownames(dlo), paste("b_chill1[", nochill, "]", sep=""))),] = 99
dlo[!is.na(match(rownames(dlo), paste("b_chill2[", nochill, "]", sep=""))),] = 99

dxt.agg <- dxt.agg[order(dxt.agg$site, dxt.agg$sp),]

warmeff <- dlo[grep("b_warm\\[",rownames(dlo)),"mean"]
photoeff <- dlo[grep("b_photo\\[",rownames(dlo)),"mean"]
chill1eff <- dlo[grep("b_chill1\\[",rownames(dlo)),"mean"]
chill2eff <- dlo[grep("b_chill2\\[",rownames(dlo)),"mean"]

effs <- data.frame(sp = levels(dxt$sp), warmeff, photoeff, chill1eff, chill2eff)

dxt2 <- merge(dxt.agg, effs, by = "sp", keep.x = T)

dxt2 <- dxt2[!duplicated(dxt2$sp),]
dxt2 <- dxt2[!is.na(match(dxt2$sp, unique(dx$sp))),]

# use unscaled version for plotting
dxt[c("wd","sla","X.N","Pore.anatomy")] = scale(dxt[c("wd","sla","X.N","Pore.anatomy")])

rowz <- c("Intercept", "Stem density", "SLA", "% N","Pore anatomy")
traitlm.t <- getSummary(lm(lday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "tree",]))$coef
traitlmb.t <- getSummary(lm(bday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "tree",]))$coef
traitlm.s <- getSummary(lm(lday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "shrub",]))$coef
traitlmb.s <- getSummary(lm(bday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "shrub",]))$coef
rownames(traitlm.t) = rownames(traitlm.s) = rownames(traitlmb.t) = rownames(traitlmb.s) = rowz


xtable(traitlmb.t)

xtable(traitlm.t)

xtable(traitlmb.s)

xtable(traitlm.s)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Phylogeny
Nsummary <- aggregate(X.N ~ sp, data = dxt, mean, na.rm=T)
Nsummary[order(Nsummary$X.N),]
SLAsummary <- aggregate(sla ~ sp, data = dxt, mean, na.rm=T)
WDsummary <- aggregate(wd ~ sp, data = dxt, mean, na.rm=T)
LDaysummary <- aggregate(lday ~ sp, data = dxt, mean, na.rm=T)
BDaysummary <- aggregate(bday ~ sp, data = dxt, mean, na.rm=T)
PAsummary <- aggregate(Pore.anatomy ~ sp, data = dxt, mean, na.rm=T)

phsp <- ph$tip.label
phspcode <- unlist(lapply(strsplit(phsp, "_"), function(x) toupper(paste(substr(x[[1]],1,3), substr(x[[2]],1,3), sep=""))))

ph$tip.label = phspcode

pa.phylo <- drop.tip(ph, phsp[is.na(match(phspcode, PAsummary$sp))])

pamatch <- match(phspcode, PAsummary$sp)

## now with caper
dxt.agg <- aggregate(dxt[c("wd","sla","X.N","Pore.anatomy","lday","bday")], by = list(dxt$sp), mean, na.rm=T)
names(dxt.agg)[1] = "sp"

ph$node.label = NULL # otherwise give duplicated names error, because of multiple "" in node labels.

sig <- comparative.data(ph, dxt.agg, names.col = "sp")

sla.signal <- pgls(sla ~ lday, sig, lambda = 'ML')
n.signal <- pgls(X.N ~ lday, sig, lambda = 'ML')
wd.signal <- pgls(wd ~ lday, sig, lambda = 'ML')
pa.signal <- pgls(Pore.anatomy ~ lday, sig, lambda = 'ML')

signaldat <- data.frame(
  rbind(summary(sla.signal)$param["lambda"], 
        summary(n.signal)$param["lambda"],
        summary(wd.signal)$param["lambda"],
        summary(pa.signal)$param["lambda"]))
        
signaldat$var = c("SLA","% N","Wood Density","Pore anatomy")

xtable(signaldat)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Additional plotting
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Plot actual change in leafout by species
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
wa = la = oa = nn = vector()

for(i in unique(dx$sp)){ # i="ACEPEN"
  dxx <- dx[dx$sp == i,]
  
  nn <- c(nn, nrow(dxx[dxx$nl==1,]))
  
  overallm = mean(dxx[dxx$warm == 1 & dxx$photo == 1 & dxx$chill == 1, "lday"], na.rm=T)
  # mean across all cool
  cm <- mean(dxx[dxx$warm == 1,'lday'], na.rm=T)
  # advance from warming
  wm <- mean(dxx[dxx$warm == 2, 'lday'], na.rm=T)
  
  warmadv = wm - cm    
  
  # mean across all short
  sm <- mean(dxx[dxx$photo == 1,'lday'], na.rm=T)
  # advance from warming
  lm <- mean(dxx[dxx$photo == 2, 'lday'], na.rm=T)
   
  longadv = lm - sm   

  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
  }
adv=data.frame(sp=unique(dx$sp), warm=wa, photo=la, overall=oa, n=nn)

# Color classes for early - mid - late fl
hist(adv$overall)
oa.col <- cut(adv$overall, 3)
levels(oa.col) <- oc <- alpha(c("blue","purple","red"), 0.8)#heat.colors(5, alpha = 0.8)[1:3]
oa.col = as.character(oa.col)

pdf(file.path(figpath, "Advplot2.pdf"), width = 8, height = 9) # Adv plot is without varying cex by sample size
plot(adv$warm, adv$photo, 
     xlim = c(-30, -2),
     ylim=c(-20,-2),
     ylab = "Advance in leafout due to 4h longer photoperiod",
     xlab = "Advance in leafout due to 5°C warmer temperature",
     pch = 16, 
     #cex = 2, 
     col = oa.col,
     #pch = 1, 
     #col = alpha("midnightblue",0.5), lwd = 3,
     #cex = adv$overall/7
     cex = adv$n/12#log(adv$n)
     )
# text(adv$warm, adv$photo,
#      labels = adv$sp, cex = 0.8, adj = 0.5, #pos = 3,
#      col = alpha('grey20', 0.9))
legend(x = -7, y = -16, bty = "n", legend = c("Early", "Intermediate", "Late"), title = "Leafout period", col = oc, pch = 16, 
       pt.cex = 2,
       y.intersp = 1.5,
       x.intersp = 1.5)
legend(x = -13, y = -16, bty = "n", legend = c(25, 75), 
       title = "N leafout samples", col = "black", pch = 1, pt.cex = c(25, 75)/12,
       y.intersp = 2,
       x.intersp = 2
)

dev.off();system(paste("open", file.path(figpath, "Advplot2.pdf"), "-a /Applications/Preview.app"))

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
dev.off();system(paste("open", file.path(figpath, "Chillplot.pdf"), "-a /Applications/Preview.app"))



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


####### Trait pairs plot

panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, #col="darkblue",
       ...) }

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  rsig <- cor.test(x, y, use = "complete.obs")$p.value 
  
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  if(rsig <= 0.05) {    text(0.5, 0.5, txt, cex = 1, font=2)}
  else text(0.5, 0.5, txt, cex = 1, font=1)
}

pdf(file.path(figpath, "traitpairs.pdf"), width = 6, height = 6)
pairs(dxt[c("bday","lday","wd","sla","X.N","Pore.anatomy")],
      diag.panel = panel.hist, lower.panel = panel.cor,
      col = hsv(0.7,0.2,0.1,alpha = 0.1), pch = 16,
      labels = c("Budburst day","Leafout day","Stem density", "SLA", "Leaf N","Pore anatomy"),
      cex = 1.5,
      cex.labels = 1, oma = rep(2,4),
      font.labels = 2,
      gap = 0.5
)
dev.off();system(paste("open", file.path(figpath, "traitpairs.pdf"), "-a /Applications/Preview.app"))

