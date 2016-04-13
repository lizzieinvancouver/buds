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
  launch_shinystan(ssm.l)
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
# Utility functions
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
# 1a. day of leaf out by all factors, stan
# 2. Effects on budburst day for species: 
#  - Traits (wood density, sla)
#  - Phylogeny

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# 1. day of budburst by all factors, first lmer. Using numeric predictors. Lmer can't handle all the interactions

m1 <- lmer(bday ~ (site|sp) + (warm|sp) + (photo|sp) + (chill1|sp) + (chill2|sp), data = dxl) 

# with interactions... hard for lmer to do. I didn't have patience to let it try, seems stuck after ~ 10 min

# m2 <- lmer(bday ~ (site*warmn|sp) + 
#              (site*photon|sp) + 
#              (site*chill1|sp) + 
#              (site*chill2|sp) + 
#              (warmn*photon|sp) +
#              (warmn*chill1|sp) +
#              (warmn*chill2|sp) +
#              (photon*chill1|sp) +
#              (photon*chill2|sp) 
#            , data = dx) 

# with interactions

summary(m1)
fixef(m1)
ranef(m1) # chill1 and chill2 effects similar to stan estimates

# xtable(getSummary(m3)$coef)
# # Graphic representation of model
# pdf(file.path(figpath, "lmerDBB.pdf"), width = 5, height = 5)
# sjp.lmer(m3, type = 'fe.std', 
#          axisTitle.x = "Predictors of days to budburst",
#          axisTitle.y = "Effect size",
#          fade.ns = F)
# dev.off();system(paste("open", file.path(figpath, "lmerDBB.pdf"), "-a /Applications/Preview.app"))
# 
# Stan version for budburst day. 

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

if(runstan){
  doym.b <- stan('stan/lday_site_sp_chill_inter.stan', 
                 data = datalist.b, iter = 5005, chains = 4,
                 control = list(adapt_delta = 0.9,
                                max_treedepth = 15)) 
  
}
  sumerb <- summary(doym.b)$summary
  sumerb[grep("mu_", rownames(sumerb)),]
  
  #ssm.b <- as.shinystan(doym.b)
  
  # launch_shinystan(ssm.b) 

  yb = dxb$bday # for shinystan posterior checks

# plot effects
col4table <- c("mean","sd","25%","50%","75%","Rhat")
meanz <- sumerb[grep("mu_", rownames(sumerb)),col4table]
rownames(meanz) = c("Temperature",
                    "Chilling 4°",
                    "Chilling 1.5°C",
                    "Photoperiod",
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
xtable(meanz)


pdf(file.path(figpath, "stanbb_fix.pdf"), width = 7, height = 4)

par(mfrow=c(1,1), mar = c(5, 10, 2, 1))

plot(seq(-30, #min(meanz[,'mean']*1.1),
         12, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanz)), 
     1:nrow(meanz),
     type="n",
     xlab = "Model estimate change in budburst day",
     ylab = "",
     yaxt = "n")

axis(2, at = nrow(meanz):1, labels = rownames(meanz), las = 1, cex.axis = 0.8)
points(meanz[,'mean'],
       nrow(meanz):1,
       pch = 16,
       col = "midnightblue")
arrows(meanz[,"mean"]-meanz[,"sd"], nrow(meanz):1, meanz[,"mean"]+meanz[,"sd"], nrow(meanz):1,
                   len = 0, col = "black")
abline(v = 0, lty = 3)
dev.off();system(paste("open", file.path(figpath, "stanbb_fix.pdf"), "-a /Applications/Preview.app"))

# Plot random effects 
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

dev.off();system(paste("open", file.path(figpath, "stanbb.pdf"), "-a /Applications/Preview.app"))

# 1a. leafout

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

if(runstan){
  doym.l <- stan('stan/lday_site_sp_chill_inter.stan',
                data = datalist.l, iter = 4000, chains = 4,
                control = list(adapt_delta = 0.9,
                               max_treedepth = 15)) 
}
  sumerl <- summary(doym.l)$summary
  sumerl[grep("mu_", rownames(sumerl)),]
  
 # ssm.l <- as.shinystan(doym.l)
  yl = dxl$lday # for shinystan posterior checks
  # launch_shinystan(ssm.l) 

  col4table <- c("mean","sd","25%","50%","75%","Rhat")
  meanz <- sumerl[grep("mu_", rownames(sumerl)),col4table]
  rownames(meanz) = c("Temperature",
                      "Chilling 4°",
                      "Chilling 1.5°C",
                      "Photoperiod",
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
  xtable(meanz)  
  
  pdf(file.path(figpath, "stanlo_fix.pdf"), width = 7, height = 4)
  
  par(mfrow=c(1,1), mar = c(5, 10, 2, 1))
  
  plot(seq(-30, #min(meanz[,'mean']*1.1),
           12, #max(meanz[,'mean']*1.1),
           length.out = nrow(meanz)), 
       1:nrow(meanz),
       type="n",
       xlab = "Model estimate change in leafout day",
       ylab = "",
       yaxt = "n")
  
axis(2, at = nrow(meanz):1, labels = rownames(meanz), las = 1, cex.axis = 0.8)
points(meanz[,'mean'],
         nrow(meanz):1,
         pch = 16,
         col = "midnightblue")
arrows(meanz[,"mean"]-meanz[,"sd"], nrow(meanz):1, meanz[,"mean"]+meanz[,"sd"], nrow(meanz):1,
         len = 0, col = "black")
abline(v = 0, lty = 3)

dev.off();system(paste("open", file.path(figpath, "stanlo_fix.pdf"), "-a /Applications/Preview.app"))
  
pdf(file.path(figpath, "stanlo.pdf"), width = 14, height = 7)
           
par(mfrow = c(1, 2))
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

savestan("Inter")

}

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

# Analyze effect sizes vs traits

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

# use unscaled version for plotting
dxt[c("wd","sla","X.N","Pore.anatomy")] = scale(dxt[c("wd","sla","X.N","Pore.anatomy")])

### Coefficients of traits by phenology effects

### Warming

(traitlm.t.warm <- getSummary(lm(warmeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "tree",])))

xtable(traitlm.t.warm$coef)

(traitlm.s.warm <- getSummary(lm(warmeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "shrub",])))

xtable(traitlm.s.warm$coef)

### Photo

(traitlm.t.photo <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "tree",])))

xtable(traitlm.t.photo$coef)

(traitlm.s.photo <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "shrub",])))

xtable(traitlm.s.photo$coef)

### Chill1

(traitlm.t.chill1 <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "tree" & dxt2$chill1eff != 99,])))

xtable(traitlm.t.chill1$coef)

(traitlm.s.chill1 <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "shrub" & dxt2$chill1eff != 99,])))

xtable(traitlm.s.chill1$coef)


### Chill2

(traitlm.t.chill2 <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "tree" & dxt2$chill1eff != 99,])))

(traitlmb.t.chill2 <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2bb[dxt2bb$fg == "tree" & dxt2bb$chill1eff != 99,])))

xtable(traitlmb.t.chill2$coef)

xtable(traitlm.t.chill2$coef)

(traitlm.s.chill2 <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2[dxt2$fg == "shrub" & dxt2$chill1eff != 99,])))

(traitlmb.s.chill2 <- getSummary(lm(photoeff ~ wd + sla + X.N + Pore.anatomy, data = dxt2bb[dxt2bb$fg == "shrub" & dxt2bb$chill1eff != 99,])))

xtable(traitlm.s.chill2$coef)


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

# sla.signal <- phylosignal(SLAsummary[match(phspcode, SLAsummary$sp),2], ph)
# n.signal <- phylosignal(Nsummary[match(phspcode, Nsummary$sp),2], ph)
# wd.signal <- phylosignal(WDsummary[match(phspcode, WDsummary$sp),2], ph)
# lday.signal <- phylosignal(LDaysummary[match(phspcode, LDaysummary$sp),2], ph)
# bday.signal <- phylosignal(BDaysummary[match(phspcode, BDaysummary$sp),2], ph)
# pa.signal <- phylosignal(PAsummary[pamatch[!is.na(pamatch)],2], pa.phylo)
# signaldat <- data.frame(rbind(bday.signal, lday.signal, wd.signal, pa.signal, sla.signal,n.signal))
# signaldat$var = c("Budburst","Leafout","Wood Density","Pore anatomy","SLA", "% N")
# names(signaldat) = c("K","PIC variance","PIC var rand", "PIC variance P","PIC variance Z","Variable")

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

# Plot m3 intercepts
plot(ranef(m3)$sp[,1],ranef(m3)$sp[,3],
	pch = "+", col = "grey10",
	type = "n",
	xlab = "Warming response",
	ylab = "Photoperiod response",
	xlim = c(-18, 18),
	ylim = c(-18, 18)
	)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
points(ranef(m3)$sp[,1],ranef(m3)$sp[,3],
	pch = "+", col = "grey10")

text(ranef(m3)$sp[,1],ranef(m3)$sp[,3], 
	labels = rownames(ranef(m3)$sp), cex = 0.6, pos = 1,
	col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))
dev.print(file = "ranefs.pdf", device = pdf)

# plot slopes
plot(ranef(m3)$sp[,2],ranef(m3)$sp[,4],
     pch = "+", col = "grey10",
     type = "n",
     xlab = "Warming response",
     ylab = "Photoperiod response",
     xlim = c(-8, 8),
     ylim = c(-8, 8)
)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
points(ranef(m3)$sp[,2],ranef(m3)$sp[,4],
       pch = "+", col = "grey10")

text(ranef(m3)$sp[,2],ranef(m3)$sp[,4], 
     labels = rownames(ranef(m3)$sp), cex = 0.6, pos = 1,
     col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))

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
wa = la = oa = vector()
for(i in unique(dx$sp)){ # i="ACEPEN"
  dxx <- dx[dx$sp == i,]
  
  overallm = mean(dxx$lday, na.rm=T)
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
adv=data.frame(sp=unique(dx$sp), warm=wa, photo=la, overall=oa)

pdf(file.path(figpath, "Advplot.pdf"), width = 8, height = 9)
plot(adv$warm, adv$photo, 
     xlim = c(-25, -2),
     ylim=c(-25,-2),
     ylab = "Advance in leafout due to photoperiod",
     xlab = "Advance in leafout due to warming",
     pch = 1, col = alpha("midnightblue",0.5), lwd = 3,
     cex = adv$overall/4
     )
text(adv$warm, adv$photo,
     labels = adv$sp, cex = 0.8, adj = 0.5,
     col = alpha('grey20', 0.9))

dev.off();system(paste("open", file.path(figpath, "Advplot.pdf"), "-a /Applications/Preview.app"))

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
