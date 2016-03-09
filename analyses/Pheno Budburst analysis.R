forlatex = F # set to F if just trying new figures, T if outputting for final

# Analysis of budburst experiment. Starting with simple linear models
# 2015-09-16 adding single species models

library(nlme)
library(scales)
library(arm)
library(rstan)
library(xtable)
library(memisc) # for getSummary
library(ggplot2)
library(GGally) # for ggpairs
library(picante)
library(sjPlot)
library(shinystan)

setwd("~/Documents/git/buds/analyses")
source('stan/savestan.R')
# get latest .Rdata file

print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))

dx <- dx[!is.na(dx$site),] # Delete betpap with not site, mystery twig which showed up at end of exp.

if(forlatex) figpath = "../docs/outline/images" else figpath = "graphs"

# Prep 

dx$warmn = scale(as.numeric(as.character(dx$warm)))
dx$photon = scale(as.numeric(as.character(dx$photo)))
dx$chilln = scale(as.numeric(substr(as.character(dx$chill), 6, 6)))
dx$spn <- as.numeric(dx$sp)

levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Analyses:
# 1. day of budburst by all factors, lmer 
# 1a. day of leaf out by all factors, lmer
# 2. Effects on budburst day for species: 
#  - Traits (wood density, sla)
#  - Phylogeny
#  - observed bbd
# 3. Individual level

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# 1. day of budburst by all factors, lmer. Using numeric predictors
# Graphic representation of data

m3 <- lmer(bday ~ warmn * photon * site * chilln + (warmn|sp) + (photon|sp), data = dx[dx$nl == 1,]) # NAs in lday being omitted, doesn't matter if specify nl == 1 or not.
summary(m3)
fixef(m3)
ranef(m3)

xtable(getSummary(m3)$coef)

# Graphic representation of model
pdf(file.path(figpath, "lmerDBB.pdf"), width = 5, height = 5)
sjp.lmer(m3, type = 'fe.std', 
         axisTitle.x = "Predictors of days to budburst",
          axisTitle.y = "Effect size",
         fade.ns = F)
dev.off();system(paste("open", file.path(figpath, "lmerDBB.pdf"), "-a /Applications/Preview.app"))


# Stan version for budburst day
dx <- dx[!is.na(dx$bday) & !is.na(dx$lday),]
sp_site = as.numeric(paste(dx$site, formatC(dx$sp, width = 2, flag = '0'), sep=""))
sp_sitef = factor(sp_site)
levels(sp_sitef) = 1:length(levels(sp_sitef))
sp_site = as.numeric(sp_sitef)

datalist4 <- list(lday = dx$bday, warm = dx$warm, site = dx$site, sp = dx$spn, sp_site = sp_site, photo = dx$photo, chill = dx$chill, N = nrow(dx), n_site = length(unique(dx$site)), n_sp = length(unique(dx$sp)), n_sp_site = length(unique(sp_site)))
  
doym4 <- stan('stan/doy_model7.stan', data = datalist4, iter = 4000, chains = 4) 

sum4 <- summary(doym4)$summary# 3765 rows... what is best way to summarize?
# ssm4 <- launch_shinystan(doym4) 

# Site effects are a1 and a2. Species level effects for warming and photo are 3:28 and 31:58. Then chill, then interaction of warm x photo. 
# how to see the pooled parameters - Use extract
# a <- extract(doym4)
# 
# mean(a$b_warm) # overall effect of warming on leafout day
# mean(a$b_photo) 
# mean(a$b_chill) 
# 
# mean(a$b_inter) # warm * photo interax
# #hist(a$a) # site effects
sumparams <- c("a","b_warm","b_photo","b_chill","b_inter")

xtable(sum4[1:114,c(1,2,3,10)]) # for supplemental material for now, need a better way to summarize.

ranef(m3) # compare with stan version
sum4[grep("b_warm", rownames(sum4)),c(1,2,3,10)]
plot(ranef(m3)$sp$warmn, sum4[grep("b_warm", rownames(sum4)),1]) # Barely related. Need help with stan model

savestan()

# 1a. leafout

m3l <- lmer(lday ~ warmn * photon * site * chilln + (warmn|sp) + (photon|sp), data = dx[dx$nl == 1,]) # NAs in lday being omitted, doesn't matter if specify nl == 1 or not.
summary(m3l)
fixef(m3l)
ranef(m3l)

xtable(getSummary(m3l)$coef)

pdf(file.path(figpath, "lmerDLO.pdf"), width = 5, height = 5)
sjp.lmer(m3l, type = 'fe.std', 
         axisTitle.x = "Predictors of days to budburst",
         axisTitle.y = "Effect size",
         fade.ns = F)
dev.off();system(paste("open", file.path(figpath, "lmerDLO.pdf"), "-a /Applications/Preview.app"))

# Stan version for leafout day
datalist4 <- list(lday = dx$lday, warm = dx$warm, site = dx$site, sp = dx$spn, sp_site = sp_site, photo = dx$photo, chill = dx$chill, N = nrow(dx), n_site = length(unique(dx$site)), n_sp = length(unique(dx$sp)), n_sp_site = length(unique(sp_site)))

doym4l <- stan('stan/doy_model41.stan', data = datalist4, iter = 1000, chains = 4) 

sum4l <- summary(doym4l)$summary
# ssm4l <- launch_shinystan(doym4l) 

xtable(sum4l[1:114,c(1,2,3,10)]) # for supplemental material for now, need a betterw way to summarize.

savestan()


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# 2. Species-specific responses

# use trait data

dxt <- merge(dx, tr, by.x = "sp", by.y = "code")
#ggpairs(dxt[c("wd","sla","X.N","Pore.anatomy","lday","bday")])

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

dxt[c("wd","sla","X.N","Pore.anatomy")] = scale(dxt[c("wd","sla","X.N","Pore.anatomy")])

(traitlm <- getSummary(lm(lday ~ wd + sla + X.N + Pore.anatomy, data = dxt)))

(traitlmb <- getSummary(lm(bday ~ wd + sla + X.N + Pore.anatomy, data = dxt)))

xtable(traitlmb$coef)

xtable(traitlm$coef)


Nsummary <- aggregate(X.N ~ sp, data = dxt, mean, na.rm=T)
Nsummary[order(Nsummary$X.N),]


SLAsummary <- aggregate(sla ~ sp, data = dxt, mean, na.rm=T)

WDsummary <- aggregate(wd ~ sp, data = dxt, mean, na.rm=T)

LDaysummary <- aggregate(lday ~ sp, data = dxt, mean, na.rm=T)
BDaysummary <- aggregate(bday ~ sp, data = dxt, mean, na.rm=T)

PAsummary <- aggregate(Pore.anatomy ~ sp, data = dxt, mean, na.rm=T)

#plot(SLAsummary$sla, Nsummary$X.N)
#text(SLAsummary$sla, Nsummary$X.N, SLAsummary$sp)

# Phylogeny
phsp <- ph$tip.label
phspcode <- unlist(lapply(strsplit(phsp, "_"), function(x) toupper(paste(substr(x[[1]],1,3), substr(x[[2]],1,3), sep=""))))

pa.phylo <- drop.tip(ph, phsp[is.na(match(phspcode, PAsummary$sp))])
pamatch <- match(phspcode, PAsummary$sp)

sla.signal <- phylosignal(SLAsummary[match(phspcode, SLAsummary$sp),2], ph)
n.signal <- phylosignal(Nsummary[match(phspcode, Nsummary$sp),2], ph)
wd.signal <- phylosignal(WDsummary[match(phspcode, WDsummary$sp),2], ph)
lday.signal <- phylosignal(LDaysummary[match(phspcode, LDaysummary$sp),2], ph)
bday.signal <- phylosignal(BDaysummary[match(phspcode, BDaysummary$sp),2], ph)

pa.signal <- phylosignal(PAsummary[pamatch[!is.na(pamatch)],2], pa.phylo)


signaldat <- data.frame(rbind(bday.signal, lday.signal, wd.signal, pa.signal, sla.signal,n.signal))
signaldat$var = c("Budburst","Leafout","Wood Density","Pore anatomy","SLA", "% N")

names(signaldat) = c("K","PIC variance","PIC var rand", "PIC variance P","PIC variance Z","Variable")


xtable(signaldat[c(6,1,2,4)])

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# 3. Individual level
# look at consistency of performance within individuals, across treatments, as measure of plasticity.

# Is consistency related to earlier leafout?

vars <- aggregate(lday ~ sp + site + ind + wd + sla + X.N + Pore.anatomy, FUN = function(x) sd(x, na.rm=T) 
                  / mean(x, na.rm=T)
                  , data = dxt)

# remove extreme values 

vars$day = lday.agg[match(vars$sp, lday.agg$sp),"lday"]
vars$site = as.factor(vars$site); levels(vars$site) = c("HF","SH")
summary(lmer(lday ~ day + (1|sp), data = vars))

pdf(file.path(figpath, "indvar.pdf"), width = 5, height = 5)

ggplot(vars, aes(day, lday, group = site)) + geom_point(aes(col=site)) + geom_smooth(method = "lm") + 
    xlab("Day of leafout in Warm/Long") + ylab("CV of leafout across treatments within individuals") 

dev.off();system(paste("open", file.path(figpath, "indvar.pdf"), "-a /Applications/Preview.app"))


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
  
  warmadv = cm - wm    
  
  # mean across all short
  sm <- mean(dxx[dxx$photo == 1,'lday'], na.rm=T)
  # advance from warming
  lm <- mean(dxx[dxx$photo == 2, 'lday'], na.rm=T)
   
  longadv = sm - lm   

  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm)
  }
adv=data.frame(sp=unique(dx$sp), warm=wa, photo=la, overall=oa)

pdf(file.path(figpath, "Advplot.pdf"), width = 8, height = 9)
plot(warm ~ photo, data = adv, xlim = c(0, 20),ylim=c(0,30),
     xlab = "Advance in leafout due to photoperiod",
     ylab = "Advance in leafout due to warming",
     pch = 1, col = alpha("midnightblue",0.5), lwd = 3,
     cex = overall/4
     )
text(adv$photo,adv$warm,
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



