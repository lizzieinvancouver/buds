forlatex =T # set to F if just trying new figures, T if outputting for final
runstan = T # set to T to actually run stan models. F if loading from previous runs

# Analysis of budburst experiment. Starting with simple linear models
# 2015-09-16 adding single species models

library(nlme)
library(arm)
library(rstan)
library(shinystan)
library(sjPlot)

library(xtable)
library(memisc) # for getSummary

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

# To run from saved stan output
if(!runstan) { 
  load(sort(dir()[grep("Stan Output", dir())], T)[1])
  ls() 
  launch_shinystan(ssm.l)
}
# 
# 
# load("Stan Output 2016-03-21.RData") # this one is for lday_nosite_plusspint.stan, without the three levels of chilling. But need to re-do, this one pools for site.
# launch_shinystan(doym.l)
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> 

print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))

if(forlatex) figpath = "../docs/ms/images" else figpath = "graphs"

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

# Chill dummy variables
dx$chill1 = ifelse(dx$chill == 2, 1, 0) 
dx$chill2 = ifelse(dx$chill == 3, 1, 0) 

with(dx, table(chill1, chill2)) # all three levels in here

dx <- dx[!is.na(dx$bday) & !is.na(dx$lday),]

with(dx, table(chill1, chill2)) # reductions due to nonleafouts

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

plotlet <- function(x, y, xlab, ylab, data, groups = NULL){
  
  minmax = range(c(data[grep(paste(x,"\\[",sep=""), rownames(data)),1], data[grep(paste(y,"\\[",sep=""), rownames(data)),1]))
  
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
    else {
      colz = c("midnightblue", "darkgreen")
      ccolz = rep(colz[1], length(groups))
      ccolz[groups == 2] = colz[2]
      col.pch = ccolz
      col.lines = alpha(ccolz, 0.4)
    }
  
  
  plot(
  data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
  data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
  pch = "+",
  xlim = c(floor(minmax)[1], ceiling(minmax)[2]),
  ylim = c(floor(minmax)[1], ceiling(minmax)[2]),
  ylab = ylab,
  xlab = xlab,
  col = col.pch
  )

  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"25%"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"75%"],
    length = 0, col = col.lines)
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"25%"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"75%"],
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

m1 <- lmer(bday ~ (site|sp) + (warmn|sp) + (photon|sp) + (chill1|sp) + (chill2|sp), data = dx) 

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

datalist.b <- list(lday = dx$bday, # budburst as respose 
                   warm = as.numeric(dx$warm), 
                   site = as.numeric(dx$site), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill1 = as.numeric(dx$chill1),
                   chill2 = as.numeric(dx$chill2),
                   N = nrow(dx), 
                   n_site = length(unique(dx$site)), 
                   n_sp = length(unique(dx$sp))
)

if(runstan){
  doym.b <- stan('stan/lday_site_sp_chill_inter.stan', 
                 data = datalist.b, iter = 5005, chains = 4,
                 control = list(adapt_delta = 0.9,
                                max_treedepth = 15)) 
  
  sumerb <- summary(doym.b)$summary
  sumerb[grep("mu_", rownames(sumerb)),]
  
  ssm.b <- as.shinystan(doym.b)
  # launch_shinystan(ssm.b) 

y = dx$bday # for shinystan posterior checks

# Plot random effects 
pdf(file.path(figpath, "stanbb.pdf"), width = 7, height = 7)

plotlet("b_warm", "b_photo", 
        xlab = "Advance due to 10° warming", 
        ylab = "Advance due to 4 hr longer photoperiod", 
        group = treeshrub,
        data = sumerb)

plotlet("b_chill1", "b_chill2", 
        xlab = "Advance due to 30d 4° chilling", 
        ylab = "Advance due to 30d 1.5° chilling", 
        group = treeshrub,
        data = sumerb)

dev.off();system(paste("open", file.path(figpath, "stanbb.pdf"), "-a /Applications/Preview.app"))

savestan("Inter")

}
# 1a. leafout

datalist.l <- list(lday = dx$lday, # leafout as respose 
                   warm = as.numeric(dx$warm), 
                   site = as.numeric(dx$site), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill1 = as.numeric(dx$chill1),
                   chill2 = as.numeric(dx$chill2),
                   N = nrow(dx), 
                   n_site = length(unique(dx$site)), 
                   n_sp = length(unique(dx$sp))
)

if(runstan){
  doym.l <- stan('stan/lday_site_sp_chill_inter.stan',
                data = datalist.l, iter = 4000, chains = 4,
                control = list(adapt_delta = 0.9,
                               max_treedepth = 15)) 
  
  sumerl <- summary(doym.l)$summary
  sumerl[grep("mu_", rownames(sumerl)),]
  
  ssm.l <- as.shinystan(doym.l)
  # launch_shinystan(ssm.l) 
  
pdf(file.path(figpath, "stanlo.pdf"), width = 7, height = 7)
           
plotlet("b_warm", "b_photo", 
        xlab = "Advance due to 10° warming", 
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
#ggpairs(dxt[c("wd","sla","X.N","Pore.anatomy","lday","bday")])

dxt$fg = "shrub"
dxt$fg[!is.na(match(dxt$sp, trees))] = "tree"

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

(traitlm.t <- getSummary(lm(lday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "tree",])))

(traitlmb.t <- getSummary(lm(bday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "tree",])))

xtable(traitlmb.t$coef)

xtable(traitlm.t$coef)




(traitlm.s <- getSummary(lm(lday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "shrub",])))

(traitlmb.s <- getSummary(lm(bday ~ wd + sla + X.N + Pore.anatomy, data = dxt[dxt$fg == "shrub",])))

xtable(traitlmb.s$coef)

xtable(traitlm.s$coef)


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

ph$tip.label = phspcode

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


## now with caper
dxt.agg <- aggregate(dxt[c("wd","sla","X.N","Pore.anatomy","lday","bday")], by = list(dxt$sp), mean)
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
