## Started back when by Dan Flynn ##
## Updates by Lizzie starting in early 2018 ##

# Where were the non-leafout cuttings, by species, site, and treatement?

# Impt NOTE: Lizzie did not update all of Dan's code...
# should remove or go through it at some point #

useshinystan <- FALSE
runstan <- FALSE
forIsabelle <- TRUE

library(scales)
library(gplots) # for textplot()
library(png)
library(arm) # for invlogit
library(rstanarm)
if(useshinystan){
library(shinystan)
}
# setwd("~/Documents/git/buds/analyses")
setwd("~/Documents/git/projects/treegarden/budexperiments/analyses")

# get latest data
print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))
figpath = "../docs/ms/images"

source("source/simpleplot.R")

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check

# Add non-budburst
dx$no <- dx$bday
dx$no[dx$no>0] <- 1
dx$no[is.na(dx$no)==TRUE] <- 0
# Overall non-budburst versus non-leafout rates:
sum(dx$no)
sum(dx$nl)
dim(dx)
# Subset to data that did burst bud
dx.bb <- subset(dx, no==1)

# What percent did not break bud?
(1-sum(dx$no)/nrow(dx))*100

# What percent did not leafout?
(1-sum(dx$nl)/nrow(dx))*100

## A few notes learned while using rstanarm ...
# (1) The hierarchical effects are given as deviations from the global parameters (called the b parameters) so you have to correct those  http://discourse.mc-stan.org/t/question-about-hierarchical-effects/3226
# (2) Watch out of factors versus integers! I was posting the dx data (e.g., write.csv(dx, "output/dx.nonleafouts.csv", row.names=TRUE) then dx <- read.csv("output/dx.nonleafouts.csv", header=TRUE)) which reads in warm and photo as INTEGERS and thus you get different answers from the model with those (otherwise identical data) then you get here. 

if(runstan){
# models (m1.nl, m1.no) with species pooling on chilling and site effects (and their interactions)
m1.no <- stan_glmer(no ~ (warm + photo + chill + site + 
    warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
    (chill + chill*site|sp), family = binomial(link = "logit"), data = dx)

# Understanding models: m1.no
# invlogit(3.007 + -0.96*1) # warm is -0.096; photo is -0.007; chill1 is -0.693, chill2 is -1.506, site is +0.542; QUEALB on intercept: -1.798
# xhere <- -1.506
# invlogit(3.007 +  xhere*1)-invlogit(3.007 +  xhere*0) # so chill2 increases leafout by 13.5%

m1.nl <- stan_glmer(nl ~ (warm + photo + chill + site + 
    warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
    (chill + chill*site|sp), family = binomial(link = "logit"), data = dx)

# models (m2.nl, m2.no) with species pooling on intercept only
m2.no <- stan_glmer(no ~ (warm + photo + chill + site + 
    warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
    (1|sp), family = binomial(link = "logit"), data = dx)

m2.nl <- stan_glmer(nl ~ (warm + photo + chill + site + 
    warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
    (1|sp), family = binomial(link = "logit"), data = dx) # considers all data so includes non-leafouts that did not burst bud and non-leafouts that did, but then did not burst bud

m2.nl.bb <- stan_glmer(nl ~ (warm + photo + chill + site + 
    warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
    (1|sp), family = binomial(link = "logit"), data = dx.bb) # considers only those that did burst bud, but then did not leafout
    
# Understanding models: notes for main text (m2 models)
summary(m2.no, digits=3)
# warm is -0.094; photo is -0.008; chill1 is -0.703, chill2 is -1.477, site is +0.420; QUEALB on intercept: -1.649 (highest)
# xhere <- -0.703
# xhere <- -1.477
# xhere <- 0.42
# 100*(invlogit(2.899 +  xhere*1)-invlogit(2.899 +  xhere*0))
# chill1 decreases BB by 4.8%; chill2 by 14.2%; SH increases BB success by 1.72%

summary(m2.nl, digits=3)
# warm is 0.525; photo is 0.671; chill1 is -0.768, chill2 is -1.752, site is 0.016
# xhere <- 0.525
# xhere <- 0.671
# xhere <- -0.768
# xhere <- -1.752
# 100*(invlogit(1.770 +  xhere*1)-invlogit(1.770 +  xhere*0))
# warm increases leafout by 5.4%; photo increases leafout by 6.5%;  chill1 decreases leafout by 12.3%; chill2 by 35%

summary(m2.nl.bb, digits=3)
# warm is 1.086; photo is 1.269; chill1 is -0.843, chill2 is -1.875, site is -0.293, warm x photo is -1.693
# xhere <- 1.086
# xhere <- 1.269
# xhere <- -0.843
# xhere <- -1.875
# xhere <- -0.293
# xhere <- -1.639
# 100*(invlogit(2.75 +  xhere*1)-invlogit(2.75 +  xhere*0))
# photo x temp
# 100*(invlogit(2.75 +  1.086+1.269-1.639)-invlogit(2.75))
# warm increases leafout by 3.9%; photo increases leafout by 4.2%;  chill1 decreases leafout by 6.9%; chill2 by 23.4%, site decreases leafout by 1.9, overall photo x temp increases leafout by only 3.0%
 
#m3.no <- stan_glmer(no ~ (warm + photo + chill + site + 
#    warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
#    ((warm + photo + chill + site + warm*photo + warm*chill + photo*chill +
#    warm*site + photo*site + chill*site)|sp), family = binomial(link = "logit"), data = dx)
# error occurred during calling the sampler; sampling not done
# Error in check_stanfit(stanfit) : 
#  Invalid stanfit object produced please report bug

save(m1.no, file="stan/models_nonleafout/m1.no.Rdata")
save(m1.nl, file="stan/models_nonleafout/m1.nl.Rdata")
save(m2.no, file="stan/models_nonleafout/m2.no.Rdata")
save(m2.nl, file="stan/models_nonleafout/m2.nl.Rdata")
save(m2.nl.bb, file="stan/models_nonleafout/m2.nl.Rdata")


}


if(!runstan){
load("stan/models_nonleafout/m1.no.Rdata")
load("stan/models_nonleafout/m1.nl.Rdata")
load("stan/models_nonleafout/m2.no.Rdata")
load("stan/models_nonleafout/m2.nl.Rdata")

}

if(useshinystan){
launch_shinystan(m1.no)
launch_shinystan(m1.nl)
}


##############################
### Plotting for m2 models ###
##############################

## Plotting the models (m2.nl or m2.nl.bb, AND m2.no) with species pooling on chilling and site effects (and their interactions)

## Select an m2 model (see notes above on differences where models are fit)
m2nl.model <- m2.nl.bb

## Below gives the main text figure on LOGIT SCALE

bbpng <- readPNG(file.path(figpath, "Finn_BB.png")) # Illustrations from Finn et al.
lopng <- readPNG(file.path(figpath, "Finn_LO.png"))
col4fig <- c("mean","sd","25%","50%","75%","Rhat")
col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat")

sumer.m2no <- summary(m2.no)

# manually to get right order
mu_params <- c("warm20","photo12","chillchill1","chillchill2","siteSH",
               "warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

meanzb <- sumer.m2no[mu_params,col4fig]

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

sumer.m2nl <- summary(m2nl.model)
meanzl <- sumer.m2nl[mu_params,col4fig]
rownames(meanzl) <- rownames(meanzb)


## prep the tables and write them out 
meanzb.table <- sumer.m2no[mu_params,col4table]
row.names(meanzb.table) <- row.names(meanzb)

meanzl.table <- sumer.m2nl[mu_params,col4table]
row.names(meanzl.table) <- row.names(meanzl)

write.csv(meanzb.table, "output/nonleafout.meanzb.table.csv", row.names=FALSE)
write.csv(meanzl.table, "output/nonleafout.meanzl.table.csv", row.names=FALSE)

## back to the figure ...
pdf(file.path(figpath, "NonBBLO_m2.pdf"), width = 7, height = 8)
par(mfrow=c(2,1), mar = c(2, 10, 5, 1))
  
  # Upper panel: bud burst
  plot(seq(-2.5, 
           2,
           length.out = nrow(meanzb)), 
       1:nrow(meanzb),
       type="n",
       xlab = "",
       ylab = "",
       yaxt = "n")
  
  legend(x =-2.8, y = 3, bty="n", legend = "a. Budburst", text.font = 2)
  # rasterImage(bbpng, -3, 1, -2, 4)
  
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
  arrows(-0.1, 15.5, -2.5, 15.5, len=0.1, col = "black")
  legend(-1, 17, legend="delay", bty="n", text.font = 1, cex=0.75)
  arrows(0.1, 15.5, 2, 15.5, len=0.1, col = "black")
  legend(0.2, 17, legend="advance", bty="n", text.font = 1, cex=0.75)
  legend(-0.25, 16.25, legend="0", bty="n", text.font = 1, cex=0.75)
  par(xpd=FALSE)

  par(mar=c(5, 10, 2, 1))
  # Lower panel: leaf-out
  plot(seq(-2.5, 
           2, 
           length.out = nrow(meanzl)), 
       1:nrow(meanzl),
       type="n",
       xlab = "Model estimate change in budburst or leafout success",
       ylab = "",
       yaxt = "n")
  
  legend(x = -2.8, y = 3, bty="n", legend = "b. Leafout", text.font = 2)
  # rasterImage(lopng, -20, 1, -14, 4)
  
  axis(2, at = nrow(meanzl):1, labels = rownames(meanzl), las = 1, cex.axis = 0.8)
  points(meanzl[,'mean'],
         nrow(meanzl):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzl[,"75%"], nrow(meanzl):1, meanzl[,"25%"], nrow(meanzl):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)

dev.off()


####
### Supplemental figure -- with species-level estimates shown!

iter.m2no <- as.data.frame(m2.no)
iter.m2nl <- as.data.frame(m2nl.model)

# manually to get right order, with intercept
mu_params.wi <- c("(Intercept)", "warm20","photo12","chillchill1","chillchill2",
               "siteSH","warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

meanzb.wi <- sumer.m2no[mu_params.wi,col4fig]

rownames(meanzb.wi) = c("Intercept",
                    "Forcing Temperature",
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

meanzl.wi <- sumer.m2nl[mu_params.wi,col4fig]
rownames(meanzl.wi) <- rownames(meanzb.wi)

speff.bb <- speff.lo <- vector()
params <- c("(Intercept)", "warm20","photo12","chillchill1",
               "chillchill2","siteSH", "warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

sp.params <- c("(Intercept)")

params.wsp <- c(1)
params.nosp <- c(1:15)[-params.wsp]

pdf(file.path(figpath, "NonBBLO_sp_m2.pdf"), width = 7, height = 8)

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))
# Upper panel: budburst
plot(seq(-4, #min(meanz[,'mean']*1.1),
         6, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb.wi)), 
     seq(1, 5*nrow(meanzb.wi), length.out = nrow(meanzb.wi)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x =-4.75, y = 11, bty="n", legend = "a. Budburst", text.font = 2)
rasterImage(bbpng, -4, 0, -2, 7)

axis(2, at = 5*(nrow(meanzb.wi):1), labels = rownames(meanzb.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dx$sp))){
  b.params <- iter.m2no[!is.na(match(colnames(iter.m2no), c(paste("b", "[", sp.params, " sp:",
      unique(dx$sp)[i], "]", sep=""))))]

  main.params <- iter.m2no[!is.na(match(colnames(iter.m2no), sp.params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp], sp.est[,"25%"],  jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp],
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp], #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.bb = rbind(speff.bb, t(sp.est[,1]))
    }

arrows(meanzb.wi[,"75%"], (5*(nrow(meanzb.wi):1))+1, meanzb.wi[,"25%"], (5*(nrow(meanzb.wi):1))+1,
       len = 0, col = "black", lwd = 3)

points(meanzb.wi[,'mean'],
       (5*(nrow(meanzb.wi):1))+1,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)


# Lower panel: leafout

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))

plot(seq(-4, #min(meanz[,'mean']*1.1),
         6, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzl.wi)), 
     seq(1, 5*nrow(meanzl.wi), length.out = nrow(meanzl.wi)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x =-4.75, y = 11, bty="n", legend = "b. Leafout", text.font = 2)
rasterImage(lopng, -4, 0, -2, 7)

axis(2, at = 5*(nrow(meanzl.wi):1), labels = rownames(meanzl.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dx$sp))){
  b.params <- iter.m2nl[!is.na(match(colnames(iter.m2nl), c(paste("b", "[", sp.params, " sp:",
      unique(dx$sp)[i], "]", sep=""))))]

  main.params <- iter.m2nl[!is.na(match(colnames(iter.m2nl), sp.params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzl.wi):1)-1)[params.wsp], sp.est[,"25%"],  jt+(5*(nrow(meanzl.wi):1)-1)[params.wsp],
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzl.wi):1)-1)[params.wsp], #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.lo = rbind(speff.lo, t(sp.est[,1]))
    }

arrows(meanzl.wi[,"75%"], (5*(nrow(meanzl.wi):1))+1, meanzl.wi[,"25%"], (5*(nrow(meanzl.wi):1))+1,
       len = 0, col = "black", lwd = 3)

points(meanzl.wi[,'mean'],
       (5*(nrow(meanzl.wi):1))+1,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)


dev.off();#system(paste("open", file.path(figpath, "Fig1_bb_lo+sp.pdf"), "-a /Applications/Preview.app"))


##############################
### Plotting for m1 models ###
##############################

## Plotting the models (m1.nl, m1.no) with species pooling on chilling and site effects (and their interactions)

## Below gives the main text figure
bbpng <- readPNG(file.path(figpath, "Finn_BB.png")) # Illustrations from Finn et al.
lopng <- readPNG(file.path(figpath, "Finn_LO.png"))
col4fig <- c("mean","sd","25%","50%","75%","Rhat")

sumer.m1no <- summary(m1.no)

# manually to get right order
mu_params <- c("warm20","photo12","chillchill1","chillchill2","siteSH",
               "warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

meanzb <- sumer.m1no[mu_params,col4fig]

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

sumer.m1nl <- summary(m1.nl)
meanzl <- sumer.m1nl[mu_params,col4fig]
rownames(meanzl) <- rownames(meanzb)

pdf(file.path(figpath, "NonBBLO.pdf"), width = 7, height = 8)
par(mfrow=c(2,1), mar = c(2, 10, 5, 1))
  
  # Upper panel: bud burst
  plot(seq(-2.5, 
           2,
           length.out = nrow(meanzb)), 
       1:nrow(meanzb),
       type="n",
       xlab = "",
       ylab = "",
       yaxt = "n")
  
  legend(x =-2.8, y = 3, bty="n", legend = "a. Budburst", text.font = 2)
  # rasterImage(bbpng, -3, 1, -2, 4)
  
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
  arrows(-0.1, 15.5, -2.5, 15.5, len=0.1, col = "black")
  legend(-1, 17, legend="delay", bty="n", text.font = 1, cex=0.75)
  arrows(0.1, 15.5, 2, 15.5, len=0.1, col = "black")
  legend(0.2, 17, legend="advance", bty="n", text.font = 1, cex=0.75)
  legend(-0.25, 16.25, legend="0", bty="n", text.font = 1, cex=0.75)
  par(xpd=FALSE)

  par(mar=c(5, 10, 2, 1))
  # Lower panel: leaf-out
  plot(seq(-2.5, 
           2, 
           length.out = nrow(meanzl)), 
       1:nrow(meanzl),
       type="n",
       xlab = "Model estimate change in budburst or leafout success",
       ylab = "",
       yaxt = "n")
  
  legend(x = -2.8, y = 3, bty="n", legend = "b. Leafout", text.font = 2)
  # rasterImage(lopng, -20, 1, -14, 4)
  
  axis(2, at = nrow(meanzl):1, labels = rownames(meanzl), las = 1, cex.axis = 0.8)
  points(meanzl[,'mean'],
         nrow(meanzl):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzl[,"75%"], nrow(meanzl):1, meanzl[,"25%"], nrow(meanzl):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)

dev.off()



### Supplemental figure -- with species-level estimates shown!

iter.m1no <- as.data.frame(m1.no)
iter.m1nl <- as.data.frame(m1.nl)

# manually to get right order, with intercept
mu_params.wi <- c("(Intercept)", "warm20","photo12","chillchill1","chillchill2",
               "siteSH","warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

meanzb.wi <- sumer.m1no[mu_params.wi,col4fig]

rownames(meanzb.wi) = c("Intercept",
                    "Forcing Temperature",
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

meanzl.wi <- sumer.m1nl[mu_params.wi,col4fig]
rownames(meanzl.wi) <- rownames(meanzb.wi)

speff.bb <- speff.lo <- vector()
params <- c("(Intercept)", "warm20","photo12","chillchill1",
               "chillchill2","siteSH", "warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

sp.params <- c("(Intercept)", "chillchill1","chillchill2","siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

params.wsp <- c(1, 4:6, 14:15)
params.nosp <- c(1:15)[-params.wsp]

pdf(file.path(figpath, "NonBBLO_sp.pdf"), width = 7, height = 8)

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))
# Upper panel: budburst
plot(seq(-4, #min(meanz[,'mean']*1.1),
         5, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb.wi)), 
     seq(1, 5*nrow(meanzb.wi), length.out = nrow(meanzb.wi)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x =-4.75, y = 11, bty="n", legend = "a. Budburst", text.font = 2)
rasterImage(bbpng, -4, 0, -2, 7)

axis(2, at = 5*(nrow(meanzb.wi):1), labels = rownames(meanzb.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dx$sp))){
  b.params <- iter.m1no[!is.na(match(colnames(iter.m1no), c(paste("b", "[", sp.params, " sp:",
      unique(dx$sp)[i], "]", sep=""))))]

  main.params <- iter.m1no[!is.na(match(colnames(iter.m1no), sp.params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp], sp.est[,"25%"],  jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp],
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp], #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.bb = rbind(speff.bb, t(sp.est[,1]))
    }

arrows(meanzb.wi[,"75%"], (5*(nrow(meanzb.wi):1))+1, meanzb.wi[,"25%"], (5*(nrow(meanzb.wi):1))+1,
       len = 0, col = "black", lwd = 3)

points(meanzb.wi[,'mean'],
       (5*(nrow(meanzb.wi):1))+1,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)


# Lower panel: leafout

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))

plot(seq(-4, #min(meanz[,'mean']*1.1),
         5, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzl.wi)), 
     seq(1, 5*nrow(meanzl.wi), length.out = nrow(meanzl.wi)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x =-4.75, y = 11, bty="n", legend = "b. Leafout", text.font = 2)
rasterImage(lopng, -4, 0, -2, 7)

axis(2, at = 5*(nrow(meanzl.wi):1), labels = rownames(meanzl.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dx$sp))){
  b.params <- iter.m1nl[!is.na(match(colnames(iter.m1nl), c(paste("b", "[", sp.params, " sp:",
      unique(dx$sp)[i], "]", sep=""))))]

  main.params <- iter.m1nl[!is.na(match(colnames(iter.m1nl), sp.params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzl.wi):1)-1)[params.wsp], sp.est[,"25%"],  jt+(5*(nrow(meanzl.wi):1)-1)[params.wsp],
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzl.wi):1)-1)[params.wsp], #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.lo = rbind(speff.lo, t(sp.est[,1]))
    }

arrows(meanzl.wi[,"75%"], (5*(nrow(meanzl.wi):1))+1, meanzl.wi[,"25%"], (5*(nrow(meanzl.wi):1))+1,
       len = 0, col = "black", lwd = 3)

points(meanzl.wi[,'mean'],
       (5*(nrow(meanzl.wi):1))+1,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)


dev.off();#system(paste("open", file.path(figpath, "Fig1_bb_lo+sp.pdf"), "-a /Applications/Preview.app"))


#############################################################################
# This does some simple models species by species for Isabelle's 5 species ##
#############################################################################

if(forIsabelle){
    isaspp <- c("POPGRA", "ACESAC", "TILAME", "FAGGRA", "BETALL", "QUERUB")
    isaspp <- sort(isaspp)
    dxisa <- dx[which(dx$sp %in% isaspp),]
    m2.noisa <- stan_glmer(no ~ (warm + photo + chill + site + 
        warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
        (1|sp), family = binomial(link = "logit"), data = dxisa)
    m1.noisa <- stan_glmer(no ~ (warm + photo + chill + site + 
        warm*photo + warm*chill + photo*chill + warm*site + photo*site + chill*site) +
        (chill + chill*site|sp), family = binomial(link = "logit"), data = dxisa) # 3 divergent transitions

    ## summarizing data
    library(plyr)
    library(dplyr)
    nonsummarywtrt <-
      ddply(dxisa, c("warm", "photo", "chill", "sp"), summarise,
      sum.no = sum(no),
      sum.nl = sum(nl),
      total.n = length(no))
    nonsummary <-
      ddply(dxisa, c("sp"), summarise,
      sum.no = sum(no),
      sum.nl = sum(nl),
      total.n = length(no))  

## Plotting the models (m2.nl or m2.nl.bb, AND m2.no) with species pooling on chilling and site effects (and their interactions)

## Below gives the main text figure on LOGIT SCALE
col4fig <- c("mean","sd","25%","50%","75%","Rhat")

sumer.m1.noisa <- summary(m1.noisa)
iter.m1noisa <- as.data.frame(m1.noisa)

# manually to get right order, with intercept
mu_params.wi <- c("(Intercept)", "warm20","photo12","chillchill1","chillchill2",
               "siteSH","warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

meanzb.wi <- sumer.m1.noisa[mu_params.wi,col4fig]

rownames(meanzb.wi) = c("Intercept",
                    "Forcing Temperature",
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


speff.bb <- speff.lo <- vector()
params <- c("(Intercept)", "warm20","photo12","chillchill1",
               "chillchill2","siteSH", "warm20:photo12",
               "warm20:chillchill1","warm20:chillchill2",
               "photo12:chillchill1","photo12:chillchill2",
               "warm20:siteSH", "photo12:siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

sp.params <- c("(Intercept)", "chillchill1","chillchill2","siteSH",
               "chillchill1:siteSH","chillchill2:siteSH")

params.wsp <- c(1, 4:6, 14:15)
params.nosp <- c(1:15)[-params.wsp]

pdf(file.path(figpath, "NonBB_sp_forIsabelle.pdf"), width = 7, height = 8)

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))
# Upper panel: budburst
plot(seq(-4, #min(meanz[,'mean']*1.1),
         5, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb.wi)), 
     seq(1, 5*nrow(meanzb.wi), length.out = nrow(meanzb.wi)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

legend(x =-4.75, y = 11, bty="n", legend = "a. Budburst", text.font = 2)
rasterImage(bbpng, -4, 0, -2, 7)

axis(2, at = 5*(nrow(meanzb.wi):1), labels = rownames(meanzb.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dxisa$sp))){
  b.params <- iter.m1noisa[!is.na(match(colnames(iter.m1noisa), c(paste("b", "[", sp.params, " sp:",
      unique(dxisa$sp)[i], "]", sep=""))))]

  main.params <- iter.m1noisa[!is.na(match(colnames(iter.m1noisa), sp.params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp], sp.est[,"25%"],  jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp],
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzb.wi):1)-1)[params.wsp], #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.bb = rbind(speff.bb, t(sp.est[,1]))
    }

arrows(meanzb.wi[,"75%"], (5*(nrow(meanzb.wi):1))+1, meanzb.wi[,"25%"], (5*(nrow(meanzb.wi):1))+1,
       len = 0, col = "black", lwd = 3)

points(meanzb.wi[,'mean'],
       (5*(nrow(meanzb.wi):1))+1,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)

    dev.off()
}


stop(print("stopping here, below code is original code by Dan Flynn ..."))

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# <> Dan's analyses  ... 
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>


# Analysis of where the non-leafout cuttings were
nlx <- dx[dx$nl == 0,]

summary(nlx)

nl1 <- as.data.frame(table(nlx$sp, nlx$site))

nl2 <- as.data.frame(table(nlx$warm, nlx$photo, nlx$chill))
# proportional to total numbers in each
dl2 <- as.data.frame(table(dx$warm, dx$photo, dx$chill))
nl2$prop <- nl2$Freq/dl2$Freq
nl3 <- as.data.frame(table(nlx$sp, nlx$site,nlx$warm, nlx$photo, nlx$chill))
dl3 <- as.data.frame(table(dx$sp, dx$site, dx$warm, dx$photo, dx$chill))
nl3$prop <- nl3$Freq/dl3$Freq
nl3$prop[is.nan(nl3$prop)==TRUE] <- 0
    
names(nl1) <- c("sp","site","freq")
names(nl2) <- c("warm","photo","chill","freq", "prop")
names(nl3) <- c("sp", "site", "warm","photo","chill","freq", "prop")

nl3.nochill <- subset(nl3, chill=="chill0")
nl3.1chill <- subset(nl3, chill=="chill1")
nl3.2chill <- subset(nl3, chill=="chill2")

#
data.frame(sort(with(nl3, tapply(prop, sp, mean)), T))
with(nl3, tapply(prop, chill, mean))
with(nl3, tapply(prop, site, mean))

# make some simple plots
# # makesimpleplot(nl3, c(0, 0.4), "prop", "% non-leafout") # all chilling combined
# makesimpleplot(nl3.nochill, c(0, 0.4), "prop", "% non-leafout")
# makesimpleplot(nl3.1chill, c(0, 0.4), "prop", "% non-leafout")
# makesimpleplot(nl3.2chill, c(0, 0.4), "prop", "% non-leafout")

sitespp <- as.data.frame(table(nl3$sp, nl3$site))
sitespp <- subset(sitespp, Freq>0)
sppatsites <- aggregate(sitespp["Var2"], sitespp["Var1"], FUN=length)
sppatbothsites <- subset(sppatsites, Var2>1)

spp <- sppatbothsites$Var1
pdf(file="graphs/simpleplots/nonleafouts_byspp.pdf", 10, 6, paper="a4r", onefile=TRUE)
for (i in c(1:length(spp))){
    spdf <- subset(nl3.nochill, sp==spp[i])
    makesimpleplot.sp(spdf, c(0, 1), "prop", "% non-leafout", spp[i])
}
dev.off()


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Simple models by species. All predictors now numeric

dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)
dx$photo <- as.numeric(dx$photo)
dx$warm <- as.numeric(dx$warm)

pdf(file="graphs/simpleplots/nonleafouts_byspp_model.pdf", height = 10, width = 10)

par(cex=0.7, xpd=TRUE, xaxt="n")
layout(matrix(c(1, 2, 3, 3), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

	makesimpleplot.sp(nl3[nl3$sp ==i,], c(0, 1), "prop", "% non-leafout", i)

	# is this species across site and chill?
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) > 1)  {
			mx <- glm(nl ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)

			} 
	# Across site but no chill?		
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) == 1)  {
			mx <- glm(nl ~ warm + photo + site 
							+ warm:photo +  warm:site + photo:site
							+ warm:photo:site 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
			} 
	# One site, no chill?		
	if(length(unique(dx[dx$sp ==i,"site"])) == 1 & length(unique(dx[dx$sp ==i,"chill"])) == 1)  {
		mx <- glm(nl ~ warm + photo +  
							+ warm:photo 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
			} 
					
  	
	textplot(round(coef(summary(mx)),3))
		
		}
		
		
dev.off(); system('open graphs/simpleplots/nonleafouts_byspp_model.pdf -a /Applications/Preview.app')

# examination code
with(dx[dx$sp == i,], table(warm, photo, site, chill))
with(dx[dx$sp == i,], tapply(nl, list(warm, photo, site, chill), mean))

# Repeat, with simple model for all. Aronia: 1 nl occured in each of the four combinations of photo and warm, no separation.
dx$warm <- as.numeric(as.character(dx$warm))
dx$photo <- as.numeric(as.character(dx$photo))

pdf(file="graphs/simpleplots/nonleafouts_byspp_simplemodel.pdf", height = 10, width = 10)

par(cex=0.7, xpd=TRUE, xaxt="n")
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

	makesimpleplot.sp(nl3[nl3$sp ==i,], c(0, 1), "prop", "% non-leafout", i)


		mx <- glm(nl ~ warm + photo  
							+ warm:photo 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
					
  	textplot(round(coef(summary(mx)),3))
	
	if(coef(summary(mx))[4,4] <= 0.05){
	with(dx[dx$sp == i,], interaction.plot(warm, photo, nl)) 
	} else { plot(1:10, type = "n", bty = "n", yaxt="n", xaxt="n",ylab="",xlab="") }

		
	}
			
dev.off(); system('open graphs/simpleplots/nonleafouts_byspp_simplemodel.pdf -a /Applications/Preview.app')

# Focus on site x chill

# Tally sig site * chill effects

pdf(file="graphs/simpleplots/nonleafouts_sitechill.pdf", height = 10, width = 10)

par(cex=0.7)
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){


	# is this species across site and chill?
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) > 1)  {
		xx <- dx[dx$sp==i,]
		
		means <- with(xx, tapply(nl, list(chill, site), mean, na.rm=T))
		sds <- with(xx, tapply(nl, list(chill, site), sd, na.rm=T))

		plot(1:3, means[,1], ylim = c(0, 1.25), main = paste(i, "HF"), pch = 16, ylab = "prop leafout", xaxt="n")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,1]-sds[,1], 1:3, means[,1]+sds[,1], length = 0)

		plot(1:3, means[,2], ylim = c(0, 1.25), main = "SH", pch = 16, ylab = "prop leafout", xaxt="n")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,2]-sds[,2], 1:3, means[,2]+sds[,2], length = 0)

				
			mx <- glm(nl ~ chill * site 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)

		textplot(round(coef(summary(mx)),3))


		if(coef(summary(mx))[4,4] <= 0.05){
			with(dx[dx$sp == i,], interaction.plot(chill, site, nl)) 
			} else { plot(1:10, type = "n", bty = "n", yaxt="n", xaxt="n",ylab="",xlab="") }

		 		
	
		}
		}
		
		
dev.off(); system('open graphs/simpleplots/nonleafouts_sitechill.pdf -a /Applications/Preview.app')


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Across species
	m1 <- glm(nl ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							, family=binomial(link='logit'), data = dx
							)
	summary(m1)						

# across species, with partial pooling
library(lme4)
m2 <- glmer(nl ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							+ (1|sp),
							, family=binomial(link='logit'), data = dx
							)
library(sjPlot)

sjp.glmer(m2)
sjp.glmer(m2, type = "fe")

# Basically, we can't say much about nonleafouts. Let's shift to leafouts

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Across species
	l1 <- lm(lday ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							, data = dx[dx$nl==1,]
							)
	summary(l1)						
	summary.aov(l1)						
# With partial pooling

l2 <- lmer(lday ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							+ (1|sp)
							, data = dx[dx$nl==1,]
							)
	summary(l2)						
sjp.lmer(l2)						
sjp.lmer(l2, type = "fe")						
interact

# Within individual species, should match nl story


pdf(file="graphs/leafoutday_byspp_simplemodel.pdf", height = 10, width = 10)

par(cex=0.7, xpd=TRUE, xaxt="n")
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

		xx <- dx[dx$sp==i,]
		
		means <- with(xx, tapply(lday, list(warm, photo, site), mean, na.rm=T))
		sds <- with(xx, tapply(lday, list(warm, photo, site), sd, na.rm=T))

		plot(1:3, means[,1], ylim = c(0, 1.25), main = paste(i, "HF"), pch = 16, ylab = "prop leafout", xaxt="n")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,1]-sds[,1], 1:3, means[,1]+sds[,1], length = 0)

		plot(1:3, means[,2], ylim = c(0, 1.25), main = "SH", pch = 16, ylab = "prop leafout", xaxt="n")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,2]-sds[,2], 1:3, means[,2]+sds[,2], length = 0)


		mx <- glm(nl ~ warm + photo  
							+ warm:photo 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
					
  	textplot(round(coef(summary(mx)),3))
	
	if(coef(summary(mx))[4,4] <= 0.05){
	with(dx[dx$sp == i,], interaction.plot(warm, photo, nl)) 
	} else { plot(1:10, type = "n", bty = "n", yaxt="n", xaxt="n",ylab="",xlab="") }

		
	}
			
dev.off(); system('open graphs/simpleplots/nonleafouts_byspp_simplemodel.pdf -a /Applications/Preview.app')



# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# colors for plotting
cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)

bb <- barplot(nl2$prop*100, ylab = "% of cuttings non-leafout", 
	ylim = c(0,50), 
	col = rep(cols, each = 4),
	main = "Percent of cuttings in each treatment which failed to leaf out",
	xlab = "Treatment combination", sub = "(temp + photo + chill)")
mtext(paste(nl2$warm, nl2$photo, nl2$chill, sep = "\n"), 1, line = -2, padj = 0, at = bb[,1])

chi.all <- summary(xtabs(~ warm+photo+chill, data = nl))
for(i in levels(nl$chill)) print(chisq.test(table(nl[nl$chill==i,]$warm, nl[nl$chill==i,]$photo))) # NS within chill1 or chill2, surprisingly

xx <- round(chi.all$statistic,2)
text(1, 45, substitute(paste(chi^2,"=", xx), list(xx=round(chi.all$statistic,2))))
text(1, 42, substitute(paste(p,"=", xx), list(xx=round(chi.all$p.value,4))))
dev.print(file = "graphs/nonleafout.pdf", device = pdf, width = 10, height = 6); system('open ./graphs/nonleafout.pdf -a /Applications/Preview.app')

# looping this same figure now for each species separately.

pdf(file="graphs/nonleafout_eachsp.pdf", width = 10, height = 6)
for(i in unique(nl$sp)){

	nlx <- with(nl[nl$sp==i,], as.data.frame(table(warm, photo, chill)))
	# proportional to total numbers in each
	dlx <- with(dx[dx$sp==i,], as.data.frame(table(warm, photo, chill)))
	nlx$prop <- nlx$Freq/dlx$Freq
	
	bb <- barplot(nlx$prop*100, ylab = "% of cuttings non-leafout", 
		ylim = c(0,100), 
		col = rep(cols, each = 4),
		main = paste(i, "\n Percent of cuttings in each treatment which failed to leaf out"),
		xlab = "Treatment combination")
	mtext(paste(nlx$warm, nlx$photo, nlx$chill, sep = "\n"), 1, line = 2, padj = 0, at = bb[,1])
	
	if(length(unique(as.character(nl[nl$sp==i,"chill"])))==1){
		chi.all <- summary(xtabs(~ warm+photo, data = nl[nl$sp==i,]))	
		}
	else {	chi.all <- summary(xtabs(~ warm+photo+chill, data = nl[nl$sp==i,]))	 }
	xx <- round(chi.all$statistic,2)
	text(1, 95, substitute(paste(chi^2,"=", xx, ", df =", df), list(xx=round(chi.all$statistic,2), df = chi.all$parameter)))
	text(1, 85, substitute(paste(p,"=", xx), list(xx=round(chi.all$p.value,4))))

}

dev.off(); system('open ./graphs/nonleafout_eachsp.pdf -a /Applications/Preview.app')


# also by species and site

nl1 <- as.data.frame(table(nl$sp, nl$site))
# proportional to total numbers in each
dl1 <- as.data.frame(table(dx$sp, dx$site))
nl1$prop <- nl1$Freq/dl1$Freq


bb <- barplot(height = rbind(nl1$prop[1:28]*100, nl1$prop[29:56]*100),
	beside = T,
	legend.text = c("HF","SH"),
	args.legend = list(bty="n"),
	space = c(0.05, 1),
	ylab = "% of cuttings non-leafout", 
	ylim = c(0,100), 
	main = "Percent of cuttings in each species which failed to leaf out"
	)
par(xpd=T)
text(bb[1,], -3, nl1$Var1[1:28], srt = 45, adj = 1, cex = 0.8)

dev.print(file = "graphs/nonleafout_species.pdf", device = pdf, width = 10, height = 5); system('open ./graphs/nonleafout_species.pdf -a /Applications/Preview.app')

# analyze now in logistic framework
dx$nl <- as.numeric(as.character(cut(dx$lday, breaks = c(0, 74, 100), labels = c(1, 0)))) # 1: leafed out. 0: failed to leaf out

summary(m1 <- glm(nl ~ warm + photo + chill + site, family=binomial(link='logit'), data = dx)) # overall strong effects of warming, long day, and chilling

summary(m1 <- glm(nl ~ as.numeric(warm), family=binomial(link='logit'), data = dx)) # just warming for plotting
plot(as.numeric(dx$warm), dx$nl, xlab="Temperature", 
     ylab="Probability of Response")
curve(predict(m1, data.frame(warm=x), type="resp"), 
      add=TRUE, col="red")

summary(glm(nl ~ warm + photo + chill + site + sp, family=binomial(link='logit'), data = dx))

summary(glm(nl ~ warm + photo + chill + site + sp +
						warm:photo + warm:chill + photo:chill,
						family=binomial(link='logit'), data = dx))

summary(glm(nl ~ site + sp + sp:site, family=binomial(link='logit'), data = dx)) # no overall site effect, some acesac and franig interax by site, more non-leafouts in HF

summary(glm(nl ~ warm + photo + chill + site + sp +
						warm:photo + warm:chill + photo:chill + 
						warm:sp + photo:sp + chill:sp,
						family=binomial(link='logit'), data = dx))	# clear species effects, interax with warm x photo, very few sp-specific responses to warming or photo. Querub improved with chilling.					


