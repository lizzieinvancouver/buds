## Started by Lizzie ##
## 30 January 2018 ##

## Additional analyses for revisions ##
## Includes correlated cues analysis ##
## Built in part off Pheno Burburst analysis.R ##

rm(list=ls())

forlatex = FALSE # set to FALSE if just trying new figures, TRUE if outputting for final
runstan = FALSE # running the models reduced by species with low bb and leafout

library(rstan) # You need this to extract samples
# library(rstanarm)
setwd("~/Documents/git/projects/treegarden/budexperiments/analyses")
library(reshape2) # Using instead of tidyr since tidyr overwrite extract command in stan!
source('source/plotletfx.R') # this one shows 50% credible intervals for the bars

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# get latest .Rdata file
# To run from saved stan output (exclude Fake data output)
realout <- dir()[grep("Stan Output", dir())[is.na(match(grep("Stan Output", dir()), grep("Fake", dir())))]]
  if(!exists("doym.b")) load(sort(realout, T)[1]) # only run if stan output file is not yet in working memory.
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

# add in budburst success
dx$no <- dx$bday
dx$no[dx$no>0] <- 1
dx$no[is.na(dx$no)==TRUE] <- 0

dxb <- dx[!is.na(dx$bday),]
dxl <- dx[!is.na(dx$lday),]

bdaymean <- t(with(dxb, tapply(bday, list(site, sp), mean, na.rm=T)))
ldaymean <- t(with(dxl, tapply(lday, list(site, sp), mean, na.rm=T)))

# Get means by treatment/site/species for OSPREE
hfdat.bdaymean <- subset(dxb, site=="1")
shdat.bdaymean <- subset(dxb, site=="2")
bdaymean.hf <- t(with(hfdat.bdaymean, tapply(bday, list(treatcode, sp), mean, na.rm=T)))
bdaymean.sh <- t(with(shdat.bdaymean, tapply(bday, list(treatcode, sp), mean, na.rm=T)))

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
sumerb <- summary(doym.b)$summary
sumerb[grep("mu_", rownames(sumerb)),]

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

# 2. Leafout day. 
sumerl <- summary(doym.l)$summary
sumerl[grep("mu_", rownames(sumerl)),]

# Chill
gotchill <- tapply(dx$spn, dx$chill2, unique)$'1'
nochill <- unique(dx$spn)[is.na(match(unique(dx$spn), gotchill))]
sumerb[!is.na(match(rownames(sumerb), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_chill2[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill2[", nochill, "]", sep=""))),] = NA


################
# Are cues correlated?
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

# all correlated
summary(lm(bb.warm~bb.photo, data=df.meaneffs))
summary(lm(bb.warm~bb.chill1, data=df.meaneffs))
summary(lm(lo.warm~lo.photo, data=df.meaneffs))
summary(lm(lo.warm~lo.chill1, data=df.meaneffs))

################
# Extract samples
################
sample.b <- extract(doym.b)
sample.l <- extract(doym.l)
dim(sample.l$b_chill2)

sample.bb.warm <- melt(sample.b$b_warm)
sample.bb.photo <- melt(sample.b$b_photo)
sample.bb.chill1 <- melt(sample.b$b_chill1)

names(sample.bb.warm) <- c("iter_warm", "sp_warm", "value_warm")
names(sample.bb.photo) <- c("iter_photo", "sp_photo", "value_photo")
names(sample.bb.chill1) <- c("iter_chill1", "sp_chill1", "value_chill1")

sample.lo.warm <- melt(sample.l$b_warm)
sample.lo.photo <- melt(sample.l$b_photo)
sample.lo.chill1 <- melt(sample.l$b_chill1)

names(sample.lo.warm) <- c("iter_warm", "sp_warm", "value_warm")
names(sample.lo.photo) <- c("iter_photo", "sp_photo", "value_photo")
names(sample.lo.chill1) <- c("iter_chill1", "sp_chill1", "value_chill1")

## Just work withe last 1000 iterations
repz <- 1000
totaln <- 15988
bb.df <- cbind(sample.bb.warm, sample.bb.photo, sample.bb.chill1)
bb.df1K <- subset(bb.df, iter_warm>totaln-repz)

lo.df <- cbind(sample.lo.warm, sample.lo.photo, sample.lo.chill1)
lo.df1K <- subset(lo.df, iter_warm>totaln-repz)

## Check your extraction work ... ##
library(plyr)
library(dplyr)
quickcheck.bb <-
      ddply(bb.df1K, c("sp_photo"), summarise,
      mean.warm = mean(value_warm),
      mean.photo = mean(value_photo))

quickcheck.lo <-
      ddply(lo.df1K, c("sp_photo"), summarise,
      mean.warm = mean(value_warm),
      mean.photo = mean(value_photo))
# checking done

## Fit a line to each iteration...
# 1. budburst
bb.post.lms <- data.frame(int.wp=rep(NA,repz), slope.wp=rep(NA,repz),
    int.cp=rep(NA,repz), slope.cp=rep(NA,repz))
for(i in c(1:repz)){
    bb.post.lms$int.wp[i] <- coef(lm(value_warm~value_photo,
       data=subset(bb.df1K, iter_warm==(i+(totaln-repz)))))[1]
    bb.post.lms$slope.wp[i] <- coef(lm(value_warm~value_photo,
       data=subset(bb.df1K, iter_warm==(i+(totaln-repz)))))[2]
    bb.post.lms$int.cp[i] <- coef(lm(value_chill1~value_photo,
       data=subset(bb.df1K, iter_warm==(i+(totaln-repz)))))[1]
    bb.post.lms$slope.cp[i] <- coef(lm(value_chill1~value_photo,
       data=subset(bb.df1K, iter_warm==(i+(totaln-repz)))))[2]
    bb.post.lms$int.wc[i] <- coef(lm(value_warm~value_chill1,
       data=subset(bb.df1K, iter_warm==(i+(totaln-repz)))))[1]
    bb.post.lms$slope.wc[i] <- coef(lm(value_warm~value_chill1,
       data=subset(bb.df1K, iter_warm==(i+(totaln-repz)))))[2]
    }

# 2. leafout
lo.post.lms <- data.frame(int.wp=rep(NA,repz), slope.wp=rep(NA,repz),
    int.cp=rep(NA,repz), slope.cp=rep(NA,repz))
for(i in c(1:repz)){
    lo.post.lms$int.wp[i] <- coef(lm(value_warm~value_photo,
       data=subset(lo.df1K, iter_warm==(i+(totaln-repz)))))[1]
    lo.post.lms$slope.wp[i] <- coef(lm(value_warm~value_photo,
       data=subset(lo.df1K, iter_warm==(i+(totaln-repz)))))[2]
    lo.post.lms$int.cp[i] <- coef(lm(value_chill1~value_photo,
       data=subset(lo.df1K, iter_warm==(i+(totaln-repz)))))[1]
    lo.post.lms$slope.cp[i] <- coef(lm(value_chill1~value_photo,
       data=subset(lo.df1K, iter_warm==(i+(totaln-repz)))))[2]
    lo.post.lms$int.wc[i] <- coef(lm(value_warm~value_chill1,
       data=subset(lo.df1K, iter_warm==(i+(totaln-repz)))))[1]
    lo.post.lms$slope.wc[i] <- coef(lm(value_warm~value_chill1,
       data=subset(lo.df1K, iter_warm==(i+(totaln-repz)))))[2]
    }

## Now get the quantiles
# 1. burburst
bb.wp.quant.m <- quantile(bb.post.lms$slope.wp, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
bb.wc.quant.m <- quantile(bb.post.lms$slope.wc, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
bb.wp.quant.int <- quantile(bb.post.lms$int.wp, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
bb.wc.quant.int <- quantile(bb.post.lms$int.wc, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
# 2. leafout
lo.wp.quant.m <- quantile(lo.post.lms$slope.wp, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
lo.wc.quant.m <- quantile(lo.post.lms$slope.wc, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
lo.wp.quant.int <- quantile(lo.post.lms$int.wp, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
lo.wc.quant.int <- quantile(lo.post.lms$int.wc, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
# Intercepts
bb.wp.quant.m
bb.wc.quant.m
lo.wp.quant.m
lo.wc.quant.m
# And here are the means
mean(bb.post.lms$slope.wp)
mean(bb.post.lms$slope.wc)
mean(lo.post.lms$slope.wp)
mean(lo.post.lms$slope.wc)

## Plot the results to be sure ...
par(mfrow=c(2,2))
plot(bb.warm~bb.photo, data=df.meaneffs)
abline(bb.wp.quant.int[3], bb.wp.quant.m[3], lwd=2)
abline(bb.wp.quant.int[2], bb.wp.quant.m[2], lty=2)
abline(bb.wp.quant.int[4], bb.wp.quant.m[4], lty=2)

plot(lo.warm~lo.photo, data=df.meaneffs)
abline(lo.wp.quant.int[3], lo.wp.quant.m[3], lwd=2)
abline(lo.wp.quant.int[2], lo.wp.quant.m[2], lty=2)
abline(lo.wp.quant.int[4], lo.wp.quant.m[4], lty=2)

plot(bb.warm~bb.chill1, data=df.meaneffs)
abline(bb.wc.quant.int[3], bb.wc.quant.m[3], lwd=2)
abline(bb.wc.quant.int[2], bb.wc.quant.m[2], lty=2)
abline(bb.wc.quant.int[4], bb.wc.quant.m[4], lty=2)

plot(lo.warm~lo.chill1, data=df.meaneffs)
abline(lo.wc.quant.int[3], lo.wc.quant.m[3], lwd=2)
abline(lo.wc.quant.int[2], lo.wc.quant.m[2], lty=2)
abline(lo.wc.quant.int[4], lo.wc.quant.m[4], lty=2)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Running the models with some of the non-leafout spp removed
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# From Pheno Budburst analysis.R the loser 5 spp which are:
# all those with BB success below 80% (4 spp), see bblo.success.bbordered
# all those with LO below 75% (4 spp), see bblo.success.loordered

sppwithlowbblo <- c("KALANG", "ACESAC", "FAGGRA", "QUEALB", "VIBLAN")

dxb.hibb <- dxb[which(!dxb$sp %in% sppwithlowbblo),]
dxl.hilo <- dxl[which(!dxl$sp %in% sppwithlowbblo),]

if(runstan){

# 1. Budburst day. 
datalist.hibb <- list(lday = dxb.hibb$bday, # bud burst as response 
                     warm = as.numeric(dxb.hibb$warm), 
                     site = as.numeric(dxb.hibb$site), 
                     sp = as.numeric(as.factor(as.character(dxb.hibb$sp))),
                     photo = as.numeric(dxb.hibb$photo), 
                     chill1 = as.numeric(dxb.hibb$chill1),
                     chill2 = as.numeric(dxb.hibb$chill2),
                     N = nrow(dxb.hibb), 
                     n_site = length(unique(dxb.hibb$site)), 
                     n_sp = length(unique(dxb.hibb$sp))
)
  
doym.hibb <- stan('stan/lday_site_sp_chill_inter_poola_ncp.stan', 
                 data = datalist.hibb, warmup=4000, iter = 7997, chains = 4,
                 control = list(adapt_delta = 0.9))
                 #               , max_treedepth = 15)) 

save(doym.hibb, file="stan/output/lday_site_sp_chill_inter_poola_ncp_doy.hibb.Rda")

# Leafout 
datalist.hilo <- list(lday = dxl.hilo$lday, # leaf-out as respose 
                     warm = as.numeric(dxl.hilo$warm), 
                     site = as.numeric(dxl.hilo$site), 
                     sp = as.numeric(as.factor(as.character(dxl.hilo$sp))), 
                     photo = as.numeric(dxl.hilo$photo), 
                     chill1 = as.numeric(dxl.hilo$chill1),
                     chill2 = as.numeric(dxl.hilo$chill2),
                     N = nrow(dxl.hilo), 
                     n_site = length(unique(dxl.hilo$site)), 
                     n_sp = length(unique(dxl.hilo$sp))
  )
  
doym.hilo <- stan('stan/lday_site_sp_chill_inter_poola_ncp.stan',
                data = datalist.hilo, warmup=4000, iter = 7997, chains = 4,
                control = list(adapt_delta = 0.95))
                #               ,max_treedepth = 15))

save(doym.hilo, file="stan/output/lday_site_sp_chill_inter_poola_ncp_doy.hilo.Rda")
}

if(!runstan){
load("stan/output/lday_site_sp_chill_inter_poola_ncp_doy.hibb.Rda")
load("stan/output/lday_site_sp_chill_inter_poola_ncp_doy.hilo.Rda")
    }

# Format the stan output
# 1. Budburst
sumerhibb <- summary(doym.hibb)$summary
sumerhibb[grep("mu_", rownames(sumerhibb)),]
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

meanzb.hibb <- sumerhibb[mu_params,col4fig]

rownames(meanzb.hibb) = c("Forcing Temperature",
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



meanzb.hibb.table <- sumerhibb[mu_params,col4table]
row.names(meanzb.hibb.table) <- row.names(meanzb.hibb)

# 2. Leafout
sumerhilo <- summary(doym.hilo)$summary
sumerhilo[grep("mu_", rownames(sumerhilo)),]

meanzl.hilo <- sumerhilo[mu_params,col4fig]
meanzl.hilo.table <- sumerhilo[mu_params,col4table]

rownames(meanzl.hilo) = rownames(meanzb.hibb)
rownames(meanzl.hilo.table) = rownames(meanzb.hibb.table)

## Compare to models using all spp
meanzb <- sumerb[mu_params,col4fig]
meanzl <- sumerl[mu_params,col4fig]

bbdiff <- data.frame(main.m=meanzb[,1], red.m=meanzb.hibb[,1])
bbdiff$diff <- bbdiff$main.m-bbdiff$red.m
bbdiff$diffper <- bbdiff$diff/bbdiff$main.m
bbdiff[order(bbdiff$diff),]
bbdiff$diffper*100

lodiff <- data.frame(main.m=meanzl[,1], red.m=meanzl.hilo[,1])
lodiff$diff <- lodiff$main.m-lodiff$red.m
lodiff$diffper <- lodiff$diff/lodiff$main.m
lodiff[order(lodiff$diff),]
lodiff$diffper*100

# What is the percent success of reduced models?
dxall.hi <- dx[which(!dx$sp %in% sppwithlowbblo),]

sum(dxall.hi$no)/nrow(dxall.hi)
sum(dxall.hi$nl)/nrow(dxall.hi)
