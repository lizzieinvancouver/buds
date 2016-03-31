dx <- dx[!is.na(dx$bday) & !is.na(dx$lday),]

datalist.b <- list(lday = dx$bday, # leafout as respose 
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


doym.1b <- stan('stan/lday_nosite_plusspint.stan', data = datalist.b, iter = 4000, chains = 4,
               control= list(adapt_delta = 0.90)) 

savestan("Chill Cont highdelt")

# no site effect at all, chill as categorical
datalist.l <- list(lday = dx$lday, # leafout as respose 
                   warm = as.numeric(dx$warm), 
                   site = as.numeric(dx$site), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill1 = as.numeric(dx$chill1),
                   chill2 = as.numeric(dx$chill2),
                   chill3 = as.numeric(dx$chill3),
                   N = nrow(dx), 
                   n_site = length(unique(dx$site)), 
                   n_sp = length(unique(dx$sp))
)

doym.2 <- stan('stan/lday_nosite_chill.stan', data = datalist.l, iter = 4000, chains = 4,
               control= list(adapt_delta = 0.90))  

savestan("No Site Chill Cat highdelt")

# Site effect, chill as categorical #-- this was run on 2016-03-25
dx <- dx[!is.na(dx$bday) & !is.na(dx$lday),]
datalist.l <- list(lday = dx$lday, # leafout as respose
                   warm = as.numeric(dx$warm),
                   site = as.numeric(dx$site),
                   sp = as.numeric(dx$sp),
                   photo = as.numeric(dx$photo),
                   chill1 = as.numeric(dx$chill1),
                   chill2 = as.numeric(dx$chill2),
                #   chill3 = as.numeric(dx$chill3),
                   N = nrow(dx),
                   n_site = length(unique(dx$site)),
                   n_sp = length(unique(dx$sp))
)

doym.3 <- stan('stan/lday_nosite_plusspint_chill.stan', data = datalist.l, iter = 4000, chains = 4,
               control= list(adapt_delta = 0.90))  


savestan("Site Fixed Chill Cat highdelt ")



doym.4l <- stan('stan/lday_site_sp_chill.stan', data = datalist.l, iter = 4000, chains = 4) 

savestan("Chill Site Sp ")

doym.4l <- stan('stan/lday_site_chill.stan', data = datalist.l, iter = 4000, chains = 4) 

savestan("Chill Site ")

sumerl <- summary(doym.4l)$summary

sumerl[grep("mu_", rownames(sumerl)),]

xtable(sumerl[grep("mu_", rownames(sumerl)),c(1,2,3,10)]) 


# load from saved version

load("Stan Output Chill Site 2016-03-30.RData")

launch_shinystan(doym.4l)

# plotting chill effects and new warm x photo effects

pdf("WarmxPhotoxChill.pdf", width = 6, height = 15)

par(mfrow = c(3, 1))

plot(
  sumerl[grep("b_warm\\[", rownames(sumerl)),1],
  sumerl[grep("b_photo\\[", rownames(sumerl)),1],
  pch = "+",
  xlim = c(-20, 1),
  ylim = c(-20, 1),
  ylab = "Advance due to long photoperiod",
  xlab = "Advance due to warm temperature"
)

abline(h=0, lty = 3, col = "grey60")
abline(v=0, lty = 3, col = "grey60")

arrows(
  sumerl[grep("b_warm\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_warm\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"75%"],
  length = 0, col = alpha("grey50",0.5))

arrows(
  sumerl[grep("b_warm\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_warm\\[", rownames(sumerl)),"75%"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"mean"],
  length = 0, col = alpha("grey50",0.5))


# match with species names
text( sumerl[grep("b_warm\\[", rownames(sumerl)),1],
      sumerl[grep("b_photo\\[", rownames(sumerl)),1],
      sort(unique(dx$sp)),
      cex = 0.5, 
      pos = 3)



### Chill1 by warm


plot(
  sumerl[grep("b_warm\\[", rownames(sumerl)),1],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),1],
  pch = "+",
  xlim = c(-20, 1),
  ylim = c(-20, 1),
  ylab = "Advance due to chilling at 4° C",
  xlab = "Advance due to warm temperature"
)

abline(h=0, lty = 3, col = "grey60")
abline(v=0, lty = 3, col = "grey60")

arrows(
  sumerl[grep("b_warm\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_warm\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"75%"],
  length = 0, col = alpha("grey50",0.5))

arrows(
  sumerl[grep("b_warm\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_warm\\[", rownames(sumerl)),"75%"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"mean"],
  length = 0, col = alpha("grey50",0.5))


# match with species names
text( sumerl[grep("b_warm\\[", rownames(sumerl)),1],
      sumerl[grep("b_chill1\\[", rownames(sumerl)),1],
      sort(unique(dx$sp)),
      cex = 0.5, 
      pos = 3)

# Chill 2 by warm

plot(
  sumerl[grep("b_warm\\[", rownames(sumerl)),1],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),1],
  pch = "+",
  xlim = c(-20, 1),
  ylim = c(-20, 1),
  ylab = "Advance due to chilling at 1.5° C",
  xlab = "Advance due to warm temperature"
)

abline(h=0, lty = 3, col = "grey60")
abline(v=0, lty = 3, col = "grey60")

arrows(
  sumerl[grep("b_warm\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_warm\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"75%"],
  length = 0, col = alpha("grey50",0.5))

arrows(
  sumerl[grep("b_warm\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_warm\\[", rownames(sumerl)),"75%"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"mean"],
  length = 0, col = alpha("grey50",0.5))


# match with species names
text( sumerl[grep("b_warm\\[", rownames(sumerl)),1],
      sumerl[grep("b_chill2\\[", rownames(sumerl)),1],
      sort(unique(dx$sp)),
      cex = 0.5, 
      pos = 3)


# Chill by photo

plot(
  sumerl[grep("b_photo\\[", rownames(sumerl)),1],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),1],
  pch = "+",
  xlim = c(-20, 1),
  ylim = c(-20, 1),
  ylab = "Advance due to chilling at 4° C",
  xlab = "Advance due to long photoperiod"
)

abline(h=0, lty = 3, col = "grey60")
abline(v=0, lty = 3, col = "grey60")

arrows(
  sumerl[grep("b_photo\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"75%"],
  length = 0, col = alpha("grey50",0.5))

arrows(
  sumerl[grep("b_photo\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"75%"],
  sumerl[grep("b_chill1\\[", rownames(sumerl)),"mean"],
  length = 0, col = alpha("grey50",0.5))


# match with species names
text( sumerl[grep("b_photo\\[", rownames(sumerl)),1],
      sumerl[grep("b_chill1\\[", rownames(sumerl)),1],
      sort(unique(dx$sp)),
      cex = 0.5, 
      pos = 3)

# Chill 2 by photo

plot(
  sumerl[grep("b_photo\\[", rownames(sumerl)),1],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),1],
  pch = "+",
  xlim = c(-20, 1),
  ylim = c(-20, 1),
  ylab = "Advance due to chilling at 1.5° C",
  xlab = "Advance due to long photoperiod"
)

abline(h=0, lty = 3, col = "grey60")
abline(v=0, lty = 3, col = "grey60")

arrows(
  sumerl[grep("b_photo\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"75%"],
  length = 0, col = alpha("grey50",0.5))

arrows(
  sumerl[grep("b_photo\\[", rownames(sumerl)),"25%"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"mean"],
  sumerl[grep("b_photo\\[", rownames(sumerl)),"75%"],
  sumerl[grep("b_chill2\\[", rownames(sumerl)),"mean"],
  length = 0, col = alpha("grey50",0.5))


# match with species names
text( sumerl[grep("b_photo\\[", rownames(sumerl)),1],
      sumerl[grep("b_chill2\\[", rownames(sumerl)),1],
      sort(unique(dx$sp)),
      cex = 0.5, 
      pos = 3)


dev.off()
system("open 'WarmxPhotoxChill.pdf' -a/Applications/Preview.app")

##################

# rm(list=ls())


load("Stan Output Chill Cont highdelt2016-03-28.RData")
mchillcont <- doym.1 


launch_shinystan(mchillcont)

pairs(mchillcont, pars = names(mchillcont)[grep("mu_", names(mchillcont))])



load("Stan Output No Site Chill Cat2016-03-28.RData")
mchillcat <- doym.2 

pairs(mchillcat, pars = c(names(mchillcat)[grep("mu_", names(mchillcat))], "lp__"))

launch_shinystan(mchillcat)


load("Stan Output Site Fixed Chill Cat highdelt 2016-03-28.RData")
mchillcatsite <- doym.3

pairs(mchillcatsite, pars = c(names(mchillcatsite)[grep("mu_", names(mchillcatsite))], "lp__"))

launch_shinystan(mchillcatsite)

## notes from Stanleyi meeting 2016-03-21




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

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> 

print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))


dx$warmn = scale(as.numeric(as.character(dx$warm)))
dx$photon = scale(as.numeric(as.character(dx$photo)))
dx$chilln = scale(as.numeric(substr(as.character(dx$chill), 6, 6)))
dx$spn <- as.numeric(dx$sp)

levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)


### looking at a real species
with(dx[dx$sp == "VIBLAN",],
     table(warm, photo, chill)
)

vx <- dx[dx$sp == "VIBLAN",]

tapply(vx$lday, list(vx$warm, vx$photo, vx$chill), mean,na.rm=T)


tapply(vx$lday, list(vx$warm, vx$photo, vx$chill), function(x) length(x[!is.na(x)]))

mean(vx$lday,na.rm=T)

# 2016-03-30

# 1. site as an effect (b_site)
# 2. sp as the intercept
# 3. interactions between:
# warm x photo
# warm x chill1
# warm x chill2
# photo x chill1
# photo x chill2
# site x warm
# site x photo
# site x chill1
# site x chill2
# 4. Color code trees and shrubs in tradeoff plots.

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


doym.l.inter <- stan('stan/lday_site_sp_chill_inter.stan', data = datalist.l, iter = 4000, chains = 4) 
  


######### scratch...

# 
# sumdf <- data.frame(
# b_warm = sumerl[grep("b_warm\\[", rownames(sumerl)),1],
# b_photo = sumerl[grep("b_photo\\[", rownames(sumerl)),1],
# b_site = sumerl[grep("b_site\\[", rownames(sumerl)),1],
# b_chill1 = sumerl[grep("b_chill1\\[", rownames(sumerl)),1],
# b_chill2 = sumerl[grep("b_chill2\\[", rownames(sumerl)),1],
# b_inter_wp = sumerl[grep("b_inter_wp\\[", rownames(sumerl)),1],
# b_inter_ws = sumerl[grep("b_inter_ws\\[", rownames(sumerl)),1],
# b_inter_ps = sumerl[grep("b_inter_ps\\[", rownames(sumerl)),1],
# b_inter_wc1 = sumerl[grep("b_inter_wc1\\[", rownames(sumerl)),1],
# b_inter_wc2 = sumerl[grep("b_inter_wc2\\[", rownames(sumerl)),1],
# b_inter_pc1 = sumerl[grep("b_inter_pc1\\[", rownames(sumerl)),1],
# b_inter_pc2 = sumerl[grep("b_inter_pc2\\[", rownames(sumerl)),1],
# b_inter_sc1 = sumerl[grep("b_inter_sc1\\[", rownames(sumerl)),1],
# b_inter_sc2 = sumerl[grep("b_inter_sc2\\[", rownames(sumerl)),1]
# ); rownames(sumdf) = levels(dx$sp)

# seeing if easier to do in ggplot..
# ggplot(sumdf,
#        aes(b_warm, b_photo)) +
#   xlab("Advance due to 10° warming") + 
#   ylab("Advance due to 4 hr photoperiod") +
#   xlim(-28, -11) +
#   ylim(-28, -11) +
#   geom_point() + theme_bw()
