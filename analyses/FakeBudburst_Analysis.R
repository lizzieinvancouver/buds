## Started March 2016 ##
## By Dan Flynn ##
## Updates by Lizzie in 2017 ##

# Fake data testing of pheno budburst experiment 2015

library(rstan)
library(xtable)
library(ggplot2)
library(shinystan)

# setwd("~/Documents/git/buds/analyses")
setwd("~/Documents/git/projects/treegarden/budexperiments/analyses")

source('stan/savestan.R')
source('source/plotlet.R')
# get latest .Rdata file

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("Fake Budburst.RData")

# load("Stan Output 2016-04-04 Fake Interax.RData")
# fakeout <- dir()[grep("Stan Output", dir())[is.na(match(grep("Stan Output", dir()), grep("Fake", dir())))]]
# load(sort(realout, T)[1])
# ls() 

# <><><><><> Unpooled intercepts (Dan's original model) <><><><> #

# To Stan!
datalist.f <- list(lday = fake$bb, # budburst as respose 
                   warm = as.numeric(fake$warm), 
                   site = as.numeric(fake$site), 
                   sp = as.numeric(fake$sp), 
                   photo = as.numeric(fake$photo), 
                   chill1 = as.numeric(fake$chill1), 
                   chill2 = as.numeric(fake$chill2), 
                   N = nrow(fake), 
                   n_site = length(unique(fake$site)), 
                   n_sp = length(unique(fake$sp))
                  )

# lday_site_chill: < 120 seconds per chain, very fast
# lday_site_sp_chill: much slower.   
# doym.f <- stan('stan/lday0.stan', data = datalist.f, iter = 4000, chains = 4) 

sumer <- summary(doym.f)$summary
sumer[grep("mu_", rownames(sumer)),] # effects are perfectly captured now.

# patience for the below, it's giant
# pairs(doym.f, pars = names(doym.f)[grep("mu_", names(doym.f))])

ssm.f <- as.shinystan(doym.f)
launch_shinystan(ssm.f) 

save(doym.f, file="stan/lday_site_sp_chill_inter.Rda")

# <><><><><><><><><> #



# <><><><><> Pooled intercepts <><><><> #
doym.f <- stan('stan/lday_site_sp_chill_inter.stan', data = datalist.f, 
               iter = 2004) 
             #  control = list(adapt_delta = 0.9,
             #                 max_treedepth = 15))

# To Stan! Pooled intercepts!
doym.fpoola <- stan('stan/lday_site_sp_chill_inter_poola.stan', data = datalist.f, 
               iter = 3000)
              # control = list(adapt_delta = 0.9,
              #                max_treedepth = 15))

doym.fpoola.ncp <- stan('stan/lday_site_sp_chill_inter_poola_ncp.stan', data = datalist.f, 
               iter = 1000)

doym.fpoola.ncp <- stan('stan/lday_site_sp_chill_inter_poola_ncpfull.stan', data = datalist.f, 
               iter = 1000)

sf.poola <- summary(doym.fpoola)$summary
sf.poola[grep("mu_", rownames(sf.poola)),]

# sanity checks
summary(lm(bb ~ (site+warm+photo+chill1+chill2)^2, data = fake))
# library(lme4)
# summary(lmer(bb ~ (site+warm+photo+chill1+chill2)^2 + (1|sp), data = fake))



save(sf.poola, file="stan/lday_site_sp_chill_inter_poola.Rda")

save(doym.fpoola.ncp, file="stan/lday_site_sp_chill_inter_poola_ncpfull.Rda")
# savestan("Fake Interax poola") # Not working! Saves a corrupted file.

# <> R stanarm <> #
library(rstanarm)

arm.doym.f <- stan_lmer(lday ~ warm + photo + chill1 + chill2 + site +
       warm*photo + warm*chill1 + warm*chill2 + warm*site + 
       photo*chill1 + photo*chill2 + photo*site + site*chill1 + site*chill2
       + (1|sp) +
       (sp|warm) + (sp|photo) + (sp|chill1) + (sp|chill2) + (sp|site), 
       data=datalist.f, 
       prior=normal(), prior_intercept=normal(0,30), prior_aux=cauchy(0,10))

# <><><><><> End pooled intercepts <><><><> #


##
library(rethinking)
goober <- map2stan(
     alist(
         lday ~ dnorm(yhat, sigma_y),
         yhat ~ a[sp] + b_site*sitis b_warm*warm + b_photo*photo + b_chill1*chill1 + b_chill2*chill2 + b_interwc*warm*photo,
         a[sp] ~ dnorm(mu_a_sp, sigma_a_sp),
         sigma_y ~ dnorm(0, 30),
         mu_a_sp ~ dnorm(0, 50),
         sigma_a_sp ~ dnorm(0, 30),
         b_site~ dnorm(0, 30),
         b_warm~ dnorm(0, 30),
         b_photo~ dnorm(0, 30),
         b_chill1~ dnorm(0, 30),
         b_chill2~ dnorm(0, 30),
         b_interwc ~ dnorm(0, 30)
     ) ,
     data=datalist.f, iter=2000)
##
stop("Lizzie has not worked with the below code .... ")

#setwd("~/Documents/git/buds/analyses")

# Load lastest fake data output. Grep for both Fake and Stan Output.
if(!exists('doym.f')){
  
  fakes <- na.omit(grep("Stan Output", dir())[match(grep("Fake", dir()), grep("Stan Output", dir()))])

  load(sort(dir()[fakes], T)[1])
}

sf <- summary(doym.f)$summary

plotlet("b_warm", "b_photo", 
        #xlab = "Advance due to 30d 4° chilling", 
        #ylab = "Advance due to 30d 1.5° chilling", 
        dat = sf)

plotlet("b_inter_wc2", "b_inter_wc1", 
        #xlab = "Advance due to 30d 4° chilling", 
        #ylab = "Advance due to 30d 1.5° chilling", 
        dat = sf)

bees <- sf[grep("mu_b", rownames(sf)),]
di <- sf[grep("mu_b_inter", rownames(sf)),]

plot(seq(min(di[,"mean"]-di[,"sd"]*1.5), max(di[,"mean"]+di[,"sd"]*1.5), length.out = nrow(di)),
     1:nrow(di), type ="n")


# # now with fixed site and fixed sp
# 
# datalist.f <- list(lday = fake$bb, # budburst as respose 
#                    warm = as.numeric(fake$warm), 
#                    site = as.numeric(fake$site), 
#                    sp = as.numeric(fake$sp), 
#                    photo = as.numeric(fake$photo), 
#                    chill = as.numeric(fake$chill), 
#                    N = nrow(fake), 
#                    n_site = length(unique(fake$site)), 
#                    n_sp = length(unique(fake$sp))
# )
# 
# doym.f2 <- stan('stan/lday0_fixedsite_fixedsp.stan', data = datalist.f, iter = 4000, chains = 4) 
# 
# ssm.f <- as.shinystan(doym.f2)
# #launch_shinystan(ssm.f2) 
# 
# (sumer <- summary(doym.f)$summary)
# 
# setwd("~/Dropbox")
# 
# savestan()
# 
# setwd("~/Documents/git/buds/analyses")
# 
# # Tips for speeding up, from google groups
# set_cppo(mode = "fast")
# # For finding part of code that is slow
# dir(tempdir())



