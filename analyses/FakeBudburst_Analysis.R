# Fake data testing of pheno budburst experiment 2015

library(nlme)
library(scales)
library(arm)
library(rstan)
library(xtable)
library(memisc) # for getSummary
library(ggplot2)
library(picante)
library(sjPlot)
library(shinystan)

setwd("~/Documents/git/buds/analyses")
source('stan/savestan.R')
# get latest .Rdata file

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("Fake Budburst.RData")

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

doym.f <- stan('stan/lday_site_sp_chill.stan', data = datalist.f, iter = 4000, chains = 4) 
# lday_site_chill: < 120 seconds per chain, very fast
# lday_site_sp_chill: much slower.   
#doym.f <- stan('stan/lday0.stan', data = datalist.f, iter = 4000, chains = 4) 

sumer <- summary(doym.f)$summary
sumer[grep("mu_", rownames(sumer)),] # effects are perfectly captured now.

pairs(doym.f, pars = names(doym.f)[grep("mu_", names(doym.f))])

ssm.f <- as.shinystan(doym.f)
launch_shinystan(ssm.f) 

setwd("~/Dropbox")

savestan("Fake")

setwd("~/Documents/git/buds/analyses")


#### now with sp intercept only

datalist.spint <- list(lday = fake$bb, # budburst as respose
                   warm = as.numeric(fake$warm), 
                   sp = as.numeric(fake$sp), 
                   photo = as.numeric(fake$photo), 
                   chill = as.numeric(fake$chill), 
                   N = nrow(fake), 
                   n_sp = length(unique(fake$sp))
)

doym.f <- stan('stan/lday_nosite_plusspint.stan', data = datalist.spint, iter = 4000, chains = 4) 

sumer <- summary(doym.f)$summary
sumer[grep("mu_", rownames(sumer)),]

ssm.f <- as.shinystan(doym.f)
#launch_shinystan(ssm.f) 

(sumer <- summary(doym.f)$summary)

setwd("~/Dropbox")

savestan()

setwd("~/Documents/git/buds/analyses")

# 
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
