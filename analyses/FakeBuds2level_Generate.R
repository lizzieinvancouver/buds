# Started in April 2016 #
# Fake data creation by Dan Flynn, updates by Lizzie #

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)

setwd("~/Documents/git/freezingexperiment/analyses/scripts")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: same as experiment, with two sites, 28 species, two levels each of warming and photoperiod, and three levels of chilling. 2016-04-01 adding interactions. This ends up generating expected differences, but variation in effect sizes across species is minimal currently.
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsp = 2
nind = 7

ntx = 2

rep = 22 # within each combination of treatments. 

(ntot = nsp*ntx*rep) 

# Build up the data frame
sp = gl(nsp, rep, length = ntot)

tx = gl(ntx, rep*nsp, length = ntot)

(d <- data.frame(sp, tx)) # critical coding error here!

###### Set up differences for each level
spdiff = 0.5 
txdiff = 3 

######## SD for each treatment
spdiff.sd = 0.1
txdiff.sd = 1 


mm <- model.matrix(~(sp+tx), data.frame(sp, tx))
# remove last column, chill1 x chill2, empty
#mm <- mm[,-grep("sp", colnames(mm))]
colnames(mm)

coeff <- c(1, spdiff, txdiff)


dvr <- rnorm(n = length(tx), mean = mm %*% coeff, sd = 1) # should be able to do sd = mm %*% sd.coeff as well, with a different sd for each parameter.

(fake <- data_frame(dvr, sp, tx))

summary(lm(dvr ~ (tx+sp)^2, data = fake)) # sanity check 

##### Again, now with individuals.

baseinter = 11 # baseline intercept across all individuals 
spint <- baseinter + c(1:nind)-mean(1:nind) # different intercepts by individual

fake <- vector()

for(i in 1:nind){ # loop over individual (random effect)
  
  # Give individuals different difference values, drawn from normal
  coeff <- c(spint[i], 
             rnorm(1, spdiff, spdiff.sd),
             rnorm(1, txdiff, txdiff.sd)
  )
  
  dvr <- rnorm(n = length(tx), mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(dvr, ind = i, sp, tx)
      
  fake <- rbind(fake, fakex)  
  }

summary(lm(dvr ~ (sp+tx)^2, data = fake)) # sanity check 

# now fix the levels to 0/1 (not 1/2) as R does
fake$tx <- as.numeric(fake$tx)
fake$tx[fake$tx==1] <- 0
fake$tx[fake$tx==2] <- 1

summary(lm(dvr ~ (sp+tx)^2, data = fake)) # double sanity check 

save(list=c("fake"), file = "Fake Buds 2level.RData")
#write.csv(fake, file="~/Documents/git/freezingexperiment/analyses/output/FakeBuds2L.csv", row.names = FALSE)

mean(fake$dvr)

### Test the fake data:

library(rstan)
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(arm)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/freezingexperiment/analyses/")
source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


df<-read.csv("output/FakeBuds2L.csv", header=TRUE)
mb<-read.csv("output/FakeBuds_apply.csv", header=TRUE)
bb<-read.csv("output/birches_clean.csv", header=TRUE)

fit1<-stan_glmer(dvr~tx+sp+(1|ind), data=df)
fit1
fit2<-stan_glmer(dvr~tx+species+(1|ind), data=mb)
fit2

bb$tx<-ifelse(bb$tx=="A", 0, 1)
bb$sp <- as.numeric(as.factor(bb$sp))
bb$dvr <- as.numeric(bb$dvr)
#bb$ind<-substr(bb$individ, 9,10)
#bb$ind <- as.numeric(as.factor(bb$bud))
fit3<-stan_glmer(dvr~tx+sp+(1|ind), data=bb)
fit3
fit1;fit3
plot(fit3)
abline(v=0)

pp_check(fit3, reps=30)

