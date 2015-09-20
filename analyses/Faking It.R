# Fake data.

library(arm)
library(rstan)

setwd("~/Documents/git/buds/analyses")
source("savestan.R")
load("input/Budburst Data 2015-09-16")

# First for doy model 1, with same individuals having multiple cuttings in the different treatments. No species or site effects yet.

N = 1000

warm = gl(2, k = 2, length = N, labels = 0:1)

photo = gl(2, k = 1, length = N, labels = 0:1)

treat = paste(warm, photo)

fak <- data.frame(warm, photo, treat)

fak <- fak[order(fak$treat),] # put it in order of treatments

# mean doy values

tapply(dx$lday, list(dx$warm, dx$photo), mean) # hm... overall, get a delay with long days. Odd. Will make fake data with advance for long day
tapply(dx$lday, list(dx$warm, dx$photo), sd)

mu_CS = 45
mu_CL = 40
mu_WS = 35
mu_WL = 30

sigma = 10

lday = c(
	rnorm(N/4, mean = mu_CS, sd = sigma),
	rnorm(N/4, mean = mu_CL, sd = sigma),
	rnorm(N/4, mean = mu_WS, sd = sigma),
	rnorm(N/4, mean = mu_WL, sd = sigma)
	)

fak$lday <- lday

tapply(fak$lday, list(fak$warm, fak$photo), mean)

# now ready for stan

datalist1 <- list(lday = fak$lday, warm = as.numeric(fak$warm), photo = as.numeric(fak$photo), N = nrow(fak))

doym0 <- stan('doy_model0.stan', data = datalist1, iter = 1000, chains = 4) # without interaction

doym0 # works. Effect size for beta[2], the warming effect, is about 10 days, while beta[3], photoperiod, about 5 days.

datalist1 <- list(lday = fak$lday, warm = as.numeric(fak$warm), photo = as.numeric(fak$photo), N = nrow(fak))

doym1 <- stan('doy_model1.stan', data = datalist1, iter = 1000, chains = 4) # with interaction of temp x photo

doym1 

# What about the interaction? There is no interaction in the data, but here have a small one (with sd of the estimate nearly equal to the estimate itself). Makes the estimates for temp and photo smaller, anyways.

# Now fake it for model 2, 



savestan()