# Fake data.

setwd("~/Documents/git/buds/analyses")

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

sigma = 20

lday = c(
	rnorm(N/4, mean = mu_CS, sd = sigma),
	rnorm(N/4, mean = mu_CL, sd = sigma),
	rnorm(N/4, mean = mu_WS, sd = sigma),
	rnorm(N/4, mean = mu_WL, sd = sigma)
	)

fak$lday <- lday

tapply(fak$lday, list(fak$warm, fak$photo), mean)