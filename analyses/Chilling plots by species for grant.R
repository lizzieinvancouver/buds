
# Making chilling density-grams for each species by site.

library(scales)

setwd("~/Documents/git/buds/analyses")

load("input/Budburst Data 2015-10-19")

sp.info <- read.csv("data/Species General Info.csv")

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check
#  without the 75's -- these did no ever leaf out, or flower, over the course of the experiment, but were not dead. 75 days was assigned to them as max value
dx1 <- dx
dx1[dx1==75] = NA

chillsp = aggregate(chill ~ sp, FUN = function(x) length(x)>100, data = dx1) # omitting 75's first
chillsp = chillsp[chillsp$chill==TRUE,"sp"]
dx.chill <-  dx1[!is.na(match(dx1$sp, chillsp)),]

data.frame(aggregate(lday~ chill * sp, FUN = mean, data = dx1)[1:2], diff = aggregate(lday~ chill * sp, FUN = mean, data = dx1)[,3] - aggregate(lday~ chill * sp, FUN = mean, data = dx)[,3])

cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)

# advanced by chill1, delayed by chill2: ilemuc, popgra. 
# Advanced by chilling at each level: Faggra
# Advanced by chilling at least by 4°C: Acepen, Acerub, Acesac  Betall, Betpap, 

densplot <- function(sp, site, response = 'lday', ylim = 0.1, adjust = 3){
	
	x0 <- na.omit(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill0' & dx.chill$site == site,response])
	x1 <- na.omit(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill1' & dx.chill$site == site,response])
	x2 <- na.omit(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill2' & dx.chill$site == site,response])
	
	df0 <- density(x0, adjust = adjust)
	df1 <- density(x1, adjust = adjust)
	df2 <- density(x2, adjust = adjust)

	ylim = max(c(df0$y, df1$y, df2$y))*1.15

	plot(
		seq(0, ylim, length.out = 100) ~ seq(-20, 120, length.out = 100),
			type = "n", xlab = "", ylab ="", yaxt="n", xaxt ="n", bty = "n"
			)
		axis(1, at = seq(0, 100, by = 20), labels = seq(0, 100, by = 20)) 
	 polygon(df0$x, df0$y, col = cols[1], border = NA)
	 polygon(df1$x, df1$y, col = cols[2], border = NA)
	 polygon(df2$x, df2$y, col = cols[3], border = NA)	
	
	abline(v = mean(x0), col = cols[1], lty = 3, lwd = 2)
	abline(v = mean(x1), col = cols[2], lty = 3, lwd = 2)	
	abline(v = mean(x2), col = cols[3], lty = 3, lwd = 2)	
	
	}


par(mfrow =c(2, 1), mar = c(4, 2, 1, 1))

densplot("FAGGRA", "HF")
title(xlab = "Days to leafout",  main = "Fagus grandifolia", font.main = 3)
legend("topleft", "Harvard Forest", bty="n")
legend("topright", fill=cols, legend = c("0", "4°C", "1.5°C"), title = "Chilling treatment", bg="white")

densplot("FAGGRA", "SH")
legend("topleft", "St. Hippolyte", bty="n")


pdf(file = "graphs/Chill x Sp x Site.pdf", width = 6, height = 9)
par(mfrow =c(2, 1), mar = c(5, 2, 2, 2))

for(i in unique(dx.chill$sp)){ # i = "ACESAC"
	
	densplot(i, "HF")
	title(xlab = "Days to leafout",  main = sp.info[!is.na(match(sp.info$Code, i)),"Species"], font.main = 3)
	legend("topleft", "Harvard Forest", bty="n")
	legend("topright", fill=cols, legend = c("0", "4°C", "1.5°C"), title = "Chilling treatment", bg="white", bty = "n")
	
	densplot(i, "SH")
	legend("topleft", "St. Hippolyte", bty="n")
	
	
	}

dev.off();system("open 'graphs/Chill x Sp x Site.pdf' -a /Applications/Preview.app")

# Repeat, but for just 4°C chilling, for select species
densplot4 <- function(sp, site, response = 'lday', ylim = 0.1, adjust = 3){
	
	x0 <- na.omit(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill0' & dx.chill$site == site,response])
	x1 <- na.omit(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill1' & dx.chill$site == site,response])
	
	df0 <- density(x0, adjust = adjust)
	df1 <- density(x1, adjust = adjust)

	ylim = max(c(df0$y, df1$y, df2$y))*1.15

	plot(
		seq(0, ylim, length.out = 100) ~ seq(-20, 120, length.out = 100),
			type = "n", xlab = "", ylab ="", yaxt="n", xaxt ="n", bty = "n"
			)
		axis(1, at = seq(0, 100, by = 20), labels = seq(0, 100, by = 20)) 
	 polygon(df0$x, df0$y, col = cols[1], border = NA)
	 polygon(df1$x, df1$y, col = cols[2], border = NA)
	
	abline(v = mean(x0), col = cols[1], lty = 3, lwd = 2)
	abline(v = mean(x1), col = cols[2], lty = 3, lwd = 2)	
	}


pdf(file = "graphs/Chill 4 only x Sp x Site.pdf", width = 6, height = 9)
par(mfrow =c(2, 1), mar = c(5, 2, 2, 2))

for(i in unique(dx.chill$sp)){
	
	densplot4(i, "HF")
	title(xlab = "Days to leafout",  main = sp.info[!is.na(match(sp.info$Code, i)),"Species"], font.main = 3)
	legend("topleft", "Harvard Forest", bty="n")
	legend("topright", fill=cols[1:2], legend = c("0", "4°C"), title = "Chilling treatment", bg="white", bty = "n")
	
	densplot4(i, "SH")
	legend("topleft", "St. Hippolyte", bty="n")
	
	
	}

dev.off();system("open 'graphs/Chill 4 only x Sp x Site.pdf' -a /Applications/Preview.app")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Repeat, now with 75's included

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>


chillsp = aggregate(chill ~ sp, FUN = function(x) length(x)>100, data = dx) # with non-leafouts 
chillsp = chillsp[chillsp$chill==TRUE,"sp"]
dx.chill <-  dx[!is.na(match(dx$sp, chillsp)),]

pdf(file = "graphs/Chill x Sp x Site with nl.pdf", width = 6, height = 9)
par(mfrow =c(2, 1), mar = c(5, 2, 2, 2))

for(i in unique(dx.chill$sp)){ # i = "ALNINC"
	
	densplot(i, "HF")
	title(xlab = "Days to leafout",  main = sp.info[!is.na(match(sp.info$Code, i)),"Species"], font.main = 3)
	legend("topleft", "Harvard Forest", bty="n")
	legend("topright", fill=cols, legend = c("0", "4°C", "1.5°C"), title = "Chilling treatment", bg="white", bty = "n")
	
	densplot(i, "SH")
	legend("topleft", "St. Hippolyte", bty="n")
	
	
	}
	
dev.off();system("open 'graphs/Chill x Sp x Site with nl.pdf' -a /Applications/Preview.app")


pdf(file = "graphs/Chill 4 only x Sp x Site with nl.pdf", width = 6, height = 9)
par(mfrow =c(2, 1), mar = c(5, 2, 2, 2))

for(i in unique(dx.chill$sp)){ # i = "ALNINC"
	
	densplot4(i, "HF")
	title(xlab = "Days to leafout",  main = sp.info[!is.na(match(sp.info$Code, i)),"Species"], font.main = 3)
	legend("topleft", "Harvard Forest", bty="n")
	legend("topright", fill=cols[1:2], legend = c("0", "4°C"), title = "Chilling treatment", bg="white", bty = "n")
	
	densplot4(i, "SH")
	legend("topleft", "St. Hippolyte", bty="n")
	
	
	}

dev.off();system("open 'graphs/Chill 4 only x Sp x Site with nl.pdf' -a /Applications/Preview.app")
