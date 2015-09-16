
# Analysis of budburst experiment. Starting with simple linear models
# 2015-0916 adding single species models

library(nlme)
library(scales)
library(arm)
library(rstan)

setwd("~/Documents/git/buds/analyses")

load("input/Budburst Data 2015-09-16")

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check

# Anovas based on day to leafout (stage 6)

summary(m1 <- aov(lday ~ sp * site + warm * photo + Error(ind), data = dx[dx$chill == 'chill0',]))

# Comparing Type I SS issues
summary(m1 <- aov(lday ~ site * warm * photo + Error(ind), data = dx[dx$chill == 'chill0',]))
summary(m1 <- aov(lday ~ warm * photo * site + Error(ind), data = dx[dx$chill == 'chill0',]))
summary(m1 <- aov(lday ~ warm * site  * photo + Error(ind), data = dx[dx$chill == 'chill0',]))



summary(m2 <- aov(lday ~ sp * site * warm * photo + Error(ind), data = dx[dx$chill == 'chill0',])) # interax with sp and warm, also sp and photo, no site effects!

summary(bm2 <- aov(bday ~ sp * site * warm * photo + Error(ind), data = dx[dx$chill == 'chill0',])) # site effex interax with warm for budbust (stage 3) but not leafout (stage 6)

summary(fm2 <- aov(fday ~ sp * site * warm * photo + Error(ind), data = dx[dx$chill == 'chill0',])) # no clear effects of anything other than species for the flowering

# with lme4 mixed effect model to better take into account species differences 

# test without the 75's -- these did no ever leaf out, or flower, over the course of the experiment, but were not dead. 75 days was assigned to them as max value
dx1 <- dx
dx1[dx1==75] = NA

m3 <- lmer(lday ~ warm * photo * site  + (warm|sp) + (photo|sp), data = dx[dx$chill == 'chill0',])
summary(m3)

m31 <- lmer(lday ~ warm * photo * site + (warm|sp) + (photo|sp), data = dx1[dx1$chill == 'chill0',])

summary(m31)


fixef(m3)
ranef(m3)
m3f <- lmer(fday ~ warm * photo + (1|sp/ind), data = dx)
summary(m3f)

# New analyses, in stan, using simple models
# model 1: lday ~ warm * photo, no species, site, or ind.
# make data all numeric.
levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$sp) = 1:length(levels(dx$sp))
dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$site <- as.numeric(dx$site)
dx$sp <- as.numeric(dx$sp)

datalist1 <- list(lday = dx$lday, warm = dx$warm, photo = dx$photo, N = nrow(dx))

doym1 <- stan('doy_model1.stan', data = datalist1, iter = 1000, chains = 4)

# Model 2: site added
datalist2 <- list(lday = dx$lday, warm = dx$warm, site = dx$site, photo = dx$photo, N = nrow(dx), n_site = length(unique(dx$site)))

doym2 <- stan('doy_model2.stan', data = datalist2, iter = 1000, chains = 4)

head(summary(doym2)$summary) # leafout day slightly later for HF

# doym3  -- need to make a sp_site vector
sp_site = as.numeric(paste(dx$site, formatC(dx$sp, width = 2, flag = '0'), sep=""))

datalist3 <- list(lday = dx$lday, warm = dx$warm, site = dx$site, sp = dx$sp, photo = dx$photo, N = nrow(dx), n_site = length(unique(dx$site)), n_sp = length(unique(dx$sp)), sp_site = sp_site, n_sp_site = length(unique(sp_site)))

doym3 <- stan('doy_model3.stan', data = datalist3, iter = 1000, chains = 4) # Error:  sp_site[k0__] is 101, but must be less than or equal to 47
failed to create the sampler; sampling not done


# Chilling
# effect of chilling treatment. 

# simple - fails bc chill different by inds
# make a chillsp column
chillsp = aggregate(chill ~ sp, FUN = function(x) length(x)>100, data = dx)
chillsp = chillsp[chillsp$chill==TRUE,"sp"]
dx.chill <-  dx[!is.na(match(dx$sp, chillsp)),]

#summary(m01 <- aov(lday ~ sp * site * warm * photo * chill + Error(ind), data =dx.chill))

summary(m01 <- aov(lday ~ chill, data = dx.chill))
# wrong...
summary(m01 <- aov(lday ~ sp * site * warm * photo * chill, data = dx.chill))
coef(m01) # chill1 advanced leafout by 4.5 days, while chill2 delayed leafout by .67 days

m5 <- lmer(lday ~ warm * photo * site * chill + (warm|sp) + (photo|sp) + (chill|sp), data = dx.chill)
summary(m5)

m6 <- lmer(lday ~ site + (warm|sp) + (photo|sp) + (chill|sp), data = dx)
summary(m6)

m7 <- lmer(lday ~ warm * photo + chill*warm + chill*photo + chill*site + (chill|sp), data = dx.chill)
summary(m7)

m17 <- lmer(lday ~ site * chill + (1|sp), data = dx)
summary(m17)



# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Plotting
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>



# Plot m31
plot(ranef(m31)$sp[,1],ranef(m31)$sp[,3],
	pch = "+", col = "grey10",
	type = "n",
	xlab = "Warming response",
	ylab = "Photoperiod response",
	xlim = c(-18, 18),
	ylim = c(-18, 18)
	)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
points(ranef(m31)$sp[,1],ranef(m31)$sp[,3],
	pch = "+", col = "grey10")

text(ranef(m31)$sp[,1],ranef(m31)$sp[,3], 
	labels = rownames(ranef(m31)$sp), cex = 0.6, pos = 1,
	col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))
dev.print(file = "ranefs.pdf", device = pdf)
# Plot sensitivity by actual leafout time

xx <- data.frame(aggregate(dx1$lday, by=list(dx1$sp), FUN = mean, na.rm=T), ranef(m31)$sp[,2], ranef(m31)$sp[,4])

# xx: col 1 is the mean leafout day across all treatments for that species. col 2 is interecept of that species for warming effect, col 3 intercept for photo. Should use slope instead?

colz.tp = c("tomato", "darkgoldenrod")

#par(mfrow=c(2,1), mar = c(4, 4, .5, .5))
plot(xx[,2], xx[,3], ylab = "Leafout sensitivity", xlab = "Observed leafout day", 
	pch = 16, col = alpha(colz.tp[1], 0.5), type = "n")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
points(xx[,2], xx[,3], ylab = "Leafout sensitivity", xlab = "Observed leafout day", 
	pch = 16, col = alpha(colz.tp[1], 0.5))
abline(lm(xx[,3]~xx[,2]), col=alpha(colz.tp[1], 0.5) )
points(xx[,2], xx[,4], 	pch = 16, col = alpha(colz.tp[2], 0.8))
abline(lm(xx[,4]~xx[,2]), col=alpha(colz.tp[2], 0.8), lty =2)
legend("topleft", pch =16, col = colz.tp, lty = c(1,2), legend = c("Temperature effect","Photoperiod Effect"), bty = "n")

dev.print(file = "analyses/graphs/tempphotsens.pdf", device = pdf)


means <- aggregate(lday ~ warm * photo * site, FUN = mean, data=dx)
ses <- aggregate(lday ~ warm * photo * site, FUN = function(x) sd(x)/sqrt(length(x)) , data=dx)

means.bud <- aggregate(bday ~ warm * photo * site, FUN = mean, data=dx)
ses.bud <- aggregate(bday ~ warm * photo * site, FUN = function(x) sd(x)/sqrt(length(x)) , data=dx)


cols = alpha(c("midnightblue","darkred"), 0.8)

xax = c(1.5, 1.65, 1.85, 2)

par(mfrow=c(2,1), mar = c(2, 4, 0.5, 1.25))
for(i in c('HF','SH')){
	mx <- means[means$site == i,"lday"]
	mxb <- means.bud[means.bud$site == i,"bday"]
	plot(xax,  mx, ylim = c(25,68), pch = 16, xaxt="n", xlab = "", ylab ="Days to event",
		col = cols, type = "n")
	rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
	points(xax,  mx, pch = 16, col = cols)
	
	points(xax, mxb, pch = 16, col = cols)
	
		
	arx <-ses[ses$site == i,"lday"]
	arxb <-ses.bud[ses.bud$site == i,"bday"]
	arrows(xax, mx-arx, xax, mx+arx, length = 0, col = cols)
	arrows(xax, mxb-arxb, xax, mxb+arxb, length = 0, col = cols)

	lines(xax[1:2], mx[1:2]); lines(xax[3:4], mx[3:4])
	lines(xax[1:2], mxb[1:2]); lines(xax[3:4], mxb[3:4])
	legend('topright', i, bty = "n")

	text(xax[3], mx[3], "Leafout", pos = 2)
	text(xax[3], mxb[3], "Budburst", pos = 2)
		
	if(i == "SH") axis(1, at = xax, labels = c("Cool/Short", "Warm/Short","Cool/Long","Warm/Long"), cex.axis = 0.8)
	}
dev.print(file = "./Figures/Overall_photo_temp.pdf", device = pdf)

# in long days, greater warming effect observed, but this does not translate to a site effect.

# getting mean and sd from model fit
summary(m31)
coef(m31)

############# chilling plot

aggregate(lday~ chill * sp, FUN = mean, data = dx)
cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)

# advanced by chill1, delayed by chill2: ilemuc, popgra. delayed: acesac, faggra
densplot <- function(sp, response = 'lday', ylim = 0.1){
	
#	cols = hcl(h = seq(120, by=360 / 3, length = 3), l = 75, alpha = 0.7) 
	cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)
	df0 <- density(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill0',response], adjust = 2.2)
	df1 <- density(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill1',response], adjust = 2.2)
	df2 <- density(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill2',response], adjust = 2.2)

	plot(
		seq(0, ylim, length.out = 100) ~ seq(0, 90, length.out = 100),
			type = "n", xlab = "", ylab ="", yaxt="n", xaxs ="r", bty = "n"
			)
	 polygon(df0$x, df0$y, col = cols[1], border = NA)
	 polygon(df1$x, df1$y,col = cols[2], border = NA)
	polygon(df2$x, df2$y, col = cols[3], border = NA)	
	
	abline(v = mean(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill0',response]), col = cols[1], lty = 3, lwd = 2)
	abline(v = mean(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill1',response]), col = cols[2], lty = 3, lwd = 2)	
	abline(v = mean(dx.chill[dx.chill$sp == sp & dx.chill$chill == 'chill2',response]), col = cols[3], lty = 3, lwd = 2)	
	
	#axis(1, at = c(0, 0.5, 1), labels = c(1, 0.5, 0), cex.axis = 0.7)
	}
par(mfrow =c(2, 2), mar = c(4, 2, 1, 1))

densplot("ILEMUC"); title(xlab = "Days to leafout", main = "Ilex mucronata", font.main = 3)
legend("topright", fill=cols, legend = c("0", "4°C", "1.5°C"), title = "Chilling treatment", bg="white")
densplot("FAGGRA", ylim = 0.03); title(xlab = "Days to leafout",  main = "Fagus grandifolia", font.main = 3)

densplot("ILEMUC", 'bday'); title(xlab = "Days to budburst")

densplot("FAGGRA", 'bday', ylim = 0.03); title(xlab = "Days to budburst")

dev.print(file = "./Figures/Chill_effect.pdf", device = pdf)

# densplot by site
densplot.site <- function(sp, response = 'lday', ylim = 0.1, site = "HF"){
	
	cols = hcl(h = seq(120, by=360 / 3, length = 3), l = 75, alpha = 0.7) 
	cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)
	df0 <- density(dx.chill[dx.chill$site == site & dx.chill$sp == sp & dx.chill$chill == 'chill0',response], adjust = 2.2)
	df1 <- density(dx.chill[dx.chill$site == site & dx.chill$sp == sp & dx.chill$chill == 'chill1',response], adjust = 2.2)
	df2 <- density(dx.chill[dx.chill$site == site & dx.chill$sp == sp & dx.chill$chill == 'chill2',response], adjust = 2.2)

	plot(
		seq(0, ylim, length.out = 100) ~ seq(0, 90, length.out = 100),
			type = "n", xlab = "", ylab ="", yaxt="n", xaxs ="r", bty = "n"
			)
	 polygon(df0$x, df0$y, col = cols[1], border = NA)
	 polygon(df1$x, df1$y,col = cols[2], border = NA)
	polygon(df2$x, df2$y, col = cols[3], border = NA)	
	
	abline(v = mean(dx.chill[dx.chill$site == site & dx.chill$sp == sp & dx.chill$chill == 'chill0',response]), col = cols[1], lty = 3, lwd = 2)
	abline(v = mean(dx.chill[dx.chill$site == site & dx.chill$sp == sp & dx.chill$chill == 'chill1',response]), col = cols[2], lty = 3, lwd = 2)	
	abline(v = mean(dx.chill[dx.chill$site == site & dx.chill$sp == sp & dx.chill$chill == 'chill2',response]), col = cols[3], lty = 3, lwd = 2)	
	
	#axis(1, at = c(0, 0.5, 1), labels = c(1, 0.5, 0), cex.axis = 0.7)
	}

par(mfcol = c(2, 2), mar = c(4, 2, 1, 1))

densplot.site("FAGGRA", ylim = 0.03); title(main = "Fagus grandifolia", font.main = 3)
legend("topleft", fill=cols, legend = c("0", "4°C", "1.5°C"), title = "Chilling treatment", bg="white")
legend("topright", legend = "HF", bty = "n", text.font=2)

densplot.site("FAGGRA", site = "SH", ylim = 0.03); title(xlab = "Days to leafout")
legend("topright", legend = "SH", bty = "n", text.font=2)

densplot.site("FAGGRA", response = 'bday', site = "HF", ylim=0.03);legend("topright", legend = "HF", bty = "n", text.font=2)

densplot.site("FAGGRA", response = 'bday', site = "SH", ylim =0.03); title(xlab = "Days to budburst")

legend("topright", legend = "SH", bty = "n", text.font=2)

dev.print(file = "./Figures/Chill_effect_fagus.pdf", device = pdf)





########### raw data plots. 

# re-sort to make sure ordered by date correctly
d <- d[order(d$Date, d$id, d$treatcode),]

colz <- c("darkred","darkgreen")
lcol <- alpha(colz, 0.1)

pdf(paste("Pheno Test ", Sys.Date(), ".pdf", sep=""))

par(mfcol=c(3, 4), mar = c(3,3,1,0.5))
for(spx in levels(d$sp)){ # spx = "ACEPEN"

	dxx = d[d$sp == spx,]

	counter = 1
	for(i in sort(as.character((unique(dx$treatcode))))){#c("CS0","CL0","WS0","WL0")){
	
		dseq = seq(0, max(dx$dayuse))
		plot(dseq, seq(0,25,length=length(dseq)), type = "n", 
			ylab = "Stage",
			xlab = "")
		if(counter == 1) mtext(spx, line = -2, adj = 0.5)
		legend("topleft",bty="n",i, cex = 0.85, inset = 0)
		xx <- dxx[dxx$treatcode == i,]
		# calculate mean response by date and site
		xt <- tapply(pmax(xx$tleaf, xx$lleaf,na.rm=T), list(xx$dayuse, xx$site), mean, na.rm=T)
			
		for(j in unique(xx$ind)){ #j=unique(xx$ind)[1]
			xj <- xx[xx$ind == j,]
			pcol = ifelse(xj$site[1] == "HF", lcol[1], lcol[2])
			lines(xj$dayuse, xj$tleaf, col = pcol)
			}
		lines(rownames(xt), xt[,1], col = colz[1])
		lines(rownames(xt), xt[,2], col = colz[2])
			
			counter = counter + 1
			}
	
	}
dev.off()
system(paste("open '", paste("Pheno Test ", Sys.Date(), ".pdf", sep=""), "' -a /Applications/Preview.app", sep=""))

# Days to first leaf out (6)

