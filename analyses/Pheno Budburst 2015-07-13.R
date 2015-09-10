# Analyzing phenology first work March 2015. Script also for Tim to work with R


# Setup: load packages, set working directory, read in the data.

library(gdata)
library(nlme)
library(scales)
library(arm)

setwd("~/Documents/git/buds")

d1 <- read.xls("Budburst Datasheet 2015-05-15.xlsx") # read.xls function from gdata. Slow, but no intermediate step of saving as csv. 

d2 <- read.xls("Budburst Datasheet 2015-05-15.xlsx", sheet = 2) # long format for consolidated treatments and chilling

# add relevant columns to d2 twigs

idx <- strsplit(as.character(d2$id), "_")
d2$site <- unlist(lapply(idx, function(x) x[2]))
d2$rep <- unlist(lapply(idx, function(x) x[3]))
d2$sp <- substr(unlist(lapply(idx, function(x) x[1])), 1, 6)
d2$gen <- substr(unlist(lapply(idx, function(x) x[1])), 1, 3)
d2$ind <- substr(d2$id, 1, 11)


d2$warm <- factor(substr(d2$treatcode, 1, 1), labels = c("cool", "warm"))
d2$photo <- factor(substr(d2$treatcode, 2, 2), labels = c("long", "short"))
d2$chill <- factor(substr(d2$treatcode, 3, 3), labels = c("chill0", "chill1", "chill2"))



coltokeep <- c("Date","id","sp","rep","site","ind","treatcode","warm","photo","chill","gen","Term.fl","Lat.fl","Term.lf","Lat.lf","Comments","Observer")

d <- rbind(d1[coltokeep], d2[coltokeep])


# Format date, making a continous day since beginning of experiment	
d$Date <- strptime(d$Date, "%Y-%m-%d")
day0 <- as.numeric(format(d$Date, "%j"))
d$day <- day0 - as.numeric(format(strptime("2015-02-06", "%Y-%m-%d"), "%j")) + 1 
d$day.chill <- day0 - as.numeric(format(strptime("2015-03-11", "%Y-%m-%d"), "%j")) + 1 

d$dayuse <- ifelse(d$chill == "chill0", d$day, d$day.chill)

# order by date

d <- d[order(d$Date, d$id, d$treatcode),]

# typical difference in date by treatment
for(i in levels(d$treatcode)){
	datediff <- diff(d[d$treatcode == i,"Date"])
	cat(rep("<>", 20), "\n", i, "\n")
	print(summary(unclass(datediff[datediff!=0]/60/60/24)))
	}


# Cleanup: fix data types (slashes and other characters in the phenostage columns), date format

names(d) 

# make continuous data, now using second value if something is split by slash
d$tleaf <- unlist(lapply(strsplit(as.character(d$Term.lf), "/"), function(x) ifelse(length(x)>1, x[2], x[1])))
d$lleaf <- unlist(lapply(strsplit(as.character(d$Lat.lf), "/"), function(x) ifelse(length(x)>1, x[2], x[1])))

# and use first values if comma separated
d$tleaf <- unlist(lapply(strsplit(as.character(d$tleaf), ", "), function(x) ifelse(length(x)>1, x[1], x[1])))
d$lleaf <- unlist(lapply(strsplit(as.character(d$lleaf), ", "), function(x) ifelse(length(x)>1, x[1], x[1])))

for(i in c("tleaf","lleaf","Term.fl","Lat.fl")){
	d[,i][d[,i] =="-"] = NA
	d[,i][d[,i] ==""] = NA
	d[,i][d[,i] =="*"] = NA
	}
	
d$lleaf <- sub("\\*", "", d$lleaf) # get rid of one asterix after 4*

# Make into numeric data, needed because it was read in with weird characters and automaticially made into factors
for(i in c("tleaf","lleaf","Term.fl","Lat.fl")){
	d[,i] = as.numeric(as.character(d[,i])) }

# calculating day since initiation of experiment, by twig id (includes treatment). Tricky part: what should NA be? They still haven't reached stage 6... for this calculation, I put them as NA. Will change with more data.

# For chill treatments, count days since they started in chambers, not since chill0 started

bday <- lday <- fday <- vector()

for(i in levels(d$id)){ # i=levels(d$id)[500] 
	
	dx <- d[d$id == i,]

	day.use <- ifelse(dx$chill[1] == "chill0", "day", "day.chill")

	bdax <- which(apply(dx[,c("tleaf","lleaf")], 1, max, na.rm=T) >= 3)
	if(length(bdax) < 1) bdax = 75 else bdax = dx[min(bdax),day.use]

	ldax <- which(apply(dx[,c("tleaf","lleaf")], 1, max, na.rm=T) >= 6)
	if(length(ldax) < 1) ldax = 75 else ldax = dx[min(ldax),day.use]
		
	fdax <- which(apply(dx[,c("Term.fl","Lat.fl")], 1, max) > 16)
	if(length(fdax) < 1) fdax = 75 else fdax = dx[min(fdax),day.use]

	bday <- c(bday, bdax)
	lday <- c(lday, ldax)
	fday <- c(fday, fdax)	
	}



# merging with unique id data

dx <- d[match(levels(d$id), d$id),] # with twig id in same order as the loop above

dx <- dx[,2:20]

dx <- data.frame(dx, lday, fday, bday)

# Clean up levels for the treatment factors. Default is alphabetical, here want to sort in a meaningful way.
# levels(dx$treatcode) <- c(2,1,4,3)
# dx$treatcode <- as.factor(dx$treatcode)
# levels(dx$treatcode) <- c("Cool-short","Cool-long","Warm-short","Warm-long")

dx$photo <- as.factor(as.character(dx$photo))
levels(dx$photo) <- c('12', '08')
dx$photo <- as.factor(as.character(dx$photo))

dx$warm <- as.factor(as.character(dx$warm))
levels(dx$warm) <- c('15', '20')
dx$warm <- as.factor(as.character(dx$warm))

write.csv(dx, "Budburst By Day.csv", row.names=F)
save(list = c('d', 'dx'), file = paste("Budburst Data", Sys.Date())) # save as R formatted data frames for easier use next time.



# 

library(gdata)
library(nlme)
library(scales)
library(arm)

setwd("~/Dropbox/Work/Harvard/Budburst/")

load("Budburst Data 2015-05-26")



# Analysis

summary(m1 <- aov(lday ~ sp * site + warm * photo + Error(ind), data = dx[dx$chill == 'chill0',]))


summary(m2 <- aov(lday ~ sp * site * warm * photo + Error(ind), data = dx[dx$chill == 'chill0',])) # interax with sp and warm, also sp and photo, no site effects!


summary(bm2 <- aov(bday ~ sp * site * warm * photo + Error(ind), data = dx[dx$chill == 'chill0',])) # site effex interax with warm for budbust but not leafout

summary(fm2 <- aov(fday ~ sp * site * warm * photo + Error(ind), data = dx[dx$chill == 'chill0',])) # no 


# with lme4 mixed effect model to better take into account species differences 

# test without the 75's
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



# Plot m3

#warming effect
# # int.warm <- ranef(m3)[[1]][,1]
# slope.warm <- ranef(m3)[[1]][,2]
# warmeff <- int.warm+slope.warm

# plot(-20:20, -20:20, type ="n")
# for(i in 1:length(int.warm)){
	# abline(a = int.warm[i], b = slope.warm[i])
	# }



# Species which are temperature sensitive vs photo sensitive
plot(ranef(m3)$sp[,2],ranef(m3)$sp[,4],
	pch = "+", col = "grey10",
	#pch = rownames(ranef(m3)$sp),
	xlab = "Warming response",
	ylab = "Photo response",
	#xlim = c(-8.5, 6),
	#ylim = c(-5, 3)
	)
text(ranef(m3)$sp[,2],ranef(m3)$sp[,4], 
	labels = rownames(ranef(m3)$sp), cex = 0.6, pos = 1,
	col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))
#dev.print(file = "Ran Eff Warming vs Photo on Days To Leafout.ps")

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

xx <- data.frame(aggregate(dx1$lday, by=list(dx1$sp), FUN = mean, na.rm=T), ranef(m31)$sp[,1], ranef(m31)$sp[,3])

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
dev.print(file = "./Figures/tempphotsens.pdf", device = pdf)



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

# getting mean and sd from model fit
summary(m3)
coef(m3)


# effect of chilling treatment. Show advance in 

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


densplot("VIBLAN"); title(xlab = "Days to leafout", main = "Ilex mucronata", font.main = 3)
legend("topright", fill=cols, legend = c("0", "4°C", "1.5°C"), title = "Chilling treatment", bg="white")
densplot("FAGGRA", ylim = 0.03); title(xlab = "Days to leafout",  main = "Populus grandifolia", font.main = 3)

densplot("VIBLAN", 'bday'); title(xlab = "Days to budburst")

densplot("FAGGRA", 'bday', ylim = 0.03); title(xlab = "Days to budburst")





# desnplot by site
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

