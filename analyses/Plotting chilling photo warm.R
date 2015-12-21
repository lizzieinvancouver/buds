# Plotting species as random versus as fixed effects

library(nlme)
library(scales)
library(arm)
library(rstan)

setwd("~/Documents/git/buds/analyses")
source('savestan.R')
# get latest data
print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check


# Again, now with interaction, to compare pooling of random effects

m.spfix <- lm(lday ~ warm*photo +warm:sp + photo:sp 
						#+ site * chill
						, data = dx[dx$nl == 1,]) 
summary(m.spfix)

species.fixed <- coef(m.spfix)

# First coef is ACEPEN under short days, cool temps
mean(dx[dx$nl ==1 & dx$sp=='ACEPEN' & dx$warm==15 & dx$photo == '08', 'lday'])

# Effect of warming for ACEPEN, 4°
coef(m.spfix)[2] 

# Effect for other species
sp.fix.coef.warm <- vector()
for(i in unique(dx$sp)){
	xx <- grep(paste("warm20:sp", i, sep=""), names(species.fixed))
	if(length(xx)>0) sp.fix.coef.warm <- c(sp.fix.coef.warm, species.fixed[xx])
	}

sp.fix.coef.cool <- vector()
for(i in unique(dx$sp)){
	xx <- grep(paste("warm15:sp", i, sep=""), names(species.fixed))
	if(length(xx)>0) sp.fix.coef.cool <- c(sp.fix.coef.cool, species.fixed[xx])
	}

# add back in first level of sp (Ace pen) modified by first level of warming (warm20)

spwarminter <- species.fixed[2]
sp.fix.coef.warm1 <- c(spwarminter, 
	( (species.fixed[1]+species.fixed[2])+sp.fix.coef.warm ) - (species.fixed[1]+sp.fix.coef.cool ))


# To get photo effect, change order of interactions with species!
m.spfix <- lm(lday ~ warm*photo + photo:sp +warm:sp 
						#+ site * chill
						, data = dx[dx$nl == 1,]) 
summary(m.spfix)
species.fixed <- coef(m.spfix)
	
sp.fix.coef.photo <- vector()
for(i in unique(dx$sp)){
	xx <- grep(paste("^photo12:sp", i, sep=""), names(species.fixed)) # carrot to mark start of string, exclude interactions
	if(length(xx)>0) sp.fix.coef.photo <- c(sp.fix.coef.photo, species.fixed[xx])
	}
	
sp.fix.coef.photoshort <- vector()
for(i in unique(dx$sp)){
	xx <- grep(paste("^photo08:sp", i, sep=""), names(species.fixed)) # carrot to mark start of string, exclude interactions
	if(length(xx)>0) sp.fix.coef.photoshort <- c(sp.fix.coef.photoshort, species.fixed[xx])
	}

# add back in first level of sp (Ace pen) modified by first level of photo (photo12)

spphotointer <- species.fixed[3]
sp.fix.coef.photo1 <- c(spphotointer, 
	( (species.fixed[1]+species.fixed[3])+sp.fix.coef.photo ) - (species.fixed[1]+sp.fix.coef.photoshort ))


# Plot sensitivity to warming vs photo period on this basis
par(mfrow=c(1,2))
plot(sp.fix.coef.photo1, sp.fix.coef.warm1, 
	xlim = c(-33, -3),
	ylim = c(-33, -3),
	pch = "+",
	col = "grey10",
	xlab = "Days earlier to leafout with 4 hr longer day",
	ylab = "Days earlier to leafout with 5° warmer temps",
	main = "Simple fixed effect model")
text(sp.fix.coef.photo1, sp.fix.coef.warm1, 
	labels = unique(dx$sp), cex = 0.6, pos = 1,
	col = alpha('grey20', 0.8))


abline(a=0, b = 1, lty = 3, col = 'midnightblue')


# species in random

m.spran <- lmer(lday ~ (warm * photo | sp)
				+ (site*chill | sp)
				, data = dx[dx$nl == 1,]) 
summary(m.spran)
species.rand <- ranef(m.spran)$sp



sp.ran.coef.warm = ranef(m.spran)$sp[,2]
sp.ran.coef.photo = ranef(m.spran)$sp[,3]


## plotting random effects version
plot(sp.ran.coef.photo, sp.ran.coef.warm, 
	xlim = c(-33, -3),
	ylim = c(-33, -3),
	pch = "+",
	col = "grey10",
	xlab = "Days earlier to leafout with 4 hr longer day",
	ylab = "Days earlier to leafout with 5° warmer temps",
	main = "Random effects model")
text(sp.ran.coef.photo, sp.ran.coef.warm, 
	labels = unique(dx$sp), cex = 0.6, pos = 1,
	col = alpha('grey20', 0.8))


abline(a=0, b = 1, lty = 3, col = 'midnightblue')



pdf("graphs/Comparing_species_fixed_v_random1.pdf", height = 8, width = 15)

n = tapply(dx$warm, dx$sp, length)	
# scaled by sample size
## Plot sensitivity to warming vs photo period on this basis
par(mfrow=c(1,2))
plot(sp.fix.coef.photo1, sp.fix.coef.warm1, 
	xlim = c(-33, -3),
	ylim = c(-33, -3),
	pch = 16,
	col = alpha("midnightblue", 0.4),
	cex = sqrt(n)/5,
	xlab = "Days earlier to leafout with 4 hr longer day",
	ylab = "Days earlier to leafout with 5° warmer temps",
	main = "Simple fixed effect model")
text(sp.fix.coef.photo1, sp.fix.coef.warm1, 
	labels = unique(dx$sp), cex = 0.7, pos = 1,
	col = alpha('grey20', 0.8))

abline(a=0, b = 1, lty = 3, col = 'midnightblue')

## plotting random effects version
plot(sp.ran.coef.photo, sp.ran.coef.warm, 
	xlim = c(-33, -3),
	ylim = c(-33, -3),
	pch = 16,
	col = alpha("midnightblue", 0.4),
	cex = sqrt(n)/5,
	xlab = "Days earlier to leafout with 4 hr longer day",
	ylab = "Days earlier to leafout with 5° warmer temps",
	main = "Random effects model")
text(sp.ran.coef.photo, sp.ran.coef.warm, 
	labels = unique(dx$sp), cex = 0.7, pos = 1,
	col = alpha('grey20', 0.8))


abline(a=0, b = 1, lty = 3, col = 'midnightblue')





# Plotting fixed versus random
#pdf("graphs/Comparing_species_fixed_v_random.pdf", height = )

#par(mfrow=c(1,2))
plot(sp.fix.coef.warm1, sp.ran.coef.warm,
	xlim = c(-33, -3),
	ylim = c(-33, -3),
	xlab = "Species as Fixed",
	ylab = "Species as Random",
	main = "Days earlier to leafout by warming x species",
	pch = 16,
	cex = sqrt(n)/5,
	col = alpha("midnightblue", 0.3)
	)

text(sp.fix.coef.warm1, sp.ran.coef.warm,
	labels = rownames(species.rand),
	srt = -45, 
	cex = 0.7
	)
abline(a=0, b = 1, lty = 3, col = 'midnightblue')

plot(sp.fix.coef.photo1, sp.ran.coef.photo,
	xlim = c(-23, -3),
	ylim = c(-23, -3),
	xlab = "Species as Fixed",
	ylab = "Species as Random",
	main = "Days earlier to leafout by photoperiod x species",
	pch = 16,
	cex = sqrt(n)/5,
	col = alpha("midnightblue", 0.3)
	)

text(sp.fix.coef.photo1, sp.ran.coef.photo,
	labels = rownames(species.rand),
	srt = -45, 
	cex = 0.7
	)
abline(a=0, b = 1, lty = 3, col = 'midnightblue')

dev.off(); system('open graphs/Comparing_species_fixed_v_random1.pdf -a /Applications/Preview.app')


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Repeat, with days to bb instead of change

# number of individuals per species, per site

head(d)

unique(d$ind)

# total number of clippings
length(unique(d$id))

du <- d[!duplicated(d$ind),]

table(du$sp, du$site)

range(d$Date)

# photo vs temp sens by chilling

m31 <- lmer(lday ~ warm * photo * site + (warm|sp) + (photo|sp), data = dx1[dx1$chill == 'chill0',])

summary(m31)



m31.c1 <- lmer(lday ~ warm * photo * site + (warm|sp) + (photo|sp), data = dx1[dx1$chill == 'chill1',])
summary(m31.c1)


m31.c2 <- lmer(lday ~ warm * photo * site + (warm|sp) + (photo|sp), data = dx1[dx1$chill == 'chill2',])
summary(m31.c2)

# Plot m31

par(mfrow=c(3,1))

plot(ranef(m31)$sp[,1],ranef(m31)$sp[,3],
	pch = "+", col = "grey10",
	type = "n",
	xlab = "Warming response",
	ylab = "Photoperiod response",
	xlim = c(-18, 18),
	ylim = c(-18, 18),
	main = "No chilling"
	)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
points(ranef(m31)$sp[,1],ranef(m31)$sp[,3],
	pch = "+", col = "grey10")
text(ranef(m31)$sp[,1],ranef(m31)$sp[,3], 
	labels = rownames(ranef(m31)$sp), cex = 0.8, pos = 1,
	col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))



plot(ranef(m31.c1)$sp[,1],ranef(m31.c1)$sp[,3],
	pch = "+", col = "grey10",
	type = "n",
	xlab = "Warming response",
	ylab = "Photoperiod response",
	xlim = c(-18, 18),
	ylim = c(-18, 18),
	main = "4° Chill"
	)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
points(ranef(m31.c1)$sp[,1],ranef(m31.c1)$sp[,3],
	pch = "+", col = "grey10")


text(ranef(m31.c1)$sp[,1],ranef(m31.c1)$sp[,3], 
	labels = rownames(ranef(m31.c1)$sp), cex = 0.6, pos = 1,
	col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))


plot(ranef(m31.c2)$sp[,1],ranef(m31.c2)$sp[,3],
	pch = "+", col = "grey10",
	type = "n",
	xlab = "Warming response",
	ylab = "Photoperiod response",
	xlim = c(-18, 18),
	ylim = c(-18, 18),
	main = "1.5° Chill"
	)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
points(ranef(m31.c2)$sp[,1],ranef(m31.c2)$sp[,3],
	pch = "+", col = "grey10")

text(ranef(m31.c2)$sp[,1],ranef(m31.c2)$sp[,3], 
	labels = rownames(ranef(m31.c2)$sp), cex = 0.6, pos = 1,
	col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))


dev.print(file = "graphs/ranefs.pdf", height= 11, width = 8, device = pdf)


# Repeat, on same graph


par(mfrow=c(1,1))

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
#text(ranef(m31)$sp[,1],ranef(m31)$sp[,3], 
#	labels = rownames(ranef(m31)$sp), cex = 0.8, pos = 1,
#	col = alpha('grey20', 0.8))
abline(h=0, lty = 3, col = alpha('darkblue', 0.5))
abline(v=0, lty = 3, col = alpha('darkblue', 0.5))

points(ranef(m31.c1)$sp[,1],ranef(m31.c1)$sp[,3],
	pch = "+", col = "orange")

points(ranef(m31.c2)$sp[,1],ranef(m31.c2)$sp[,3],
	pch = "+", col = "blue")

legend("topleft", pch = "+", col = c("grey10","orange","blue"), legend = c("No additional chilling", "4°C Chilling", "1.5°C Chilling"), bty = "n", cex = 0.8)

dev.print(file = "graphs/chilling_ranefs.pdf", height= 8, width = 8, device = pdf)
system("open graphs/chilling_ranefs.pdf -a /Applications/Preview.app")

### now modeling chilling explicitly

m4 <- lmer(lday ~ warm * photo * site + chill:warm + chill:photo + chill:site + (warm|sp) + (photo|sp) + (chill|sp), data = dx1)
summary(m4)

ranef(m4)


########  
m.spran <- lmer(lday ~ (warm * photo | sp)
				+ site*chill
				, data = dx[dx$nl == 1,]) 
summary(m.spran)
sjp.lmer(m.spran)
