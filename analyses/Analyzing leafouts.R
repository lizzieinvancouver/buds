# Where were the leafout cuttings, by species, site, and treatement? Analogous to non-leafout script earlier.
library(scales)
library(gplots) # for textplot()
library(lme4)
library(sjPlot)

setwd("~/Documents/git/buds/analyses") # setwd("~/Documents/git/projects/treegarden/budburstexp2015/analyses")

# get latest data
print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))

source("source/simpleplot.R")

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check

# Analysis of where the leafout cuttings were
lx <- dx[dx$nl == 1,]

summary(lx)

nl1 <- as.data.frame(table(lx$sp, lx$site))

nl2 <- as.data.frame(table(lx$warm, lx$photo, lx$chill))
# proportional to total numbers in each
dl2 <- as.data.frame(table(dx$warm, dx$photo, dx$chill))
nl2$prop <- nl2$Freq/dl2$Freq
nl3 <- as.data.frame(table(lx$sp, lx$site,lx$warm, lx$photo, lx$chill))
dl3 <- as.data.frame(table(dx$sp, dx$site, dx$warm, dx$photo, dx$chill))
nl3$prop <- nl3$Freq/dl3$Freq
nl3$prop[is.nan(nl3$prop)==TRUE] <- 0
    
names(nl1) <- c("sp","site","freq")
names(nl2) <- c("warm","photo","chill","freq", "prop")
names(nl3) <- c("sp", "site", "warm","photo","chill","freq", "prop")

nl3.nochill <- subset(nl3, chill=="chill0")
nl3.1chill <- subset(nl3, chill=="chill1")
nl3.2chill <- subset(nl3, chill=="chill2")

#
data.frame(sort(with(nl3, tapply(prop, sp, mean)), T))
with(nl3, tapply(prop, chill, mean))
with(nl3, tapply(prop, site, mean))

# make some simple plots
# # makesimpleplot(nl3, c(0, 0.4), "prop", "% non-leafout") # all chilling combined
# makesimpleplot(nl3.nochill, c(0, 0.4), "prop", "% non-leafout")
# makesimpleplot(nl3.1chill, c(0, 0.4), "prop", "% non-leafout")
# makesimpleplot(nl3.2chill, c(0, 0.4), "prop", "% non-leafout")

sitespp <- as.data.frame(table(nl3$sp, nl3$site))
sitespp <- subset(sitespp, Freq>0)
sppatsites <- aggregate(sitespp["Var2"], sitespp["Var1"], FUN=length)
sppatbothsites <- subset(sppatsites, Var2>1)

spp <- sppatbothsites$Var1
pdf(file="graphs/simpleplots/leafouts_byspp.pdf", 10, 6, paper="a4r", onefile=TRUE)
par(mfrow=c(1,2))
for (i in c(1:length(spp))){
    spdf <- subset(nl3.nochill, sp==spp[i])
    makesimpleplot.sp(spdf, c(0, 1), "prop", "% leafout", spp[i])
}
dev.off()


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Simple models by species. All predictors now numeric. Using lday as response, not nl as in nonleafout script.

dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)
dx$photo <- as.numeric(dx$photo)
dx$warm <- as.numeric(dx$warm)

ldayx <- with(dx, aggregate(lday ~ warm + photo+ site+chill + sp, FUN = mean, na.rm=T))

pdf(file="graphs/simpleplots/leafoutday_byspp_model.pdf", height = 10, width = 10)

par(cex=0.7, xpd=TRUE, xaxt="n")
layout(matrix(c(1, 2, 3, 3), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

	makesimpleplot.sp.lday(ldayx[ldayx$sp == i,], c(0, 70), "lday", "Leafout day", i)

	# is this species across site and chill?
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) > 1)  {
			mx <- glm(lday ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							, data = dx[dx$sp == i,]
							)

			} 
	# Across site but no chill?		
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) == 1)  {
			mx <- glm(lday ~ warm + photo + site 
							+ warm:photo +  warm:site + photo:site
							+ warm:photo:site 
							, data = dx[dx$sp == i,]
							)
			} 
	# One site, no chill?		
	if(length(unique(dx[dx$sp ==i,"site"])) == 1 & length(unique(dx[dx$sp ==i,"chill"])) == 1)  {
		mx <- glm(lday ~ warm + photo +  
							+ warm:photo 
							, data = dx[dx$sp == i,]
							)
			} 
					
  	
	textplot(round(coef(summary(mx)),3))
		
		}
		
		
dev.off(); system('open graphs/simpleplots/leafoutday_byspp_model.pdf -a /Applications/Preview.app')


# Repeat, with simple model for all. 
dx$warm <- as.numeric(as.character(dx$warm))
dx$photo <- as.numeric(as.character(dx$photo))

pdf(file="graphs/simpleplots/leafouts_byspp_simplemodel.pdf", height = 10, width = 10)

par(cex=0.7, xpd=T)
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

	makesimpleplot.sp.lday(ldayx[ldayx$sp == i,], c(0, 80), "lday", "Leafout day", i)


		mx <- glm(lday ~ warm + photo  
							+ warm:photo 
							, data = dx[dx$sp == i,]
							)
					
  	textplot(round(coef(summary(mx)),3))
	
	if(coef(summary(mx))[4,4] <= 0.05){
	with(dx[dx$sp == i,], interaction.plot(warm, photo, nl)) 
	} else { plot(1:10, type = "n", bty = "n", yaxt="n", xaxt="n",ylab="",xlab="") }

		
	}
			
dev.off(); system('open graphs/simpleplots/leafouts_byspp_simplemodel.pdf -a /Applications/Preview.app')

# Focus on site x chill

# Tally sig site * chill effects

pdf(file="graphs/simpleplots/leafouts_sitechill.pdf", height = 10, width = 10)

par(cex=0.7)
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){


	# is this species across site and chill?
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) > 1)  {
		xx <- dx[dx$sp==i,]
		
		means <- with(xx, tapply(lday, list(chill, site), mean, na.rm=T))
		sds <- with(xx, tapply(lday, list(chill, site), sd, na.rm=T))

		plot(1:3, means[,1], ylim = c(0, 75), main = paste(i, "HF"), pch = 16, ylab = "prop leafout", xaxt="n", xlab = "")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,1]-sds[,1], 1:3, means[,1]+sds[,1], length = 0)

		plot(1:3, means[,2], ylim = c(0, 75), main = "SH", pch = 16, ylab = "prop leafout", xaxt="n", xlab = "")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,2]-sds[,2], 1:3, means[,2]+sds[,2], length = 0)

				
			mx <- glm(nl ~ chill * site 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)

		textplot(round(coef(summary(mx)),3))


		if(coef(summary(mx))[4,4] <= 0.05){
			with(dx[dx$sp == i,], interaction.plot(chill, site, nl)) 
			} else { plot(1:10, type = "n", bty = "n", yaxt="n", xaxt="n",ylab="",xlab="") }

		 		
	
		}
		}
		
		
dev.off(); system('open graphs/simpleplots/leafouts_sitechill.pdf -a /Applications/Preview.app')

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Across species
	l1 <- lm(lday ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							, data = dx[dx$nl==1,]
							)
	summary(l1)						
	summary.aov(l1)						
# With partial pooling

l2 <- lmer(lday ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							+ (1|sp)
							, data = dx[dx$nl==1,]
							)
	summary(l2)						
sjp.lmer(l2)						
sjp.lmer(l2, type = "fe")						

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

