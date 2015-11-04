# Where were the non-leafout cuttings, by species, site, and treatement?
library(scales)
library(gplots) # for textplot()

setwd("~/Documents/git/buds/analyses") # setwd("~/Documents/git/projects/treegarden/budburstexp2015/analyses")

# get latest data
print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))

source("source/simpleplot.R")

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check

# Analysis of where the non-leafout cuttings were
nlx <- dx[dx$nl == 0,]

summary(nlx)

nl1 <- as.data.frame(table(nlx$sp, nlx$site))

nl2 <- as.data.frame(table(nlx$warm, nlx$photo, nlx$chill))
# proportional to total numbers in each
dl2 <- as.data.frame(table(dx$warm, dx$photo, dx$chill))
nl2$prop <- nl2$Freq/dl2$Freq
nl3 <- as.data.frame(table(nlx$sp, nlx$site,nlx$warm, nlx$photo, nlx$chill))
dl3 <- as.data.frame(table(dx$sp, dx$site, dx$warm, dx$photo, dx$chill))
nl3$prop <- nl3$Freq/dl3$Freq
nl3$prop[is.nan(nl3$prop)==TRUE] <- 0
    
names(nl1) <- c("sp","site","freq")
names(nl2) <- c("warm","photo","chill","freq", "prop")
names(nl3) <- c("sp", "site", "warm","photo","chill","freq", "prop")

nl3.nochill <- subset(nl3, chill=="chill0")
nl3.1chill <- subset(nl3, chill=="chill1")
nl3.2chill <- subset(nl3, chill=="chill2")

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
pdf(file="graphs/simpleplots/nonleafouts_byspp.pdf", 10, 6, paper="a4r", onefile=TRUE)
for (i in c(1:length(spp))){
    spdf <- subset(nl3.nochill, sp==spp[i])
    makesimpleplot.sp(spdf, c(0, 1), "prop", "% non-leafout", spp[i])
}
dev.off()


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Simple models by species. All predictors now numeric

dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)
dx$photo <- as.numeric(dx$photo)
dx$warm <- as.numeric(dx$warm)

pdf(file="graphs/simpleplots/nonleafouts_byspp_model.pdf", height = 10, width = 10)

par(cex=0.7, xpd=TRUE, xaxt="n")
layout(matrix(c(1, 2, 3, 3), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

	makesimpleplot.sp(nl3[nl3$sp ==i,], c(0, 1), "prop", "% non-leafout", i)

	# is this species across site and chill?
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) > 1)  {
			mx <- glm(nl ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)

			} 
	# Across site but no chill?		
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) == 1)  {
			mx <- glm(nl ~ warm + photo + site 
							+ warm:photo +  warm:site + photo:site
							+ warm:photo:site 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
			} 
	# One site, no chill?		
	if(length(unique(dx[dx$sp ==i,"site"])) == 1 & length(unique(dx[dx$sp ==i,"chill"])) == 1)  {
		mx <- glm(nl ~ warm + photo +  
							+ warm:photo 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
			} 
					
  	
	textplot(round(coef(summary(mx)),3))
		
		}
		
		
dev.off(); system('open graphs/simpleplots/nonleafouts_byspp_model.pdf -a /Applications/Preview.app')

# examination code
with(dx[dx$sp == i,], table(warm, photo, site, chill))
with(dx[dx$sp == i,], tapply(nl, list(warm, photo, site, chill), mean))

# Repeat, with simple model for all. Aronia: 1 nl occured in each of the four combinations of photo and warm, no separation.
dx$warm <- as.numeric(as.character(dx$warm))
dx$photo <- as.numeric(as.character(dx$photo))

pdf(file="graphs/simpleplots/nonleafouts_byspp_simplemodel.pdf", height = 10, width = 10)

par(cex=0.7, xpd=TRUE, xaxt="n")
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

	makesimpleplot.sp(nl3[nl3$sp ==i,], c(0, 1), "prop", "% non-leafout", i)


		mx <- glm(nl ~ warm + photo  
							+ warm:photo 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
					
  	textplot(round(coef(summary(mx)),3))
	
	if(coef(summary(mx))[4,4] <= 0.05){
	with(dx[dx$sp == i,], interaction.plot(warm, photo, nl)) 
	} else { plot(1:10, type = "n", bty = "n", yaxt="n", xaxt="n",ylab="",xlab="") }

		
	}
			
dev.off(); system('open graphs/simpleplots/nonleafouts_byspp_simplemodel.pdf -a /Applications/Preview.app')

# Focus on site x chill

# Tally sig site * chill effects

pdf(file="graphs/simpleplots/nonleafouts_sitechill.pdf", height = 10, width = 10)

par(cex=0.7)
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){


	# is this species across site and chill?
	if(length(unique(dx[dx$sp ==i,"site"])) > 1 & length(unique(dx[dx$sp ==i,"chill"])) > 1)  {
		xx <- dx[dx$sp==i,]
		
		means <- with(xx, tapply(nl, list(chill, site), mean, na.rm=T))
		sds <- with(xx, tapply(nl, list(chill, site), sd, na.rm=T))

		plot(1:3, means[,1], ylim = c(0, 1.25), main = paste(i, "HF"), pch = 16, ylab = "prop leafout", xaxt="n")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,1]-sds[,1], 1:3, means[,1]+sds[,1], length = 0)

		plot(1:3, means[,2], ylim = c(0, 1.25), main = "SH", pch = 16, ylab = "prop leafout", xaxt="n")
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
		
		
dev.off(); system('open graphs/simpleplots/nonleafouts_sitechill.pdf -a /Applications/Preview.app')


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Across species
	m1 <- glm(nl ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							, family=binomial(link='logit'), data = dx
							)
	summary(m1)						

# across species, with partial pooling
library(lme4)
m2 <- glmer(nl ~ warm + photo + chill + site 
							+ warm:photo + warm:chill + warm:site 
							+ photo:chill + photo:site
							+ warm:photo:chill
							+ warm:photo:site 
							+ warm:chill:site 
							+ photo:chill:site
							+ (1|sp),
							, family=binomial(link='logit'), data = dx
							)
library(sjPlot)

sjp.glmer(m2)
sjp.glmer(m2, type = "fe")

# Basically, we can't say much about nonleafouts. Let's shift to leafouts

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
interact

# Within individual species, should match nl story


pdf(file="graphs/leafoutday_byspp_simplemodel.pdf", height = 10, width = 10)

par(cex=0.7, xpd=TRUE, xaxt="n")
layout(matrix(c(1, 2, 3, 4), byrow=T, ncol = 2, nrow = 2), heights = c(3, 2))
for(i in sort(unique(dx$sp))){

		xx <- dx[dx$sp==i,]
		
		means <- with(xx, tapply(lday, list(warm, photo, site), mean, na.rm=T))
		sds <- with(xx, tapply(lday, list(warm, photo, site), sd, na.rm=T))

		plot(1:3, means[,1], ylim = c(0, 1.25), main = paste(i, "HF"), pch = 16, ylab = "prop leafout", xaxt="n")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,1]-sds[,1], 1:3, means[,1]+sds[,1], length = 0)

		plot(1:3, means[,2], ylim = c(0, 1.25), main = "SH", pch = 16, ylab = "prop leafout", xaxt="n")
		axis(1, at=1:3, labels = c("chill0", "chill1","chill2"))		
		arrows(1:3, means[,2]-sds[,2], 1:3, means[,2]+sds[,2], length = 0)


		mx <- glm(nl ~ warm + photo  
							+ warm:photo 
							, family=binomial(link='logit'), data = dx[dx$sp == i,]
							)
					
  	textplot(round(coef(summary(mx)),3))
	
	if(coef(summary(mx))[4,4] <= 0.05){
	with(dx[dx$sp == i,], interaction.plot(warm, photo, nl)) 
	} else { plot(1:10, type = "n", bty = "n", yaxt="n", xaxt="n",ylab="",xlab="") }

		
	}
			
dev.off(); system('open graphs/simpleplots/nonleafouts_byspp_simplemodel.pdf -a /Applications/Preview.app')



# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# colors for plotting
cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)

bb <- barplot(nl2$prop*100, ylab = "% of cuttings non-leafout", 
	ylim = c(0,50), 
	col = rep(cols, each = 4),
	main = "Percent of cuttings in each treatment which failed to leaf out",
	xlab = "Treatment combination", sub = "(temp + photo + chill)")
mtext(paste(nl2$warm, nl2$photo, nl2$chill, sep = "\n"), 1, line = -2, padj = 0, at = bb[,1])

chi.all <- summary(xtabs(~ warm+photo+chill, data = nl))
for(i in levels(nl$chill)) print(chisq.test(table(nl[nl$chill==i,]$warm, nl[nl$chill==i,]$photo))) # NS within chill1 or chill2, surprisingly

xx <- round(chi.all$statistic,2)
text(1, 45, substitute(paste(chi^2,"=", xx), list(xx=round(chi.all$statistic,2))))
text(1, 42, substitute(paste(p,"=", xx), list(xx=round(chi.all$p.value,4))))
dev.print(file = "graphs/nonleafout.pdf", device = pdf, width = 10, height = 6); system('open ./graphs/nonleafout.pdf -a /Applications/Preview.app')

# looping this same figure now for each species separately.

pdf(file="graphs/nonleafout_eachsp.pdf", width = 10, height = 6)
for(i in unique(nl$sp)){

	nlx <- with(nl[nl$sp==i,], as.data.frame(table(warm, photo, chill)))
	# proportional to total numbers in each
	dlx <- with(dx[dx$sp==i,], as.data.frame(table(warm, photo, chill)))
	nlx$prop <- nlx$Freq/dlx$Freq
	
	bb <- barplot(nlx$prop*100, ylab = "% of cuttings non-leafout", 
		ylim = c(0,100), 
		col = rep(cols, each = 4),
		main = paste(i, "\n Percent of cuttings in each treatment which failed to leaf out"),
		xlab = "Treatment combination")
	mtext(paste(nlx$warm, nlx$photo, nlx$chill, sep = "\n"), 1, line = 2, padj = 0, at = bb[,1])
	
	if(length(unique(as.character(nl[nl$sp==i,"chill"])))==1){
		chi.all <- summary(xtabs(~ warm+photo, data = nl[nl$sp==i,]))	
		}
	else {	chi.all <- summary(xtabs(~ warm+photo+chill, data = nl[nl$sp==i,]))	 }
	xx <- round(chi.all$statistic,2)
	text(1, 95, substitute(paste(chi^2,"=", xx, ", df =", df), list(xx=round(chi.all$statistic,2), df = chi.all$parameter)))
	text(1, 85, substitute(paste(p,"=", xx), list(xx=round(chi.all$p.value,4))))

}

dev.off(); system('open ./graphs/nonleafout_eachsp.pdf -a /Applications/Preview.app')


# also by species and site

nl1 <- as.data.frame(table(nl$sp, nl$site))
# proportional to total numbers in each
dl1 <- as.data.frame(table(dx$sp, dx$site))
nl1$prop <- nl1$Freq/dl1$Freq


bb <- barplot(height = rbind(nl1$prop[1:28]*100, nl1$prop[29:56]*100),
	beside = T,
	legend.text = c("HF","SH"),
	args.legend = list(bty="n"),
	space = c(0.05, 1),
	ylab = "% of cuttings non-leafout", 
	ylim = c(0,100), 
	main = "Percent of cuttings in each species which failed to leaf out"
	)
par(xpd=T)
text(bb[1,], -3, nl1$Var1[1:28], srt = 45, adj = 1, cex = 0.8)

dev.print(file = "graphs/nonleafout_species.pdf", device = pdf, width = 10, height = 5); system('open ./graphs/nonleafout_species.pdf -a /Applications/Preview.app')

# analyze now in logistic framework
dx$nl <- as.numeric(as.character(cut(dx$lday, breaks = c(0, 74, 100), labels = c(1, 0)))) # 1: leafed out. 0: failed to leaf out

summary(m1 <- glm(nl ~ warm + photo + chill + site, family=binomial(link='logit'), data = dx)) # overall strong effects of warming, long day, and chilling

summary(m1 <- glm(nl ~ as.numeric(warm), family=binomial(link='logit'), data = dx)) # just warming for plotting
plot(as.numeric(dx$warm), dx$nl, xlab="Temperature", 
     ylab="Probability of Response")
curve(predict(m1, data.frame(warm=x), type="resp"), 
      add=TRUE, col="red")

summary(glm(nl ~ warm + photo + chill + site + sp, family=binomial(link='logit'), data = dx))

summary(glm(nl ~ warm + photo + chill + site + sp +
						warm:photo + warm:chill + photo:chill,
						family=binomial(link='logit'), data = dx))

summary(glm(nl ~ site + sp + sp:site, family=binomial(link='logit'), data = dx)) # no overall site effect, some acesac and franig interax by site, more non-leafouts in HF

summary(glm(nl ~ warm + photo + chill + site + sp +
						warm:photo + warm:chill + photo:chill + 
						warm:sp + photo:sp + chill:sp,
						family=binomial(link='logit'), data = dx))	# clear species effects, interax with warm x photo, very few sp-specific responses to warming or photo. Querub improved with chilling.					


