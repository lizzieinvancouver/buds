# Where were the non-leafout cuttings, by species, site, and treatement?
library(scales)

setwd("~/Documents/git/buds/analyses")

load("input/Budburst Data 2015-09-16")

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check

# Analysis of where the non-leafout cuttings were

nl <- dx[dx$lday >=75,]

summary(nl)

nl1 <- as.data.frame(table(nl$sp, nl$site))

nl2 <- as.data.frame(table(nl$warm, nl$photo, nl$chill))
# proportional to total numbers in each
dl2 <- as.data.frame(table(dx$warm, dx$photo, dx$chill))
nl2$prop <- nl2$Freq/dl2$Freq

# colors for plotting
cols = alpha(c("darkseagreen", "deepskyblue", "slateblue"), 0.5)

bb <- barplot(nl2$prop*100, ylab = "% of cuttings non-leafout", 
	ylim = c(0,50), 
	col = rep(cols, each = 4),
	main = "Percent of cuttings in each treatment which failed to leaf out",
	xlab = "Treatment combination", sub = "(temp + photo + chill)")
mtext(paste(nl2$Var1, nl2$Var2, nl2$Var3, sep = "\n"), 1, line = -2, padj = 0, at = bb[,1])

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
						,
						family=binomial(link='logit'), data = dx))

summary(glm(nl ~ site + sp + sp:site, family=binomial(link='logit'), data = dx)) # no overall site effect, some acesac and franig interax by site, more non-leafouts in HF

summary(glm(nl ~ warm + photo + chill + site + sp +
						warm:photo + warm:chill + photo:chill + 
						warm:sp + photo:sp + chill:sp,
						family=binomial(link='logit'), data = dx))	# clear species effects, interax with warm x photo, very few sp-specific responses to warming or photo. Querub improved with chilling.					

