# Analyzing phenology first work March 2015. Script also for Tim to work with R


# Setup: load packages, set working directory, read in the data.

#library(gdata)
#library(nlme)
library(scales)
#library(arm)

setwd("~/Documents/git/buds/analyses") # setwd("~/Documents/git/projects/treegarden/budburstexp2015/analyses")

# get latest data


load("input/Budburst Data 2015-10-28")



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



