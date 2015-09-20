# Plotting species doy leafout by site


setwd("~/Documents/git/buds/analyses")

load("input/Budburst Data 2015-09-16")

dx <- dx[!is.na(dx$site),] # one Betpap entry has no site, need to check
dx1 <- dx
dx1[dx1>=75] = NA # without 75's

# all together

bb <- boxplot(lday ~ sp, dx1[dx1$site == 'HF',], 
	boxwex = 0.1,
	ylim = c(5, 75),
	at = 1:length(unique(dx1$sp)) - 0.1,
	col = "grey40",
	cex = 0.5,
	lty = 1,
	ylab = "Leafout day of experiment",
	xaxt="n")
	
boxplot(lday ~ sp, dx1[dx1$site == 'SH',], 
	boxwex = 0.1,
	ylim = c(5, 75),
	at = 1:length(unique(dx1$sp)) + 0.1,
	col = "grey90",
	cex = 0.5,
	lty = 1,
	add = T,
	yaxt = "n",
	xaxt="n")	
par(xpd=T)	

text(1:28, -3, unique(dx1$sp), srt = 45, adj = 1, cex = 0.8)

legend("topleft", fill = c("grey40", "grey90"), legend = c("HF","SH"), bty = "n")

dev.print(file = "graphs/leafout_species.pdf", device = pdf, width = 15, height = 4); system('open ./graphs/leafout_species.pdf -a /Applications/Preview.app')

# now for each species, with all treatments


pdf(paste("Leafout x Species x Treatment", Sys.Date(), ".pdf", sep=""))

par(mfcol=c(3, 4), mar = c(3,3,1,0.5))
for(spx in levels(d$sp)){ # spx = "ACEPEN"

	dxx = dx[dx$sp == spx,]

	counter = 1
	for(i in sort(as.character((unique(dx$treatcode))))){#c("CS0","CL0","WS0","WL0")){
				
				
		boxplot(lday ~ site, dxx[dxx$treatcode == i,], 
			boxwex = 0.5,
			ylim = c(5, 95),
			col = c("grey40", "grey90"),
			cex = 0.5,
			lty = 1,
			frame.plot = F,
			ylab = "Leafout day of experiment"
			)
		legend("topleft",bty="n",i, cex = 0.75, inset = 0)
		if(counter == 1) mtext(spx, line = -2, adj = 0.5)
		counter = counter + 1
		}
	
	}
dev.off()
system(paste("open '", paste("Leafout x Species x Treatment", Sys.Date(), ".pdf", sep=""), "' -a /Applications/Preview.app", sep=""))

# Repeat, without 75s

pdf(paste("Leafout x Species x Treatment Without Non-leafouts", Sys.Date(), ".pdf", sep=""))

par(mfcol=c(3, 4), mar = c(3,3,1,0.5))
for(spx in levels(d$sp)){ # spx = "ACEPEN"

	dxx = dx1[dx1$sp == spx,]

	counter = 1
	for(i in sort(as.character((unique(dx$treatcode))))){#c("CS0","CL0","WS0","WL0")){
				
				
		boxplot(lday ~ site, dxx[dxx$treatcode == i,], 
			boxwex = 0.5,
			ylim = c(5, 95),
			col = c("grey40", "grey90"),
			cex = 0.5,
			lty = 1,
			frame.plot = F,
			ylab = "Leafout day of experiment"
			)
		legend("topleft",bty="n",i, cex = 0.75, inset = 0)
		if(counter == 1) mtext(spx, line = -2, adj = 0.5)
		counter = counter + 1
		}
	
	}
dev.off()
system(paste("open '", paste("Leafout x Species x Treatment Without Non-leafouts", Sys.Date(), ".pdf", sep=""), "' -a /Applications/Preview.app", sep=""))
