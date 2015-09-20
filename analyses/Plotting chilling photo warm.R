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


