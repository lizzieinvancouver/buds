
# Load all my favorite packages

library(gdata)
library(lattice)
library(geometry)
library(vegan)
library(reshape)

# Functions

se <- function(x, na.rm=T){	
	if(na.rm){
		 (sd(x, na.rm=T)/sqrt(length(x[!is.na(x)])-1))
		} 
	else (sd(x)/sqrt(length(x)-1))
	}

do.lm <- function(xvar, yvar, data=NULL, display.sum = F, man.labels = F) {
	# Can label the points by entering in a "data" object -- this should have the labels
	# in the first column (not row names). display.sum just reports the regression results.
	# If man.labels is true, will not plot x and y labels, and the user then adds them manually. This is useful for loops.

	if (man.labels == FALSE) {
		plot(xvar, yvar,
			pch = 16,
			col = "midnightblue",
			xlab = deparse(substitute(xvar)),
			ylab = deparse(substitute(yvar)),
			xlim = c(min(xvar, na.rm=T), max(xvar, na.rm=T)*1.1)
			) }
		
		else {plot(xvar, yvar,
			pch = 16,
			col = "midnightblue",
			xlab = " ",
			ylab = " ",
			xlim = c(min(xvar, na.rm=T), max(xvar, na.rm=T)*1.1)
			) 
			}

	text(xvar, yvar, 
		labels = names(data),
		pos = 4,
		cex = 0.8 
		)

	lm.result <- lm(yvar ~ xvar)

	abline(lm.result, 
		lty = 2,
		col = "tomato")

	mtext( 
		c(expression(r^2), paste("  =", round(summary(lm.result)$r.squared, 3))),
		side = 3,
		adj = 0,
		line = 1
		)

	fstat <- summary(lm.result)$fstatistic

	if (round(1-pf(fstat[1], fstat[2], fstat[3]), 3) > 0) {
		pval = paste("p =", round(1-pf(fstat[1], fstat[2], fstat[3]), 3)) }
	else 
		pval = "p < 0.001"


	mtext( 
		pval,
		side = 3,
		adj = 1,
		line = 1
		)

	mtext(
		paste("Slope =", 
			round(summary(lm.result)$coefficients[2], 3) ),
		side = 3,
		adj = 1,
		line = 2
		)

	if (display.sum == TRUE){ print(summary(lm.result)) }
}

do.t.test <- function(a, b, man.labels = F, display.sum = F, paired = F) {

	if (man.labels == FALSE) {
		boxplot(a, b,
			col = heat.colors(4),
			names = c(substitute(a), substitute(b))
			) }
	
	else {boxplot(a, b,
		col = heat.colors(4),
		xlab = " ",
		ylab = " "
		) 
		}

	if (paired == TRUE) {
		t.res <- t.test(a,b, paired=T)}
	else 
		t.res <- t.test(a, b)

	if (round(t.res$p.value, 3) > 0) {
		pval = paste("p =", round(t.res$p.value, 3)) }
	else 
		pval = "p < 0.001"

	mtext( 
		pval,
		side = 3,
		adj = 1,
		line = 1
		)

	if (display.sum == TRUE){ print(t.res) }
	
	}
	
	

	
do.lm.loop <- function(x, skip = 0, data.lab = NULL) {
# Loops through a data frame, applying 'do.lm' to every unique pair. If row names are in the first column, enter skip = 1  

for(i in (1+skip):(length(x[1,])-1)){
	for (j in i+1:length(x[1,])){
		if (j < (length(x[1,])+1)) {
	do.lm(x[,i], x[,j], data.lab, man.labels = T)
	title(main = paste(colnames(x)[j], "~", colnames(x)[i]),
		xlab = colnames(x)[i],
		ylab = colnames(x)[j])
			}
		}
	}
}

do.aov <- function(x, y, man.labels = F, display.sum = F) {

	if (man.labels == FALSE) {
		boxplot(y ~ x,
			col = sort(heat.colors(length(unique(x))), T),
#			xlab = substitute(x),
#			ylab = substitute(y)
			)
			} 
	
	else {
		boxplot(y ~ x,
			col = sort(heat.colors(length(unique(x))), T),
			xlab = " ", names = c(rep(" ", length(unique(x)))),
			ylab = " "
			) 
			}

	aov.res <- aov(y ~ x) 

	if (round(unlist(summary(aov.res))[9], 3) > 0) {
		pval = paste("p =", round(unlist(summary(aov.res))[9], 3)) }
	else 
		pval = "p < 0.001"

	mtext( 
		pval,
		side = 3,
		adj = 1,
		line = 1
		)

	if (display.sum == TRUE){ print(summary(aov.res)) }
	}

set.rownames <- function(x){
	rownames(x) <- x[,1]
	x <- x[,-1]
	x
	}

makepdf <- function(filename = "Rplot", width = 6, height = 6, filepath = NULL){
	filename = paste(filename, "pdf", sep=".")
	if(!is.null(filepath)) {
		system(paste("mkdir -p", filepath))
		filename = file.path(filepath, filename)
		}
	dev.copy2pdf(width = width, height = height, file = filename) 
	system(paste("open '", filename, "' -a /Applications/Preview.app", sep=""))
	}

plotblank <- function(){ 
#    op <- par(no.readonly=TRUE)
#    par(mar=rep(0,4))
  plot(1:10, 1:10, type = "n", xlab = "", ylab="", bty="n", xaxt="n", yaxt="n") 
#    par(op)
  }

as.df.by <- function(x) { # where x is a 2D 'by' object, and you want to convert it to a data frame.
	tx <- as.data.frame(matrix(unlist(x), ncol =dim(x), nrow = length(x[[1]])))
	rownames(tx) = names(x[[1]])
	colnames(tx) = dimnames(x)[[1]]
	tx
	}
