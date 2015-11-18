savestan <- function() {
	tosave <- which(
		sapply(ls(), function(x) class(get(x)))
				=="stanfit")
	save(file=paste("Stan Output ", Sys.Date(), ".RData", sep=""), list = c(ls()[tosave]))
	} 