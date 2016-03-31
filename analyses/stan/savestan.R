savestan <- function(suffix=NULL) {
  
  tosave <- which(
		sapply(ls(envir=.GlobalEnv), function(x) class(get(x)))
				=="stanfit" |
		  sapply(ls(envir=.GlobalEnv), function(x) class(get(x)))
		=="shinystan" 
		  )
  suffix = c(suffix, " ")
	save(file=paste("Stan Output ", suffix, Sys.Date(), ".RData", sep=""), list = ls(envir=.GlobalEnv)[tosave])
	
	} 