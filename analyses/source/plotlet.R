plotlet <- function(x, y, xlab=NULL, ylab=NULL, dat, groups = NULL){
  
  if(is.null(xlab)) xlab = x
  if(is.null(ylab)) ylab = y
  
  minmax = range(c(dat[grep(paste(x,"\\[",sep=""), rownames(dat)),1], dat[grep(paste(y,"\\[",sep=""), rownames(dat)),1]))
  
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
  else {
    colz = c("midnightblue", "darkgreen")
    ccolz = rep(colz[1], length(groups))
    ccolz[groups == 2] = colz[2]
    col.pch = ccolz
    col.lines = alpha(ccolz, 0.4)
  }
  
    plot(
    dat[grep(paste(x,"\\[",sep=""), rownames(dat)),1],
    dat[grep(paste(y,"\\[",sep=""), rownames(dat)),1],
    pch = "+",
    xlim = c(floor(minmax)[1], ceiling(minmax)[2]),
    ylim = c(floor(minmax)[1], ceiling(minmax)[2]),
    ylab = ylab,
    xlab = xlab,
    col = col.pch
  )
  
  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    dat[grep(paste(x,"\\[",sep=""), rownames(dat)),"mean"],
    dat[grep(paste(y,"\\[",sep=""), rownames(dat)),"25%"],
    dat[grep(paste(x,"\\[",sep=""), rownames(dat)),"mean"],
    dat[grep(paste(y,"\\[",sep=""), rownames(dat)),"75%"],
    length = 0, col = col.lines)
  
  arrows(
    dat[grep(paste(x,"\\[",sep=""), rownames(dat)),"25%"],
    dat[grep(paste(y,"\\[",sep=""), rownames(dat)),"mean"],
    dat[grep(paste(x,"\\[",sep=""), rownames(dat)),"75%"],
    dat[grep(paste(y,"\\[",sep=""), rownames(dat)),"mean"],
    length = 0, col = col.lines)
  

}
