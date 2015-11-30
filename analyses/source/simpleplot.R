## Started 5 October 2015 ##
## Make a simple f(x) to plot two factors (warm x photo) across sites ##

## Currently these functions could be re-written so that any y variable would work ##
## This would be a couple tweaks to remove the specific y variables ('prop' in makesimpleplot
# and lday in the other, and adding in the option to change some of that graphing parameters
# from afar, but for now, I am leaving it here. Hope to fix it someday! ##

makesimpleplot <- function(datahere, yrange, yvar, ylabelhere){
    # first get means and errors by photo*warm*site
    nochill.mean <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=mean, na.action = na.exclude)
    nochill.sd <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=sd)
    nochill.n <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=length)
    nochill <- nochill.mean
    nochill$se <- nochill.sd$prop/nochill.n$prop
    # subset down as needed
    nochill.hf.8 <- subset(nochill, site=="HF" & photo=="08")
    nochill.hf.12 <- subset(nochill, site=="HF" & photo=="12")
    nochill.sh.8 <- subset(nochill, site=="SH" & photo=="08")
    nochill.sh.12 <- subset(nochill, site=="SH" & photo=="12")
   # some graph parameters
   f<-1
   x<-c(1.2, 1.8)
   x1<-c(1.2, 1.8)
   xtxt <- c(1,2)
   yrange.use <- yrange
   # Open a blank plot
  # quartz("Quartz", width=4.5, height=3, pointsize=12)
  # par(mfrow=c(1,2), cex=0.7, xpd=TRUE, xaxt="n")
   plot(yrange.use,type="n",
        main="Harvard Forest, USA",
        xlab="",
        ylab=ylabelhere)
    text(xtxt,rep(-0.05,1.5),as.vector(c("15 C", "20 C")), cex=1)
    leg.txt<- c("8 hours", "12 hours")
    legend(1.5,0.41, leg.txt,pch=c(21,22), pt.bg=c("black","white"), bty="n")
    #  all species at Harvard Forest: 8 hours photoperiod
    y <- as.vector(nochill.hf.8$prop) 
    ysem <- as.vector(nochill.hf.8$se)
    points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  all species at Harvard Forest: 12 hours photoperiod
    y <- as.vector(nochill.hf.12$prop) 
    ysem<-as.vector(nochill.hf.12$se)
    points(x1,y,pch=22,bg='white', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  saint hippo: 8 hours
    plot(yrange.use,type="n",
            main="Saint Hippolyte, Qc",
            xlab="",
            ylab= ylabelhere)
        text(xtxt,rep(-0.05,1.2),as.vector(c("15 C", "20 C")), cex=1)
    leg.txt<- c("8 hours", "12 hours")
    legend(1.5,0.41, leg.txt,pch=c(21,22), pt.bg=c("black","white"), bty="n")
    y <- as.vector(nochill.sh.8$prop) 
    ysem <- as.vector(nochill.sh.8$se)
    points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  saint hippo: 12 hours
    y <- as.vector(nochill.sh.12$prop) 
    ysem <- as.vector(nochill.sh.12$se)
    points(x,y,pch=22,bg='white', cex=1.2)
    arrows(x,y-ysem,x,y+ysem,code=3,angle=90,length=0.05)
}


makesimpleplot.sp <- function(datahere, yrange, yvar, ylabelhere, species){
    # first get means and errors by photo*warm*site
    nochill.mean <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=mean, na.action = na.exclude)
    nochill.sd <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=sd)
    nochill.n <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=length)
    nochill <- nochill.mean
    nochill$se <- nochill.sd$prop/nochill.n$prop
    # subset down as needed
    nochill.hf.8 <- subset(nochill, site=="HF" & photo=="08")
    nochill.hf.12 <- subset(nochill, site=="HF" & photo=="12")
    nochill.sh.8 <- subset(nochill, site=="SH" & photo=="08")
    nochill.sh.12 <- subset(nochill, site=="SH" & photo=="12")
   # some graph parameters
   f<-1
   x<-c(1.2, 1.8)
   x1<-c(1.2, 1.8)
   xtxt <- c(1,2)
   yrange.use <- yrange
   # Open a blank plot
   # quartz("Quartz", width=4.5, height=3, pointsize=12)
  # par(mfrow=c(1,2), cex=0.7, xpd=TRUE, xaxt="n")
   plot(yrange.use,type="n",
        main=paste(species, "at Harvard Forest, USA"),
        xlab="",
        ylab=ylabelhere)
    text(xtxt,rep(-0.05,1.5),as.vector(c("15 C", "20 C")), cex=1)
    leg.txt<- c("8 hours", "12 hours")
    legend(1.5,0.74, leg.txt,pch=c(21,22), pt.bg=c("black","white"), bty="n")
    #  all species at Harvard Forest: 8 hours photoperiod
    y <- as.vector(nochill.hf.8$prop) 
    ysem <- as.vector(nochill.hf.8$se)
    points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  all species at Harvard Forest: 12 hours photoperiod
    y <- as.vector(nochill.hf.12$prop) 
    ysem<-as.vector(nochill.hf.12$se)
    points(x1,y,pch=22,bg='white', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  saint hippo: 8 hours
    plot(yrange.use,type="n",
            main="Saint Hippolyte, Qc",
            xlab="",
            ylab=ylabelhere)
        text(xtxt,rep(-0.05,1.2),as.vector(c("15 C", "20 C")), cex=1)
    leg.txt<- c("8 hours", "12 hours")
    legend(1.5,0.74, leg.txt,pch=c(21,22), pt.bg=c("black","white"), bty="n")
    y <- as.vector(nochill.sh.8$prop) 
    ysem <- as.vector(nochill.sh.8$se)
    points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  saint hippo: 12 hours
    y <- as.vector(nochill.sh.12$prop) 
    ysem <- as.vector(nochill.sh.12$se)
    points(x,y,pch=22,bg='white', cex=1.2)
    arrows(x,y-ysem,x,y+ysem,code=3,angle=90,length=0.05)
}


makesimpleplot.lday <- function(datahere, yrange, yvar, ylabelhere){
    # first get means and errors by photo*warm*site
    nochill.mean <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=mean, na.action = na.exclude)
    nochill.sd <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=sd)
    nochill.n <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=length)
    nochill <- nochill.mean
    nochill$se <- nochill.sd$lday/nochill.n$lday
    # subset down as needed
    nochill.hf.8 <- subset(nochill, site=="HF" & photo=="08")
    nochill.hf.12 <- subset(nochill, site=="HF" & photo=="12")
    nochill.sh.8 <- subset(nochill, site=="SH" & photo=="08")
    nochill.sh.12 <- subset(nochill, site=="SH" & photo=="12")
   # some graph parameters
   f<-1
   x<-c(1.2, 1.8)
   x1<-c(1.2, 1.8)
   xtxt <- c(1,2)
   yrange.use <- yrange
   # Open a blank plot
   quartz("Quartz", width=4.5, height=3, pointsize=12)
   par(mfrow=c(1,2), cex=0.7, xpd=TRUE, xaxt="n")
   plot(yrange.use,type="n",
        main="Harvard Forest, USA",
        xlab="",
        ylab=ylabelhere)
    text(xtxt,rep(-0.05,1.5),as.vector(c("15 C", "20 C")), cex=1)
    leg.txt<- c("8 hours", "12 hours")
    legend(1.5,0.41, leg.txt,pch=c(21,22), pt.bg=c("black","white"), bty="n")
    #  all species at Harvard Forest: 8 hours photoperiod
    y <- as.vector(nochill.hf.8$lday) 
    ysem <- as.vector(nochill.hf.8$se)
    points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  all species at Harvard Forest: 12 hours photoperiod
    y <- as.vector(nochill.hf.12$lday) 
    ysem<-as.vector(nochill.hf.12$se)
    points(x1,y,pch=22,bg='white', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  saint hippo: 8 hours
    plot(yrange.use,type="n",
            main="Saint Hippolyte, Qc",
            xlab="",
            ylab= ylabelhere)
        text(xtxt,rep(-0.05,1.2),as.vector(c("15 C", "20 C")), cex=1)
    leg.txt<- c("8 hours", "12 hours")
    legend(1.5,0.41, leg.txt,pch=c(21,22), pt.bg=c("black","white"), bty="n")
    y <- as.vector(nochill.sh.8$lday) 
    ysem <- as.vector(nochill.sh.8$se)
    points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  saint hippo: 12 hours
    y <- as.vector(nochill.sh.12$lday) 
    ysem <- as.vector(nochill.sh.12$se)
    points(x,y,pch=22,bg='white', cex=1.2)
    arrows(x,y-ysem,x,y+ysem,code=3,angle=90,length=0.05)
}


makesimpleplot.sp.lday <- function(datahere, yrange, yvar, ylabelhere, species){
    # first get means and errors by photo*warm*site
    nochill.mean <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=mean, na.action = na.exclude)
    nochill.sd <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=sd)
    nochill.n <- aggregate(datahere[yvar], datahere[c("site", "warm", "photo")],
        FUN=length)
    nochill <- nochill.mean
    nochill$se <- nochill.sd$lday/nochill.n$lday
    # subset down as needed
    nochill.hf.8 <- subset(nochill, site==1 & photo==1)
    nochill.hf.12 <- subset(nochill, site==1 & photo==2)
    nochill.sh.8 <- subset(nochill, site==2 & photo==1)
    nochill.sh.12 <- subset(nochill, site==2 & photo==2)
   # some graph parameters
   f <- 1
   x <- c(1.2, 1.8)
   x1 <- c(1.2, 1.8)
   xtxt <- c(1,2)
   yrange.use <- yrange
   # Open a blank plot
   # quartz("Quartz", width=4.5, height=3, pointsize=12)
   #par(mfrow=c(1,2), cex=0.7, xpd=TRUE, xaxt="n")
   plot(yrange.use, type="n",
        main=paste(species, "at Harvard Forest, USA"),
        xlab="", xaxt = "n",
        ylab=ylabelhere)
    mtext(text = c("15 C", "20 C"), side = 1, line = 1, at = x)
    leg.txt<- c("8 hours", "12 hours")
    legend("bottomright", leg.txt, pch=c(21,22), pt.bg=c("black","white"), bty="n")
    #  all species at Harvard Forest: 8 hours photoperiod
    y <- as.vector(nochill.hf.8$lday) 
    ysem <- as.vector(nochill.hf.8$se)
    points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)
    #  all species at Harvard Forest: 12 hours photoperiod
    y <- as.vector(nochill.hf.12$lday) 
    ysem<-as.vector(nochill.hf.12$se)
    points(x1,y,pch=22,bg='white', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05)

    #  saint hippo: 8 hours
    plot(yrange.use,type="n",
            main="Saint Hippolyte, Qc",
            xlab="",
            ylab= ylabelhere,
            xaxt = "n")
    mtext(text = c("15 C", "20 C"), side = 1, line = 1, at = x)
    leg.txt<- c("8 hours", "12 hours")
    legend("bottomright", leg.txt,pch=c(21,22), pt.bg=c("black","white"), bty="n")
    y <- as.vector(nochill.sh.8$lday)
    ysem <- as.vector(nochill.sh.8$se)
    if(length(y) == 2) {
    	points(x1,y,pch=21,bg='black', cex=1.2)
    arrows(x1,y-ysem,x1,y+ysem,code=3,angle=90,length=0.05) 
    			}
    #  saint hippo: 12 hours
    y <- as.vector(nochill.sh.12$lday) 
    ysem <- as.vector(nochill.sh.12$se)
    if(length(y) == 2) { 
    points(x,y,pch=22,bg='white', cex=1.2)
    arrows(x,y-ysem,x,y+ysem,code=3,angle=90,length=0.05) 
    			}
}
