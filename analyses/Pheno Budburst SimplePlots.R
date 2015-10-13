## Started 13 October 2015 ##
## Simple plots of leafout day ##

setwd("~/Documents/git/buds/analyses") # setwd("~/Documents/git/projects/treegarden/budburstexp2015/analyses")

load("input/Budburst Data 2015-10-05")

source("source/simpleplot.R")

dx1 <- dx
dx1[dx1==75] = NA
dxnoNA <- subset(dx1, is.na(lday)==FALSE)
dxnoNA <- as.data.frame(dxnoNA)

dxnoNA.nochill <- subset(dxnoNA, chill=="chill0")
dxnoNA.1chill <- subset(dxnoNA, chill=="chill1")
dxnoNA.2chill <- subset(dxnoNA, chill=="chill2")

makesimpleplot.lday(dxnoNA, c(20,100), "lday", "leafout day of year")
makesimpleplot.lday(dxnoNA.nochill, c(20,100), "lday", "leafout day of year")
makesimpleplot.lday(dxnoNA.1chill, c(20,100), "lday", "leafout day of year")
makesimpleplot.lday(dxnoNA.2chill, c(20,100), "lday", "leafout day of year")

sitespp <- as.data.frame(table(dxnoNA$sp, dxnoNA$site))
sitespp <- subset(sitespp, Freq>0)
sppatsites <- aggregate(sitespp["Var2"], sitespp["Var1"], FUN=length)
sppatbothsites <- subset(sppatsites, Var2>1)
spp <- sppatbothsites$Var1
spp <- spp[!spp=="KALANG"]


# f(x) makesimpleplot.sp.lday is designed to work with the quartz code I commented out...
# so the legend etc. do not come out in perfect place
pdf(file="graphs/simpleplots/lday_byspp.pdf", 10, 6, paper="a4r", onefile=TRUE)
for (i in c(1:length(spp))){
    spdf <- subset(dxnoNA, sp==spp[i])
    makesimpleplot.sp.lday(spdf, c(20,100), "lday", "leafout day of year", spp[i])
}
dev.off()
