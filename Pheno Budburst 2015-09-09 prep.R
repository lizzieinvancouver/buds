# Analyzing phenology first work March 2015. Script also for Tim to work with R


# Setup: load packages, set working directory, read in the data.

library(gdata)
library(nlme)
library(scales)
library(arm)

setwd("~/Documents/git/buds")

d <- read.csv("Budburst.csv")

# add relevant columns to d2 twigs

idx <- strsplit(as.character(d2$id), "_")
d2$site <- unlist(lapply(idx, function(x) x[2]))
d2$rep <- unlist(lapply(idx, function(x) x[3]))
d2$sp <- substr(unlist(lapply(idx, function(x) x[1])), 1, 6)
d2$gen <- substr(unlist(lapply(idx, function(x) x[1])), 1, 3)
d2$ind <- substr(d2$id, 1, 11)


d2$warm <- factor(substr(d2$treatcode, 1, 1), labels = c("cool", "warm"))
d2$photo <- factor(substr(d2$treatcode, 2, 2), labels = c("long", "short"))
d2$chill <- factor(substr(d2$treatcode, 3, 3), labels = c("chill0", "chill1", "chill2"))



coltokeep <- c("Date","id","sp","rep","site","ind","treatcode","warm","photo","chill","gen","Term.fl","Lat.fl","Term.lf","Lat.lf","Comments","Observer")

d <- rbind(d1[coltokeep], d2[coltokeep])


# Format date, making a continous day since beginning of experiment	
d$Date <- strptime(d$Date, "%Y-%m-%d")
day0 <- as.numeric(format(d$Date, "%j"))
d$day <- day0 - as.numeric(format(strptime("2015-02-06", "%Y-%m-%d"), "%j")) + 1 
d$day.chill <- day0 - as.numeric(format(strptime("2015-03-11", "%Y-%m-%d"), "%j")) + 1 

d$dayuse <- ifelse(d$chill == "chill0", d$day, d$day.chill)

# order by date

d <- d[order(d$Date, d$id, d$treatcode),]

# typical difference in date by treatment
for(i in levels(d$treatcode)){
	datediff <- diff(d[d$treatcode == i,"Date"])
	cat(rep("<>", 20), "\n", i, "\n")
	print(summary(unclass(datediff[datediff!=0]/60/60/24)))
	}


# Cleanup: fix data types (slashes and other characters in the phenostage columns), date format

names(d) 

# make continuous data, now using second value if something is split by slash
d$tleaf <- unlist(lapply(strsplit(as.character(d$Term.lf), "/"), function(x) ifelse(length(x)>1, x[2], x[1])))
d$lleaf <- unlist(lapply(strsplit(as.character(d$Lat.lf), "/"), function(x) ifelse(length(x)>1, x[2], x[1])))

# and use first values if comma separated
d$tleaf <- unlist(lapply(strsplit(as.character(d$tleaf), ", "), function(x) ifelse(length(x)>1, x[1], x[1])))
d$lleaf <- unlist(lapply(strsplit(as.character(d$lleaf), ", "), function(x) ifelse(length(x)>1, x[1], x[1])))

for(i in c("tleaf","lleaf","Term.fl","Lat.fl")){
	d[,i][d[,i] =="-"] = NA
	d[,i][d[,i] ==""] = NA
	d[,i][d[,i] =="*"] = NA
	}
	
d$lleaf <- sub("\\*", "", d$lleaf) # get rid of one asterix after 4*

# Make into numeric data, needed because it was read in with weird characters and automaticially made into factors
for(i in c("tleaf","lleaf","Term.fl","Lat.fl")){
	d[,i] = as.numeric(as.character(d[,i])) }

# calculating day since initiation of experiment, by twig id (includes treatment). Tricky part: what should NA be? They still haven't reached stage 6... for this calculation, I put them as NA. Will change with more data.

# For chill treatments, count days since they started in chambers, not since chill0 started

bday <- lday <- fday <- vector()

for(i in levels(d$id)){ # i=levels(d$id)[500] 
	
	dx <- d[d$id == i,]

	day.use <- ifelse(dx$chill[1] == "chill0", "day", "day.chill")

	bdax <- which(apply(dx[,c("tleaf","lleaf")], 1, max, na.rm=T) >= 3)
	if(length(bdax) < 1) bdax = 75 else bdax = dx[min(bdax),day.use]

	ldax <- which(apply(dx[,c("tleaf","lleaf")], 1, max, na.rm=T) >= 6)
	if(length(ldax) < 1) ldax = 75 else ldax = dx[min(ldax),day.use]
		
	fdax <- which(apply(dx[,c("Term.fl","Lat.fl")], 1, max) > 16)
	if(length(fdax) < 1) fdax = 75 else fdax = dx[min(fdax),day.use]

	bday <- c(bday, bdax)
	lday <- c(lday, ldax)
	fday <- c(fday, fdax)	
	}



# merging with unique id data

dx <- d[match(levels(d$id), d$id),] # with twig id in same order as the loop above

dx <- dx[,2:20]

dx <- data.frame(dx, lday, fday, bday)

# Clean up levels for the treatment factors. Default is alphabetical, here want to sort in a meaningful way.
# levels(dx$treatcode) <- c(2,1,4,3)
# dx$treatcode <- as.factor(dx$treatcode)
# levels(dx$treatcode) <- c("Cool-short","Cool-long","Warm-short","Warm-long")

dx$photo <- as.factor(as.character(dx$photo))
levels(dx$photo) <- c('12', '08')
dx$photo <- as.factor(as.character(dx$photo))

dx$warm <- as.factor(as.character(dx$warm))
levels(dx$warm) <- c('15', '20')
dx$warm <- as.factor(as.character(dx$warm))

write.csv(dx, "Budburst By Day.csv", row.names=F)
write.csv(d, "Budburst.csv", row.names=F)

save(list = c('d', 'dx'), file = paste("Budburst Data", Sys.Date())) # save as R formatted data frames for easier use next time.


