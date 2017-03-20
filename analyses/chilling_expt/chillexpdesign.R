# Calculating chilling portions for new chilling experiment

# 5 chilling levels, including control
# 6 species
# 8 reps / species / level
# 3 time points after control
3 * 6* 8 * 5 + 6 * 8 # exp + control; total units to observe

chilltreat = c(1, 2, 6, 10)

timetreat = c(10, 20, 30)

spp = c("acesac", "acerub", "querub","hamvir","vibcas","franig")

chillev = length(chilltreat)
spno = length(spp)
repno = 8
timeno = length(timetreat)
lengthout = chillev*spno*repno*timeno 

time <- gl(timeno, chillev*spno*repno, length = lengthout, labels = timetreat) # two time blocks
sp <- gl(spno, repno*chillev, length = lengthout, labels = spp) # within time block, have species repeat rep * chilllev times
chill <- gl(chillev, repno, length = lengthout, labels = chilltreat) # for each species in each time block, repeat rep number of times
reps <- gl(repno, 1, length = lengthout) # for each treatment combination, give number of reps


design <- data.frame(time, sp, chill, rep=reps)

design[1:50,]

# Now calculating chilling portions

library(chillR)

# chill unit calcs, for each treatment

chillcalc <- function(days, temp){
	Year = 2016
	JDay = as.numeric(gl(days, 24))
	Hour = as.numeric(gl(24, 1, length = days*24))
	Temp = temp
	xx <- data.frame(Year, JDay, Hour, Temp)
	chilling(xx, 1, days)
	}


chilltreat = seq(0, 10, by = 0.5)
timetreat = seq(5, 40, by = 5)
chillportions <- vector()
numdays <- vector()

for(i in chilltreat){
	for(j in timetreat){
	
	chillportions <- c(chillportions, as.numeric(chillcalc(j, i)[6]))
	numdays <- c(numdays, j)
	}
}
	
dat <- data.frame(days = numdays, 
					chill = gl(length(chilltreat), length(timetreat), labels = chilltreat), 
					cp = chillportions)

library(ggplot2)

#qplot(days, cp, color = chill, data = dat)

qplot(chill, cp, color = days, data = dat, 
	ylab = "Chill Portions \n (Dynamic Model)", 
	xlab = "Chilling Treatment (°C)")
	
# Finding equivalent treatments:
# 1 degree, 30 days = 2 degree, ~17 days! According to this model.

chillcalc(30, 1)
chillcalc(17, 2)

# Maximum spread: 1, 2, 6, and 10 degrees chilling. Should be essentially no differences between 4 and 8. Should be very large differnces between 1 and 2.

# Times: 




chilltreat = c(1, 2, 5, 10)
timetreat = c(10, 20, 30)
chillportions <- vector()
numdays <- vector()
for(i in chilltreat){
	for(j in timetreat){
	
	chillportions <- c(chillportions, as.numeric(chillcalc(j, i)[6]))
	numdays <- c(numdays, j)
	}
}
	
dat <- data.frame(days = numdays, 
					chill = gl(length(chilltreat), length(timetreat), labels = chilltreat), 
					cp = chillportions)

library(ggplot2)

#qplot(days, cp, color = chill, data = dat)

qplot(chill, cp, color = days, data = dat, 
	ylab = "Chill Portions \n (Dynamic Model)", 
	xlab = "Chilling Treatment (°C)")
	



