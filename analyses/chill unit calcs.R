# Calculating chill units

setwd("~/Documents/git/buds/analyses")
print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])
load(file.path("input", toload))


# Dynamic Model

library(chillR)
# Luedeling recommends Dynamic Modle (chill portions)

# HF chilling from Oct 1 2014

htemp <- read.csv("~/Documents/git/buds/analyses/data/hf001-10-15min-m.csv")



htemp$datetime <- as.POSIXlt(htemp$datetime, format = "%Y-%m-%dT%H:%M")

# select years. There are 44 na's

htemp1 <- htemp[htemp$datetime > "2014-09-30" & htemp$datetime < "2015-01-20" & !is.na(htemp$datetime),]

htemp2 <- htemp[htemp$datetime > "2015-11-01" & !is.na(htemp$datetime),]

# aggregate to hourly averages 
ht <- aggregate(airt ~ format(htemp1$datetime, "%Y-%m-%d %H"), mean, data = htemp1)
names(ht)[1] = 'datetime'

ht$Year <- as.numeric(substr(ht$datetime, 1, 4))
ht$JDay <- as.numeric(format(strptime(substr(ht$datetime, 1, 10), "%Y-%m-%d"), "%j"))
ht$Hour <- as.numeric(substr(ht$datetime, 12, 13))
names(ht)[2] = "Temp"

# For current year chilling 
ht2 <- aggregate(airt ~ format(htemp2$datetime, "%Y-%m-%d %H"), mean, data = htemp2)
names(ht2)[1] = 'datetime'

ht2$Year <- as.numeric(substr(ht2$datetime, 1, 4))
ht2$JDay <- as.numeric(format(strptime(substr(ht2$datetime, 1, 10), "%Y-%m-%d"), "%j"))
ht2$Hour <- as.numeric(substr(ht2$datetime, 12, 13))
names(ht2)[2] = "Temp"

chilling(ht2, 305, 60) # Already 18 chill portions as of Dec 8.

chill0calc <- chilling(ht, 305, 60) # 39 chill portions by Jan 20 last year.

# Chilling in experimental setting. From Jan 22 - Feb 12 at 4, then to either 4 or 1.5 for additional 30 d. total: 22 d at 4, then 30 d extra.
diff(as.POSIXlt(c("2015-01-22", "2015-02-12", "2015-03-14"), "%Y-%m-%d"))

dayseq = seq(as.numeric(format(as.POSIXlt("2015-01-22", "%Y-%m-%d"), "%j")),
             as.numeric(format(as.POSIXlt("2015-03-14", "%Y-%m-%d"), "%j")))

chill1 <- data.frame(
  Year = as.numeric(rep("2015", 52*24)),
  JDay = as.numeric(rep(dayseq, each = 24)),
  Hour = rep(0:23, 52),
  Temp = 4
  )

# bug: Jday vs JDay, returns error for "chillout not found"

chill1calc <- chilling(chill1, 305, 60)

chill2 <- chill1
chill2$Temp = c(rep(4, 22), rep(1.5, 30))

chill2calc <- chilling(chill2, 305, 60) # FEWER chill portions and fewer Utah Model chill hours

allcalc <- rbind(chill0calc[3:6], chill0calc[3:6]+chill1calc[3:6], chill0calc[3:6]+chill2calc[3:6])

allcalc <- data.frame(Treatment = c("Field chilling","4.0° x 30 d", "1.5° x 30 d"), allcalc)
rownames(allcalc)=NULL
xtable(allcalc)


# Three models: Chilling Hours, Utah, Dynamic. Chilling hours: all temperatures equally effective (0-7.2 deg C). 
# Utah: Richardson EA, Seeley SD, Walker DR (1974) A model for estimating the completion of rest for Redhaven and Elberta peach trees. Hortscience 9(4):331–332
# No chill units below 1.4 deg C, half a unit from 1.4 to 2.4, full unit from 2.4 to 9.1, HALF from 9.1 to 12.4, FULL from 12.4 to 15.9??!
# Dynamic: Erez 1990, Fishman 1987.


# Sequential, parallel, and unified models: need multiple years of data?
# Ecodormancy, from external factors, Endodormancy from internal factors (broken by chilling)

#
# Interpolating hourly temperature data

# install.packages("Interpol.T")
library(Interpol.T)

