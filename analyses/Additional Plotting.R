# Pheno budburst plots and tables - 
# Additional plots 2016-04-03

library(scales) # for alpha
library(ggplot2)

setwd("~/Documents/git/buds/analyses")

# Stan Output 2016-03-31 Inter.RData
load(sort(dir()[grep("Stan Output", dir())], T)[1])

print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))

levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)

# Chill dummy variables
dx$chill1 = ifelse(dx$chill == 2, 1, 0) 
dx$chill2 = ifelse(dx$chill == 3, 1, 0) 

# 