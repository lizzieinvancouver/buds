# Fake data for buburst stan work
library(dplyr)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# new version, with vectorized random value generation
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsite = 2
nsp = 28

nwarm = 2
nphoto = 2
nchill = 3

rep = 101

(ntot = nsp*nsite*nwarm*nphoto*nchill*rep) # 67k rows

# Build up the data frame
sp = gl(nsp, rep, length = ntot) 
site = gl(nsite, rep*nsp, length = ntot)

warm = gl(nwarm, rep*nsp*nsite, length = ntot)
photo = gl(nphoto, rep*nsp*nsite*nwarm, length = ntot)
chill = gl(nchill, rep*nsp*nsite*nwarm*nphoto, length = ntot)

treatcombo = paste(warm, photo, chill, sep = "_")

(d <- data_frame(sp, site, warm, photo, chill, treatcombo))

###### Set up differences for each level

warmdiff = c(0, 10) # days earlier from 1 to 2
photodiff = c(0, 7)
chilldiff = c(0, 1, 3)

sd.warmdiff = 1 
sd.photodiff = 0.5 
sd.chilldiff = 2 

sd.sp = seq(0.1, 2, length.out = nsp) # perhaps remove... giving species different sd

sitemeans = c(0, 2) # day of year of budburst
spmeans = 1:nsp # additional days for each species

####### Generate values. Go over site, then treatment combo. First arrange data frame in order of loop
d <- arrange(d, site, treatcombo)

bb <- vector()

for(i in 1:nsite){
  d1 <- filter(d, site == i)
  for(j in unique(d1$treatcombo)){
    d2 <- filter(d1, treatcombo == j)
    
    warmx = d2$warm[1]
    photx = d2$photo[1]
    chilx = d2$chill[1]
    sitx = d2$site[1]
    
    # vector of means, based on species values at this site and treatment combination
    meanx = spmeans + sitemeans[sitx] + warmdiff[warmx]+ photodiff[photx] + chilldiff[chilx]
    
    # Draw species_number x replicate number of draws, for each mean
    xx <- rnorm(nsp*rep, meanx, sqrt( sd.warmdiff^2 + sd.photodiff^2 + sd.chilldiff^2 + sd.sp^2) )
    xx.sp <- rep(1:nsp, rep) # put these values in the right order to match the data frame
    
    bb <- c(bb, xx[order(xx.sp)])
  }
}

fake <- data.frame(d, bb)

#ggplot(d, aes(warm, bb)) + geom_point(color = site) + facet_grid(.~sp)
tapply(fake$bb, list(fake$sp, fake$site), mean)

save(list=c("fake"), file = "Fake Budburst.RData")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
  
### do a small fake 

nsite = 2
nsp = 28

nwarm = 2
nphoto = 2
nchill = 3

rep = 5

(ntot = nsp*nsite*nwarm*nphoto*nchill*rep) # 3k rows

# Build up the data frame
sp = gl(nsp, rep, length = ntot) 
site = gl(nsite, rep*nsp, length = ntot)

warm = gl(nwarm, rep*nsp*nsite, length = ntot)
photo = gl(nphoto, rep*nsp*nsite*nwarm, length = ntot)
chill = gl(nchill, rep*nsp*nsite*nwarm*nphoto, length = ntot)

treatcombo = paste(warm, photo, chill, sep = "_")

(d <- data_frame(sp, site, warm, photo, chill, treatcombo))

###### Set up differences for each level

warmdiff = c(0, 10) # days earlier from 1 to 2
photodiff = c(0, 7)
chilldiff = c(0, 1, 3)

sd.warmdiff = 1 
sd.photodiff = 0.5 
sd.chilldiff = 2 

sd.sp = seq(0.1, 2, length.out = nsp) # perhaps remove... giving species different sd

sitemeans = c(0, 2) # day of year of budburst
spmeans = 1:nsp # additional days for each species

####### Generate values. Go over site, then treatment combo. First arrange data frame in order of loop
d <- arrange(d, site, treatcombo)

bb <- vector()

for(i in 1:nsite){
  d1 <- filter(d, site == i)
  for(j in unique(d1$treatcombo)){
    d2 <- filter(d1, treatcombo == j)
    
    warmx = d2$warm[1]
    photx = d2$photo[1]
    chilx = d2$chill[1]
    sitx = d2$site[1]
    
    # vector of means, based on species values at this site and treatment combination
    meanx = spmeans + sitemeans[sitx] + warmdiff[warmx]+ photodiff[photx] + chilldiff[chilx]
    
    # Draw species_number x replicate number of draws, for each mean
    xx <- rnorm(nsp*rep, meanx, sqrt( sd.warmdiff^2 + sd.photodiff^2 + sd.chilldiff^2 + sd.sp^2) )
    xx.sp <- rep(1:nsp, rep) # put these values in the right order to match the data frame
    
    bb <- c(bb, xx[order(xx.sp)])
  }
}

fake <- data.frame(d, bb)

save(list=c("fake"), file = "Fake Budburst Smaller.RData")



