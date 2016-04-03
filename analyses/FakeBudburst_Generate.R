# Fake data for buburst stan work
library(dplyr)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: same as experiment, with two sites, 28 species, two levels each of warming and photoperiod, and three levels of chilling. 2016-04-01 adding interactions, and changing to 
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsite = 2
nsp = 28

nwarm = 2
nphoto = 2
nchill = 3

rep = 10 # within each combination of treatments. 

(ntot = nsite*nwarm*nphoto*nchill*rep) # 792 rows; 22k rows across species

# Build up the data frame
site = gl(nsite, rep, length = ntot)

warm = gl(nwarm, rep*nsite, length = ntot)
photo = gl(nphoto, rep*nsite*nwarm, length = ntot)
chill = gl(nchill, rep*nsite*nwarm*nphoto, length = ntot)

chill1 = ifelse(chill == 2, 1, 0) 
chill2 = ifelse(chill == 3, 1, 0) 

treatcombo = paste(warm, photo, chill1, chill2, sep = "_")

(d <- data_frame(site, warm, photo, chill1, chill2, treatcombo))

###### Set up differences for each level
sitediff = 2 
warmdiff = -20 # days earlier from 1 to 2
photodiff = -14
chill1diff = -20
chill2diff = -19

# interactions. 9 two-way interactions
sitewarm = 0
sitephoto = 0
sitechill1 = -1 # similar to stan results
sitechill2 = -2
warmphoto = 3.5 # positive 3.5. So at the warm level, the effect of longer days is muted by 3.5 days.
warmchill1 = 11 # both positive ~ 10. 
warmchill2 = 9
photochill1 = 0.1 # from stan results
photochill2 = 1

mm <- model.matrix(~(site+warm+photo+chill1+chill2)^2, data.frame(site, warm, photo))
# remove last column, chill1 x chill2, empty
mm <- mm[,-grep("chill1:chill2", colnames(mm))]
colnames(mm)

coeff <- c(1, sitediff, warmdiff, photodiff, chill1diff, chill2diff, 
           sitewarm, sitephoto, sitechill1, sitechill2,
           warmphoto, warmchill1, warmchill2,
           photochill1, photochill2
)

bb <- rnorm(n = length(warm), mean = mm %*% coeff, sd = 1) # should be able to do sd = mm %*% sd.coeff as well, with a different sd for each parameter.

fake <- data.frame(bb, site, warm, photo, chill1, chill2)

summary(lm(bb ~ (site+warm+photo+chill1+chill2)^2, data = fake)) # sanity check 

##### Again, now with species now.

baseinter = 35 # baseline intercept across all species 
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

fake <- vector()

for(i in 1:nsp){ # loop over species, as these are the random effect modeled
  
  coeff <- c(spint[i], sitediff, warmdiff, photodiff, chill1diff, chill2diff, 
             sitewarm, sitephoto, sitechill1, sitechill2,
             warmphoto, warmchill1, warmchill2,
             photochill1, photochill2
  )
  
  bb <- rnorm(n = length(warm), mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(bb, sp = i, site, warm, photo, chill1, chill2)
      
  fake <- rbind(fake, fakex)  
  }

summary(lm(bb ~ (site+warm+photo+chill1+chill2)^2, data = fake)) # sanity check 

#summary(lmer(bb ~ (site|sp) + (warm|sp) + (photo|sp) + (chill1|sp) + (chill2|sp), data = fake)) # too hard for lmer.

save(list=c("fake"), file = "Fake Budburst.RData")



