dx <- dx[!is.na(dx$bday) & !is.na(dx$lday),]

# Site as fixed, chill as continuous
datalist.l <- list(lday = dx$lday, # leafout as respose 
                   warm = as.numeric(dx$warm), 
                   site = as.numeric(dx$site), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill = as.numeric(dx$chill),
                   N = nrow(dx), 
                   n_site = length(unique(dx$site)), 
                   n_sp = length(unique(dx$sp))
)


datalist.b <- list(lday = dx$bday, # leafout as respose 
                   warm = as.numeric(dx$warm), 
                   site = as.numeric(dx$site), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill = as.numeric(dx$chill),
                   N = nrow(dx), 
                   n_site = length(unique(dx$site)), 
                   n_sp = length(unique(dx$sp))
)


doym.1b <- stan('stan/lday_nosite_plusspint.stan', data = datalist.b, iter = 4000, chains = 4,
               control= list(adapt_delta = 0.90)) 

savestan("Chill Cont highdelt")

# no site effect at all, chill as categorical
datalist.l <- list(lday = dx$lday, # leafout as respose 
                   warm = as.numeric(dx$warm), 
                   site = as.numeric(dx$site), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill1 = as.numeric(dx$chill1),
                   chill2 = as.numeric(dx$chill2),
                   chill3 = as.numeric(dx$chill3),
                   N = nrow(dx), 
                   n_site = length(unique(dx$site)), 
                   n_sp = length(unique(dx$sp))
)

doym.2 <- stan('stan/lday_nosite_chill.stan', data = datalist.l, iter = 4000, chains = 4,
               control= list(adapt_delta = 0.90))  

savestan("No Site Chill Cat highdelt")

# Site effect, chill as categorical #-- this was run on 2016-03-25

datalist.l <- list(lday = dx$lday, # leafout as respose
                   warm = as.numeric(dx$warm),
                   site = as.numeric(dx$site),
                   sp = as.numeric(dx$sp),
                   photo = as.numeric(dx$photo),
                   chill1 = as.numeric(dx$chill1),
                   chill2 = as.numeric(dx$chill2),
                   chill3 = as.numeric(dx$chill3),
                   N = nrow(dx),
                   n_site = length(unique(dx$site)),
                   n_sp = length(unique(dx$sp))
)

doym.3 <- stan('stan/lday_nosite_plusspint_chill.stan', data = datalist.l, iter = 4000, chains = 4,
               control= list(adapt_delta = 0.90))  


savestan("Site Fixed Chill Cat highdelt ")


##################

rm(list=ls())


load("Stan Output Chill Cont highdelt2016-03-28.RData")
mchillcont <- doym.1 


launch_shinystan(mchillcont)

pairs(mchillcont, pars = names(mchillcont)[grep("mu_", names(mchillcont))])



load("Stan Output No Site Chill Cat2016-03-28.RData")
mchillcat <- doym.2 

pairs(mchillcat, pars = c(names(mchillcat)[grep("mu_", names(mchillcat))], "lp__"))

launch_shinystan(mchillcat)


load("Stan Output Site Fixed Chill Cat highdelt 2016-03-28.RData")
mchillcatsite <- doym.3

pairs(mchillcatsite, pars = c(names(mchillcatsite)[grep("mu_", names(mchillcatsite))], "lp__"))

launch_shinystan(mchillcatsite)

## notes from Stanleyi meeting 2016-03-21




setwd("~/Documents/git/buds/analyses")
source('stan/savestan.R')
# get latest .Rdata file

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# To run from saved stan output
if(!runstan) { 
  load(sort(dir()[grep("Stan Output", dir())], T)[1])
  ls() 
  launch_shinystan(ssm.l)
}

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> 

print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload))


dx$warmn = scale(as.numeric(as.character(dx$warm)))
dx$photon = scale(as.numeric(as.character(dx$photo)))
dx$chilln = scale(as.numeric(substr(as.character(dx$chill), 6, 6)))
dx$spn <- as.numeric(dx$sp)

levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)


### looking at a real species
with(dx[dx$sp == "VIBLAN",],
     table(warm, photo, chill)
)

vx <- dx[dx$sp == "VIBLAN",]

tapply(vx$lday, list(vx$warm, vx$photo, vx$chill), mean,na.rm=T)


tapply(vx$lday, list(vx$warm, vx$photo, vx$chill), function(x) length(x[!is.na(x)]))

mean(vx$lday,na.rm=T)

