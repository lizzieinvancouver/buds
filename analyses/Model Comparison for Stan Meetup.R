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
