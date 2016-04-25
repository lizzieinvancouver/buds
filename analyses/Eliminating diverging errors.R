# Eliminating diverging errors
# 1. scale data


datalist.scale <- list(lday = drop(scale(dx$lday)), 
                   warm = as.numeric(dx$warm), 
                   site = as.numeric(dx$site), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill = as.numeric(dx$chill), 
                   N = nrow(dx), 
                   n_site = length(unique(dx$site)), 
                   n_sp = length(unique(dx$sp))
)


  doym.scaled <- stan('stan/lday0.stan', data = datalist.scale, iter = 4000, chains = 4) 
  
  sumnosite <- summary(doym.scaled)$summary
  
  ssm.scaled <- as.shinystan(doym.scaled)
  launch_shinystan(ssm.scaled)
  
  pairs(doym.scaled)
  
# 2. throw out site


datalist.nosite <- list(lday = dx$lday, # leafout as respose 
                   warm = as.numeric(dx$warm), 
                   sp = as.numeric(dx$sp), 
                   photo = as.numeric(dx$photo), 
                   chill = as.numeric(dx$chill), 
                   N = nrow(dx), 
                   n_sp = length(unique(dx$sp))
)

doym.nosite <- stan('stan/lday_nosite.stan', data = datalist.nosite, iter = 4000, chains = 4) 

sumnosite <- summary(doym.nosite)$summary

ssm.nosite <- as.shinystan(doym.nosite)
launch_shinystan(ssm.nosite)


## with sp intercept


datalist.spint <- list(lday = dx$lday, # leafout as respose 
                        warm = as.numeric(dx$warm), 
                        sp = as.numeric(dx$sp), 
                        photo = as.numeric(dx$photo), 
                        chill = as.numeric(dx$chill), 
                        N = nrow(dx), 
                        n_sp = length(unique(dx$sp))
)

doym.spint <- stan('stan/lday_spint_plusspint.stan', data = datalist.spint, iter = 4000, chains = 4) 

sumspint <- summary(doym.nosite)$summary

ssm.spint <- as.shinystan(doym.nosite)
launch_shinystan(ssm.spint)
