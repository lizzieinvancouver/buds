README
budexperiments/analyses/stan
Started 6 March 2017
By Lizzie

This folder contains a bunch of different Stan models written by Dan F. B. Flynn for his growth chamber experiment conducted in winter-spring 2015.

Lizzie put many of these models in models_archive folders to make things easier.

Since it’s by Lizzie and not Dan, these are educated guesses!

<><><><><><><><><><><><><>
Supp materials
<><><><><><><><><><><><><>
NOTE: As of 2 July 2017 analyses/supp contains copies of these three files:

Main Stan model used:
lday_site_sp_chill_inter_poola_ncp.stan - but with comments updated or deleted if not important

Model output:
lday_site_sp_chill_inter_poola_ncp_doyl.Rda - on real leafout data with 3 divergent transitions (model run from 2 May 2017)

lday_site_sp_chill_inter_poola_ncp_doymb.Rda - on real budburst data with no divergent transitions (model run on 2 May 2017)


<><><><><><><><><><><><><>
In main folder:
<><><><><><><><><><><><><>

lday_site_sp_chill_inter_poola_ncp.stan —  built off lday_site_sp_chill_inter_poola.stan -  y ~ (warm * photo * chill * site)^2 …. It is a linear model with all interactions, with chilling levels as dummy variables, with hierarchical effect of species on slopes and intercepts. This model has non-centered parameterization the interaction terms, which showed funneling issues in lday_site_sp_chill_inter_poola.stan. 

This model (lday_site_sp_chill_inter_poola_ncp.stan) did run well on the fake data and on the real data. It is the model I am currently using! Whoop! Whoop!

lday_site_sp_chill_inter_poola_ncp_adjpriors.stan - same as lday_site_sp_chill_inter_poola_ncp.stan,but with adjusted code so that the priors were tripled: 10->30 and 35->105. Answer is the same. See lday_site_sp_chill_inter_poola_ncp_adjpriors_INFO.txt


* Remember: Increasing warmup iterations can also help with divergent transitions. *

<><><><>
Output:
<><><><>
doym.fpoola.ncpBAD.RData - a non-converging model of lday_site_sp_chill_inter_poola_ncpfull.stan on the fake data (I think). 

lday_site_sp_chill_inter_poola_doymb.Rda is the real bud burst data run on 
lday_site_sp_chill_inter_poola.stan (with diverging transitions)

lday_site_sp_chill_inter_poola_ncp_doyl.Rda - on real leafout data with 3 divergent transitions (model run from 2 May 2017)

lday_site_sp_chill_inter_poola_ncp_doymb.Rda - on real budburst data with no divergent transitions (model run on 2 May 2017)

lday_site_sp_chill_inter_poola_ncpfull_doyl.Rda - on real leafout data — ran fine. 

lday_site_sp_chill_inter_poola_ncpfull_doymb.Rda - on real bud burst data — ran okay (no divergent transitions) but had a few n_eff size issues related to estimating a (intercept).

lday_site_sp_chill_inter.Rda is the fake data run on lday_site_sp_chill_inter.stan in August 2016 (by Dan)

ncpmore_vs_ncpfull: txt output of models run on 29-30 April 2017, compares output between lday_site_sp_chill_inter_poola_ncpfull.stan and lday_site_sp_chill_inter_poola_ncpmore.stan on both bud burst and leafout data. 


<><><><><><><><><><><><><>
In models_archive_simple:
<><><><><><><><><><><><><>

doy_model0.stan	- very simple linear model with no hierarchical effects: y ~ warm + photo

doy_model1.stan	- very simple linear model with no hierarchical effects: y ~ warm * photo

doy_model2.stan	- linear model with one hierarchical effect — site on the intercept: y ~ warm * photo

doy_model3.stan	- linear model with two hierarchical effects — site on the intercept and species on each slope: y ~ warm * photo


<><><><><><><><><><><><><>
In models_archive_lesssimple:
<><><><><><><><><><><><><>

lday_site_sp_chill_inter_poola_ncpmore.stan - same as lday_site_sp_chill_inter_poola_ncp.stan (see below) but with CP on the intercept (a). You could also say this model is lday_site_sp_chill_inter_poola_ncpfull.stan with NCP turned off on the intercepts. 

*This model runs well on the real data (lday_site_sp_chill_inter_poola_ncpmore.stan) but I never got it to run on the fake data. See output/ncpmore_vs_ncpfull for some comparisons of output and more info.* 


doy_model4.stan	- linear model with some sort of crossed hierarchical effects — site and species (mentions latin square design, shudder!): I think…. y ~ warm * photo + chill (alert! not coded as dummy variable)

doy_model5.stan	- similar to doy_model4.stan

doy_model6.stan — linear model with two hierarchical effects — site on the intercept and species on each slope, again some off looping (but not with priors now): y ~ warm * photo + chill (alert! not coded as dummy variable)

doy_model7.stan — linear model with with two hierarchical effects — site on the intercept and species on each slope, some weird looping in each prior, which I don’t quite understand: y ~ warm * photo + chill (alert! not coded as dummy variable)

doy_model8.stan	- very similar to doy_model7.stan, did not work to figure out differences

doy_model41.stan - very similar to doy_model7.stan, did not work to figure out differences

lday0.stan — similar to doy_model7.stan but Stan code looks more correct, though chilling is still wrong: y ~ warm + photo + chill (alert! not coded as dummy variable)

multilevel_logistic_DF.stan — logistic model using some other (Gelman and Hill I suspect) data


<><><><><><><><><><><><><>
In models_archive_nested:
<><><><><><><><><><><><><>

lday_ind_trait.stan — linear model on traits, with two hierarchical effects — ind nested in species (follow classroom example that we used a lot): y ~ latitude

lday_ind2.stan — linear model on lday (leafout day), with two hierarchical effects — ind nested in species (follow classroom example that we used a lot): y ~ warm + photo


<><><><><><><><><><><><><>
In models_archive_altmodels:
<><><><><><><><><><><><><>

lday_site_sp_chill_inter.stan -  y ~ (warm * photo * chill * site)^2 …. this was the *main* model that Dan Flynn was using in 2016 when I (Lizzie) picked up the project. It is a linear model with all interactions, with chilling levels as dummy variables, with hierarchical effect of species on slopes and intercepts (but NO pooling on the intercepts). This models runs well on the fake data but returns divergent transitions (a lot of them) on the real data. (Note from Dan or me, not sure: tried to include species nested within site, which does not run!) 

lday_site_sp_chill_inter_poola.stan - as in lday_site_sp_chill_inter.stan but with pooling on the intercepts. Lizzie made this model in 2017.  This models runs well on the fake data but returns divergent transitions (a lot of them) on the real data. 

lday_nosite_chill.stan - linear model with no sp_site or interactions, BUT with chilling levels as dummy variables for each level, hierarchical effect of species on slopes: y ~ warm + photo + chill

lday_nosite_plusspint.stan — same as lday_nosite_chill.stan but with an INTERCEPT added

lday_nosite.stan - linear model with no sp_site or interactions, BUT with chilling levels as dummy variables for each level, hierarchical effect of species on slopes: y ~ warm + photo + chill (alert! not coded as dummy variable)

lday_site_chill.stan - similar to lday_nosite_chill.stan, but now site is included on intercept

lday_site_sp_chill.stan — similar to lday_site_sp_chill_inter.stan but with NO interactions and tried to include species nested within site, which does not run! y ~ warm * photo * chill

lday_sp_x_site_chill.stan — no interactions, hierarchical levels of species on intercept (only) and site on intercept only. No hierarchical levels on slopes. y ~ warm + photo + chill 


<><><><><><><><><><><><><>
In models_archive_ncp: (non-centered parameterization)
<><><><><><><><><><><><><>

lday_site_sp_chill_inter_poola_ncpfull.stan — as in lday_site_sp_chill_inter_poola.stan but WITH all possible non-centered parameterizations (i.e., in addition to NCP on the interactions, I tried adding it to the main effects too). This model actually rain painfully slow on the fake data thought and NEVER converged. So I did not use it. (On Lizzie’s computer I save the output in output/doym.fpoola.ncpBAD.Rdata)

lday_site_sp_chill_inter3X_poola_ncp.stan - as in lday_site_sp_chill_inter_poola_ncp.stan but attempted to add a 3X interaction (warm*photo*chill1). This model ran with this code:

    doym.b <- stan('stan/lday_site_sp_chill_inter3X_poola_ncp.stan', 
                 data = datalist.b, warmup=4000, iter = 7997, chains = 4,
                 control = list(adapt_delta = 0.9))

…led to 15541 divergent transitions (I think every transition was divergent!) and a crazy value for mu_b_inter_wpc1 (somewhere between about -1^7 and 3^6 … it never converged). Runs quickly though!

