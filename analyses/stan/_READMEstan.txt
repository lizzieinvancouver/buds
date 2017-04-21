README
budexperiments/analyses/stan
Started 6 March 2017
By Lizzie

This folder contains a bunch of different Stan models written by Dan F. B. Flynn for his growth chamber experiment conducted in winter-spring 2015.

Lizzie put many of these models in models_archive folders to make things easier.

Since it’s by Lizzie and not Dan, these are educated guesses!

<><><><><><><><><><><><><>
Still in main folder:
<><><><><><><><><><><><><>

lday_site_sp_chill_inter_poola_ncp.stan —  built off lday_site_sp_chill_inter_poola.stan -  y ~ (warm * photo * chill * site)^2 …. It is a linear model with all interactions, with chilling levels as dummy variables, with hierarchical effect of species on slopes and intercepts. This model has non-centered parameterization the interaction terms, which showed funneling issues in lday_site_sp_chill_inter_poola.stan. 

*This models runs well on the fake data and on the real data! Whoop, whoop!* 

<><><><>
Output:
<><><><>
lday_site_sp_chill_inter.Rda is the fake data run on lday_site_sp_chill_inter.stan
lday_site_sp_chill_inter_poola_doymb.Rda is the real bud burst data run on lday_site_sp_chill_inter_poola.stan (with diverging transitions)
  
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