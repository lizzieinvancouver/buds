// Stan model following adapted from lday_site_sp_chill_inter3X_poola_ncp
// But for ONE species and four treatments (b0=control)


data {
  int<lower=0> N; // size of total observations (4*15)
  vector[N] lday; // y data
  vector[N] chill1; // x data of 0/1 for drought treat 1 
  vector[N] chill2; // x data of 0/1 for drought treat 2 
  vector[N] chill3; // x data of 0/1 for drought treat 3
}


parameters {
  real b0; // intercept -- control 
  real b1; // effect of drought 1 relative to control
  real b2; // effect of drought 2 relative to control
  real b3; // effect of drought 3 relative to control 
  real<lower=0> sigma_y; 
  }


transformed parameters { 
  vector[N] y_hat; // Note to self: all these declarations must happen together!

	for(i in 1:N){
		y_hat[i] = b0 + 
    b1 * chill1[i] + 
		b2 * chill2[i] + 
    b3 * chill3[i];
				
		}
	
}

model {
	// Priors 
	b0 ~ normal(20, 10); // covers -10 to 50 ...hist(rnorm(1000,20,10))
	b1 ~ normal(0, 5);
	b2 ~ normal(0, 5);
	b3 ~ normal(0, 5);
	sigma_y ~ normal(0, 5);
	
	lday ~ normal(y_hat, sigma_y);

}

