data {
  int<lower=0> N;
  int<lower=0> n_site;
  int<lower=0> n_sp;
  int<lower=0> n_sp_site;
    int<lower=1, upper=n_site> site[N];
  int<lower=1, upper=n_sp> sp[N];
  int<lower=1, upper=n_sp_site> sp_site[N]; // created in R
  vector[N] lday;
  vector[N] warm;
  vector[N] photo;
  vector[N] chill;
}
transformed data {
  vector[N] inter;           // interaction of warming and photo as fixed
  inter    <- warm .* photo;  
}
parameters {
  vector[n_site] a;
  vector[n_sp] b_warm;
  vector[n_sp] b_photo;
  vector[n_sp] b_chill;
  vector[n_sp] b_inter;
  matrix[n_sp,n_site] c; // From Ch.13 earnings_latin_square.stan
  matrix[n_sp,n_site] d;
  vector[n_sp_site] mu_a; // not real any more!
  vector<lower=0>[n_sp_site] sigma_a;
  vector<lower=0>[n_sp] sigma_y; // not n_sp_site
  real mu_c;
  real mu_d;
   real<lower=0,upper=100> sigma_c;
  real<lower=0,upper=100> sigma_d;
  }

transformed parameters {
	vector[n_site] mu_a_hat;
	vector<lower=0>[n_site] sigma_a_hat;
	vector<lower=0>[N] sigma_y_hat;
	vector[N] y_hat;
	// From Ch.13 earnings_latin_square.stan, add matrix for sp by site for chill effects
	
	for(i in 1:N){
		y_hat[i] <- a[site[i]] + b_warm[sp[i]] * warm[i] + b_photo[sp[i]] * photo[i] + b_inter[sp[i]] * inter[i] + c[sp[i],site[i]] 
                + d[sp[i],site[i]] * chill[i];;
		sigma_y_hat[i] <- sigma_y[sp[i]];
	}

	for(i in 1:n_site){
		sigma_a_hat[i] <- sigma_a[sp_site[i]];
		mu_a_hat[i] <- mu_a[sp_site[i]];
	}

}

model {
	mu_a ~ normal(0, 1);
	a ~ normal(mu_a_hat, sigma_a_hat);
	b_warm ~ normal(0, 100);
	b_photo ~ normal(0, 100);
	b_chill ~ normal(0, 100);
	b_inter ~ normal(0, 100);
	mu_c ~ normal(0, 1);
  	for (i in 1:n_sp) c[i] ~ normal(10 * mu_c, sigma_c);

	mu_d ~ normal(0, 1);
	for (i in 1:n_sp) d[i] ~ normal(0.1 * mu_d, sigma_d);	
	lday ~ normal(y_hat, sigma_y_hat);
}