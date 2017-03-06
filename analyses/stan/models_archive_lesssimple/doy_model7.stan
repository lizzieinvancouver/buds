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
  vector[n_sp] mu_a; 
  vector<lower=0>[n_sp] sigma_a;
  real<lower=0> sigma_y; 
  
  real mu_b_warm;
  real mu_b_chill;
  real mu_b_photo;
  real mu_b_inter;
  real<lower=0> sigma_b_warm;
  real<lower=0> sigma_b_photo;
  real<lower=0> sigma_b_chill;
  real<lower=0> sigma_b_inter;
  }

transformed parameters {
	vector[N] y_hat;
	
	vector[n_site] mu_a_hat;
	vector<lower=0>[n_site] sigma_a_hat;
	
	for(i in 1:N){
		y_hat[i] <- a[site[i]] + b_warm[sp[i]] * warm[i] + b_photo[sp[i]] * photo[i] + b_inter[sp[i]] * inter[i] + b_chill[sp[i]] * chill[i];
	}

	for(i in 1:n_site){
		sigma_a_hat[i] <- sigma_a[sp_site[i]];
		mu_a_hat[i] <- mu_a[sp_site[i]];
	}
	
	
}

model {
	// Priors

	mu_a ~ normal(0, 1);
	mu_b_warm ~ normal(0, 1);
	mu_b_photo ~ normal(0, 1);
	mu_b_chill ~ normal(0, 1);
	mu_b_inter ~ normal(0, 1);
			
	a ~ normal(mu_a_hat, sigma_a_hat);
	
	b_warm ~ normal(mu_b_warm, sigma_b_warm);
	b_photo ~ normal(mu_b_photo, sigma_b_photo);
	b_chill ~ normal(mu_b_chill, sigma_b_chill);
	b_inter ~ normal(mu_b_inter, sigma_b_inter);
	
	lday ~ normal(y_hat, sigma_y);
}