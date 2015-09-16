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
  
}
transformed data {
  vector[N] inter;           // interaction
  inter    <- warm .* photo;  
}
parameters {
  vector[n_site] a;
  vector[n_sp] b_warm;
  vector[n_sp] b_photo;
  vector[n_sp] b_inter;
  vector[n_sp_site] mu_a; // not real any more!
  vector<lower=0>[n_sp_site] sigma_a;
  vector<lower=0>[n_sp] sigma_y; // not n_sp_site
  }

transformed parameters {
	vector[n_site] mu_a_hat;
	vector<lower=0>[n_site] sigma_a_hat;
	vector<lower=0>[N] sigma_y_hat;
	vector[N] y_hat;
	// Unfinished...
	for(i in 1:N){
		y_hat[i] <- a[site[i]] + b_warm[sp[i]] * warm[i] + b_photo[sp[i]] * photo[i] + b_inter[sp[i]] * inter[i];
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
	b_inter ~ normal(0, 100);
	lday ~ normal(y_hat, sigma_y_hat);
}