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
  vector[n_sp_site] mu_a; // not real any more!
  vector<lower=0>[n_sp_site] sigma_a;
  vector<lower=0>[n_sp] sigma_y; // not n_sp_site
  real mu_c;
  real mu_d;
  real mu_b_warm;
  real mu_b_chill;
  real mu_b_photo;
  real mu_b_inter;
  real sigma_b_warm;
  real sigma_b_photo;
  real sigma_b_chill;
  real sigma_b_inter;
  }

transformed parameters {
	vector[N] y_hat;
	vector<lower=0>[N] sigma_y_hat;
		
	// Site level
	vector[n_sp] mu_a_hat; 
	vector<lower=0>[n_site] sigma_a_hat;

	//  Species level
	vector<lower=0>[n_sp] mu_b_warm_hat;	
	vector<lower=0>[n_sp] mu_b_photo_hat;
	vector<lower=0>[n_sp] mu_b_chill_hat;	
	vector<lower=0>[n_sp] mu_b_inter_hat;	
				
	vector<lower=0>[n_sp] sigma_b_warm_hat;	
	vector<lower=0>[n_sp] sigma_b_photo_hat;
	vector<lower=0>[n_sp] sigma_b_chill_hat;	
	vector<lower=0>[n_sp] sigma_b_inter_hat;	
	
	for(i in 1:N){
		y_hat[i] <- a[site[i]] + b_warm[sp[i]] * warm[i] + b_photo[sp[i]] * photo[i] + b_inter[sp[i]] * inter[i] + b_chill * chill[i];
	}

	// Need to specify that a has two levels, b have n_sp levels, c and d have n_sp_site levels
	
	for(i in 1:n_site){
		sigma_a_hat[i] <- sigma_a[sp_site[i]];
		mu_a_hat[i] <- mu_a[sp_site[i]];
	}


	for(i in 1:n_sp){
		sigma_b_warm_hat[i] <- sigma_b_warm[sp[i]];
		mu_b_warm_hat[i] <- mu_b_warm[sp[i]];
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
			
		
	a ~ normal(mu_a, sigma_a);	
	
	b_warm ~ normal(mu_b_warm, sigma_b_warm);
	b_photo ~ normal(mu_b_photo, sigma_b_photo);
	b_chill ~ normal(mu_b_chill, sigma_b_chill);
	b_inter ~ normal(mu_b_inter, sigma_b_inter);
	
	mu_c ~ normal(0, 1);
  	for (i in 1:n_sp) c[i] ~ normal(mu_c, sigma_c);

	mu_d ~ normal(0, 1);
	for (i in 1:n_sp) d[i] ~ normal(mu_d, sigma_d);	

	lday ~ normal(y_hat, sigma_y);
}