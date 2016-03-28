data {
  int<lower=0> N;
  int<lower=0> n_sp;
  int<lower=1, upper=n_sp> sp[N];
  vector[N] lday;
  vector[N] warm;
  vector[N] photo;
  vector[N] chill1; 
  vector[N] chill2;
  vector[N] chill3;
}

parameters {
  vector[n_sp] b_warm;
  vector[n_sp] b_photo;
  vector[n_sp] b_chill1;
  vector[n_sp] b_chill2;
  vector[n_sp] b_chill3;
 
  real mu_b_warm; 
  real mu_b_chill1;
  real mu_b_chill2;
  real mu_b_chill3;
  real mu_b_photo;

  real<lower=0> sigma_b_warm;
  real<lower=0> sigma_b_photo;
  real<lower=0> sigma_b_chill1;
  real<lower=0> sigma_b_chill2;
  real<lower=0> sigma_b_chill3;
  
  real<lower=0> sigma_y; 
  }


transformed parameters {
	vector[N] y_hat;
		
	for(i in 1:N){
		
		y_hat[i] <- b_warm[sp[i]] * warm[i] + b_photo[sp[i]] * photo[i] + b_chill1[sp[i]] * chill1[i] + b_chill2[sp[i]] * chill2[i] + b_chill3[sp[i]] * chill3[i];
		
		}
	
}

model {
	// Priors. Make them flat
	mu_b_warm ~ normal(0, 100);
	mu_b_photo ~ normal(0, 100);
	mu_b_chill1 ~ normal(0, 100);
	mu_b_chill2 ~ normal(0, 100);
	mu_b_chill3 ~ normal(0, 100);
	
	b_warm ~ normal(mu_b_warm, sigma_b_warm);
	b_photo ~ normal(mu_b_photo, sigma_b_photo);
	b_chill1 ~ normal(mu_b_chill1, sigma_b_chill1);
	b_chill2 ~ normal(mu_b_chill2, sigma_b_chill2);
    b_chill3 ~ normal(mu_b_chill3, sigma_b_chill3);

	lday ~ normal(y_hat, sigma_y);
}