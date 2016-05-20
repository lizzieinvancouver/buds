

data {
  int<lower=0> N;
  int<lower=0> n_ind;
  int<lower=0> n_sp;
  int<lower=1, upper=n_site> site[N];
  int<lower=1, upper=n_sp> sp[N];
  int<lower=1, upper=n_ind> ind[N];
  vector[N] lday; // response
  vector[N] warm; // predictor
  vector[N] photo; // predictor

	int<lower=1> splookup[n_ind]; // will this work if unbalanced?
		
	}

parameters {
  vector[n_sp] a_sp;
  vector[n_sp] b_warm;
  vector[n_sp] b_photo;
 
 
  real mu_b_warm; 
  real mu_b_photo;

  real<lower=0> sigma_b_warm; // 
  real<lower=0> sigma_b_photo;
  
  real<lower=0> sigma_y; // this only gets used in model block
  }


transformed parameters {
	vector[N] y_hat;
		
	// Species level. Random slopes
	for (k in 1:n_sp) {
	
		b_warm_sp <- b_warm_0 + mu_b_warm_sp[k]
		b_photo_sp <- b_photo_0 + mu_b_photo_sp[k]
				
		}
	
	// individual level	
	for (j in 1:n_ind){
	
		b_warm_sp_ind <- b_warm_sp[splookup] + mu_b_warm_sp_ind[j]
		b_photo_sp_ind <- b_photo_sp[splookup] + mu_b_photo_sp_ind[j]	
	
	}
	
	// cutting level
	for(i in 1:N){

		y_hat[i] <- a_sp[sp[i]] + b_warm_sp_ind[ind[i]] * warm[i] + b_photo_sp_ind[ind[i]] * photo[i];
		
		}
	
}

model {
	mu_b_warm_sp ~ normal(0, 35); // 100 = 3 months on either side. Narrow down to 35
	mu_b_photo_sp ~ normal(0, 35);

	mu_b_warm_sp_ind ~ normal(0, 10); // 10 d on either side at individual level
	mu_b_photo_sp_ind ~ normal(0, 10);


	sigma_b_warm_sp ~ normal(0, 10); // Start big at 10, go smaller if introduces problems
	sigma_b_photo_sp ~ normal(0, 10); 
	
	sigma_b_warm_ind ~ normal(0, 10); // Reduce sd of sigma at individual level? 
	sigma_b_photo_ind ~ normal(0, 10); 

	
	b_warm_sp ~ normal(mu_b_warm_sp, sigma_b_warm_sp);
	b_photo_sp ~ normal(mu_b_photo, sigma_b_photo);

	lday ~ normal(y_hat, sigma_y);
}