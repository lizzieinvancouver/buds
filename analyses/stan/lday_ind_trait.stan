

data {
  int<lower=0> N;
  int<lower=0> n_ind;
  int<lower=0> n_sp;
  int<lower=1, upper=n_site> site[N];
  int<lower=1, upper=n_sp> sp[N];
  int<lower=1, upper=n_ind> ind[N];
  vector[N] y; // response
  vector[N] lat; // predictor

	int<lower=1> splookup[n_ind]; // will this work if unbalanced?
		
	}

parameters {
  vector[n_sp] a_sp;
  vector[n_sp] b_lat;
 
  real mu_b_lat; 

  real<lower=0> sigma_b_lat; // 
  
  real<lower=0> sigma_y; // this only gets used in model block
  }


transformed parameters {
	vector[N] y_hat;
		
	// Species level. Random slopes
	for (k in 1:n_sp) {
	
		b_lat_sp <- b_lat_0 + mu_b_lat_sp[k]
				
		}
	
	// individual level	
	for (j in 1:n_ind){
	
		b_lat_sp_ind <- b_lat_sp[splookup] + mu_b_lat_sp_ind[j]
	
	}
	
	// cutting level
	for(i in 1:N){

		y_hat[i] <- a_sp[sp[i]] + b_lat_sp_ind[ind[i]] * lat[i];
		
		}
	
}

model {
	mu_b_lat_sp ~ normal(0, 35); // 100 = 3 months on either side. Narrow down to 35

	mu_b_lat_sp_ind ~ normal(0, 10); // 10 d on either side at individual level

	sigma_b_lat_sp ~ normal(0, 10); // Start big at 10, go smaller if introduces problems
		
	sigma_b_lat_ind ~ normal(0, 10); // Reduce sd of sigma at individual level? 
	
	b_lat_sp ~ normal(mu_b_lat_sp, sigma_b_lat_sp);

	lday ~ normal(y_hat, sigma_y);
}