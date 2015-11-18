data {
  int<lower=0> N;
  int<lower=0> n_site;
  int<lower=1, upper=n_site> site[N];
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
  vector[3] beta;
  real mu_a;
  real<lower=0> sigma_a;
  real<lower=0> sigma_y;
}

transformed parameters {

	vector[N] y_hat;
	
	for(i in 1:N)
		y_hat[i] <- a[site[i]] + beta[1] * warm[i] + beta[2] * photo[i] + beta[3] * inter[i];
}

model {
  mu_a ~ normal(0, 1);
  a ~ normal(mu_a, sigma_a);
  
  beta ~ normal(0, 100);
  
	lday ~ normal(y_hat, sigma_y);
}