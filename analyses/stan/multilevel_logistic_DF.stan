data {
  int<lower=0> N;
  int<lower=0> n_income;
  int<lower=0> n_state;
  int<lower=0, upper=n_income> income[N];
  int<lower=0, upper=n_state> state[N];
  int<lower=0, upper=1> y[N];
  vector[n_state] v_prev;
}

parameters {
  real<lower=0> sigma;
  real<lower=0> sigma_state;
  real<lower=0> sigma_income;

  real b_0;
  real b_income;

  vector[n_state] b_hat;
}
model {
  vector[N] p;
  vector[n_state] b_state_hat;

  b_0 ~ normal(0, 100);
  b_income ~ normal(0, 100);
  b_hat ~ normal(0, 100);

  for (i in 1:N)
    p[i] <- fmax(0, fmin(1, inv_logit(b_0 + b_income[income[i]] +
      b_hat[state[i]])));

  y ~ bernoulli(p);
}
