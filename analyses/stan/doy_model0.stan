data {
  int<lower=0> N;
  vector[N] lday;
  vector[N] warm;
  vector[N] photo;
}
parameters {
  vector[3] beta;
  real<lower=0> sigma;
}
model {
  lday ~ normal(beta[1] + beta[2] * warm + beta[3] * photo, sigma);
}