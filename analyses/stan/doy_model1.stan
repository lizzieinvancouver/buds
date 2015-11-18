data {
  int<lower=0> N;
  vector[N] lday;
  vector[N] warm;
  vector[N] photo;
}
transformed data {
  vector[N] inter;           // interaction
  inter    <- warm .* photo;
}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  lday ~ normal(beta[1] + beta[2] * warm + beta[3] * photo + beta[4] * inter, sigma);
}