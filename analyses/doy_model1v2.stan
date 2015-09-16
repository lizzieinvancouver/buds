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
  vector[1] b_interc;
  vector[1] b_warm;
  vector[1] b_photo;
  vector[1] b_warmxphoto;
  real<lower=0> sigma;
}
model {
  lday ~ normal(b_interc + b_warm * warm + b_photo * photo + b_warmxphoto * inter, sigma);
}