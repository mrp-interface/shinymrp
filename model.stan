
data { 
  int<lower=1> N;
  array[N] int y;
  array[N] int n_sample;
  int<lower=0> K;
  matrix[N, K] X;
  real<lower=0> intercept_prior_mean;
  real<lower=0> intercept_prior_scale;
  real<lower=0> coef_prior_scale;
  real<lower=0> lambda_scale;
  real<lower=0> sens;
  real<lower=0> spec;
}

transformed data { 
  matrix[N, K] Xc;
  for (i in 1:K) {
    Xc[, i] = X[, i] - mean(X[, i]);
  }
}

parameters { 
  vector[K] b;
  real Intercept;
}

transformed parameters { 
  vector<lower=0, upper=1>[N] p = inv_logit(Intercept + Xc * b);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);
}

model { 
  y ~ binomial(n_sample, p_sample);
  Intercept ~ normal(0, 5);
  b[1] ~ normal(0, 1);
}
  
