
data { 
  int<lower=1> N;
  array[N] int y;
  array[N] int n_sample;
  int<lower=0> K;
  matrix[N, K] X;
  int<lower=1> N_race;
  array[N] int<lower=1, upper=N_race> J_race;
  int<lower=1> N_age;
  array[N] int<lower=1, upper=N_age> J_age;
  int<lower=1> N_time;
  array[N] int<lower=1, upper=N_time> J_time;
  int<lower=1> N_timeage;
  array[N] int<lower=1, upper=N_timeage> J_timeage;
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
  vector<lower=0.0001>[N_race] lambda_race;
  vector[N_race] z_race;
  vector<lower=0.0001>[N_age] lambda_age;
  vector[N_age] z_age;
  vector<lower=0.0001>[N_time] lambda_time;
  vector[N_time] z_time;
  vector<lower=0.0001>[N_timeage] lambda_timeage;
  vector[N_timeage] z_timeage;
}

transformed parameters { 
  vector[N_race] a_race = z_race .* lambda_race;
  vector[N_age] a_age = z_age .* lambda_age;
  vector[N_time] a_time = z_time .* lambda_time;
  vector[N_timeage] a_timeage = z_timeage .* lambda_timeage;
  vector<lower=0, upper=1>[N] p = inv_logit(Intercept + Xc * b + a_race[J_race] + a_age[J_age] + a_time[J_time] + a_timeage[J_timeage]);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);
}

model { 
  y ~ binomial(n_sample, p_sample);
  Intercept ~ normal(0, 5);
  b[1] ~ normal(0, 1);
  z_race ~ std_normal();
  z_age ~ std_normal();
  z_time ~ std_normal();
  z_timeage ~ std_normal();
  lambda_race ~ normal(0, 1);
  lambda_age ~ normal(0, 1);
  lambda_time ~ normal(0, 1);
  lambda_timeage ~ normal(0, 1);
}
  
