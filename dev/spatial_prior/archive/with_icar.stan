
data { 
  int<lower=1> N;
  int<lower=0> K;
  matrix[N, K] X;

  array[N] int y;
  array[N] int n_sample;
  
  int<lower=1> N_race;
  array[N] int<lower=1, upper=N_race> J_race;
  
  int<lower=1> N_zip;
  array[N] int<lower=1, upper=N_zip> J_zip;

  int<lower=1> N_edges;
  int<lower=1, upper=N_zip> node1[N_edges];
  int<lower=1, upper=N_zip> node2[N_edges];
  
  real<lower=0> sens;
  real<lower=0> spec;
}

parameters { 
  real intercept;
  vector[K] beta;
  real<lower=0> lambda_race;
  vector[N_race] z_race;
  vector[N_zip] a_zip;
}

transformed parameters { 
  real<lower=0> scaled_lambda_race = lambda_race;
  vector[N_race] a_race = z_race * scaled_lambda_race;
  vector<lower=0, upper=1>[N] p = inv_logit(intercept + X * beta + a_race[J_race] + a_zip[J_zip]);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);
}

model { 
  y ~ binomial(n_sample, p_sample);
  intercept ~ normal(0, 5);
  beta ~ normal(0, 3);
  z_race ~ std_normal();
  lambda_race ~ normal(0, 3);
  target += -0.5 * dot_self(a_zip[node1] - a_zip[node2]);
  sum(a_zip) ~ normal(0, 0.001 * N_zip);
}
  
