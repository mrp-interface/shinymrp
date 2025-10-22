
functions { 
  real icar_normal_lpdf(vector phi, real tau, array[] int node1, array[] int node2) {
    return -0.5 * tau * dot_self(phi[node1] - phi[node2]);
  }
}

data { 
  int<lower=1> N;
  int<lower=0> K;
  matrix[N, K] X;
  
  array[N] int y;
  array[N] int n_sample;
  
  int<lower=1> N_race;
  array[N] int<lower=1, upper=N_race> J_race;
  
  int<lower=1> N_age;
  array[N] int<lower=1, upper=N_age> J_age;
  
  int<lower=1> N_time;
  array[N] int<lower=1, upper=N_time> J_time;
  
  int<lower=1> N_zip;
  array[N] int<lower=1, upper=N_zip> J_zip;
  
  int<lower=0> N_edges_zip;
  array[N_edges_zip] int<lower=1, upper=N_zip> node1_zip;
  array[N_edges_zip] int<lower=1, upper=N_zip> node2_zip;
}

parameters { 
  real intercept;
  vector[K] beta;
  real<lower=0> lambda_race;
  vector[N_race] z_race;
  real<lower=0> lambda_age;
  vector[N_age] z_age;
  real<lower=0> lambda_time;
  vector[N_time] z_time;
  real<lower=0> lambda_zip;
  sum_to_zero_vector[N_zip] z_zip;
  real<lower=0> tau_zip;
}

transformed parameters { 
  real<lower=0> scaled_lambda_race = lambda_race;
  vector[N_race] a_race = z_race * scaled_lambda_race;
  real<lower=0> scaled_lambda_age = lambda_age;
  vector[N_age] a_age = z_age * scaled_lambda_age;
  real<lower=0> scaled_lambda_time = lambda_time;
  vector[N_time] a_time = z_time * scaled_lambda_time;
  real<lower=0> scaled_lambda_zip = lambda_zip;
  vector[N_zip] a_zip = z_zip * scaled_lambda_zip;
  vector<lower=0, upper=1>[N] p = inv_logit(intercept + X * beta + a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]);
}

model { 
  y ~ binomial(n_sample, p);
  intercept ~ normal(0, 5);
  beta[1] ~ normal(0, 3);
  z_race ~ std_normal();
  z_age ~ std_normal();
  z_time ~ std_normal();
  z_zip ~ icar_normal(tau_zip, node1_zip, node2_zip);
  lambda_race ~ normal(0, 3);
  lambda_age ~ normal(0, 3);
  lambda_time ~ normal(0, 3);
  lambda_zip ~ normal(0, 3);
  tau_zip ~ gamma(2, 2);
}
  
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = binomial_lpmf(y[n] | n_sample[n], p[n]);
  }
}
