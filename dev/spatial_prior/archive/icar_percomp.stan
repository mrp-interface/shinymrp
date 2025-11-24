// file: icar_percomp.stan
functions {
  real icar_normal_lpdf(vector phi, int N_nodes, array[] int node1, array[] int node2) {
    return -0.5 * dot_self(phi[node1] - phi[node2]) + normal_lpdf(sum(phi) | 0, 0.001 * N_nodes);
  }
}

data {
  int<lower=1> N;
  int<lower=0> K;
  matrix[N, K] X;
  array[N] int y;
  array[N] int n_sample;

  int<lower=1> N_race;
  array[N] int<lower=1,upper=N_race> J_race;

  int<lower=1> N_zip;
  array[N] int<lower=1,upper=N_zip> J_zip;

  int<lower=0> N_edges;
  array[N_edges] int<lower=1,upper=N_zip> node1;
  array[N_edges] int<lower=1,upper=N_zip> node2;

  int<lower=1> N_comps;
  array[N_comps] int<lower=1> comp_sizes;
  array[N_comps, max(comp_sizes)] int<lower=0,upper=N_zip> comp_index; // padded with 0s
}
parameters {
  real intercept;
  vector[K] beta;

  real<lower=0> sigma_race;
  vector[N_race] z_race;

  real<lower=0> sigma_zip;
  vector[N_zip] phi_zip;
}
transformed parameters {
  vector[N_race] a_race = sigma_race * z_race;
  vector[N_zip]  a_zip  = sigma_zip  * phi_zip;
}
model {
  vector[N] eta = intercept + X * beta + a_race[J_race] + a_zip[J_zip];
  y ~ binomial_logit(n_sample, eta);

  intercept  ~ normal(0, 5);
  beta       ~ normal(0, 3);
  z_race     ~ std_normal();
  sigma_race ~ normal(0, 1);
  sigma_zip  ~ normal(0, 1);
  phi_zip ~ icar_normal(N_zip, node1, node2);
}
