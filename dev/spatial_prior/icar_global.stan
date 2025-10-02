// file: icar_global.stan
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
}
parameters {
  real intercept;
  vector[K] beta;

  real<lower=0> sigma_race;
  vector[N_race] z_race;

  real<lower=0> sigma_zip;
  vector[N_zip] phi_zip;  // ICAR latent field
}
transformed parameters {
  vector[N_race] a_race = sigma_race * z_race;
  vector[N_zip]  a_zip  = sigma_zip  * phi_zip;
}
model {
  // Likelihood with logit link
  vector[N] eta = intercept + X * beta + a_race[J_race] + a_zip[J_zip];
  y ~ binomial_logit(n_sample, eta);

  // Priors
  intercept  ~ normal(0, 5);
  beta       ~ normal(0, 3);
  z_race     ~ std_normal();
  sigma_race ~ normal(0, 1);
  sigma_zip  ~ normal(0, 1);

  // ICAR roughness penalty
  if (N_edges > 0)
    target += -0.5 * dot_self(phi_zip[node1] - phi_zip[node2]);

  // *** Single global soft sum-to-zero ***
  sum(phi_zip) ~ normal(0, 0.001 * N_zip);
}
