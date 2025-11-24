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

  // Reduced-rank ICAR basis from R: size N_zip x N_pos
  int<lower=0> N_pos;
  matrix[N_zip, N_pos] R; // already BYM2-scaled so that GM(marg var)=1
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
  real<lower=0, upper=1> rho_zip;

  // BYM2 parts:
  vector[N_zip] theta_zip;       // IID part
  vector[N_pos] eta_zip;         // structured reduced-rank scores
}

transformed parameters {
  vector[N_race] a_race = lambda_race * z_race;
  vector[N_age]  a_age  = lambda_age  * z_age;
  vector[N_time] a_time = lambda_time * z_time;

  // Structured ICAR field (already per-component constrained via R)
  vector[N_zip] phi_zip = R * eta_zip;

  // BYM2 mixture (R is scaled, so GM(marg var of phi_zip) = 1)
  vector[N_zip] z_zip = sqrt(rho_zip) * phi_zip + sqrt(1 - rho_zip) * theta_zip;

  // ZIP effect with global scale
  vector[N_zip] a_zip = lambda_zip * z_zip;

  vector<lower=0, upper=1>[N] p = inv_logit(intercept + X * beta
              + a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]);
}

model {
  // Likelihood
  y ~ binomial(n_sample, p);

  // Priors
  intercept ~ normal(0, 5);
  if (K > 0) beta ~ normal(0, 3);

  z_race ~ std_normal();
  z_age  ~ std_normal();
  z_time ~ std_normal();

  theta_zip ~ std_normal();  // IID part
  eta_zip   ~ std_normal();  // structured scores

  rho_zip ~ beta(0.5, 0.5);

  lambda_race ~ normal(0, 1);
  lambda_age  ~ normal(0, 1);
  lambda_time ~ normal(0, 1);
  lambda_zip  ~ normal(0, 1);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = binomial_lpmf(y[n] | n_sample[n], p[n]);
  }
  real sigma_struct = lambda_zip * sqrt(rho_zip);
  real sigma_iid    = lambda_zip * sqrt(1 - rho_zip);
  real phi_sum = sum(phi_zip);
}
