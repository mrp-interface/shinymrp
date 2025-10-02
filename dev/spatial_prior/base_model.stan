
data { 
  int<lower=1> N;
  int<lower=0> K;
  matrix[N, K] X;
  int<lower=1> N_pop;
  int<lower=0> K_pop;
  matrix[N_pop, K_pop] X_pop;
  
  array[N] int y;
  array[N] int n_sample;
  
  int<lower=1> N_race;
  array[N] int<lower=1, upper=N_race> J_race;
  int<lower=1> N_race_pop;
  array[N_pop] int<lower=1, upper=N_race_pop> J_race_pop;
  
  int<lower=1> N_zip;
  array[N] int<lower=1, upper=N_zip> J_zip;
  int<lower=1> N_zip_pop;
  array[N_pop] int<lower=1, upper=N_zip_pop> J_zip_pop;
  
  vector<lower=0, upper=1>[N_pop] P_overall_pstrat;
  
  int<lower=1> N_sex_pstrat;
  array[N_pop] int<lower=1, upper=N_sex_pstrat> J_sex_pstrat;
  vector<lower=0, upper=1>[N_pop] P_sex_pstrat;
  
  int<lower=1> N_race_pstrat;
  array[N_pop] int<lower=1, upper=N_race_pstrat> J_race_pstrat;
  vector<lower=0, upper=1>[N_pop] P_race_pstrat;
  
  int<lower=1> N_age_pstrat;
  array[N_pop] int<lower=1, upper=N_age_pstrat> J_age_pstrat;
  vector<lower=0, upper=1>[N_pop] P_age_pstrat;
  
  int<lower=1> N_county_pstrat;
  array[N_pop] int<lower=1, upper=N_county_pstrat> J_county_pstrat;
  vector<lower=0, upper=1>[N_pop] P_county_pstrat;
  
  int<lower=1> N_time_pstrat;
  array[N_pop] int<lower=1, upper=N_time_pstrat> J_time_pstrat;
  
  real<lower=0> sens;
  real<lower=0> spec;
}

parameters { 
  real intercept;
  vector[K] beta;
  real<lower=0> lambda_race;
  vector[N_race] z_race;
  real<lower=0> lambda_zip;
  vector[N_zip] z_zip;
}

transformed parameters { 
  real<lower=0> scaled_lambda_race = lambda_race;
  vector[N_race] a_race = z_race * scaled_lambda_race;
  real<lower=0> scaled_lambda_zip = lambda_zip;
  vector[N_zip] a_zip = z_zip * scaled_lambda_zip;
  vector<lower=0, upper=1>[N] p = inv_logit(intercept + X * beta + a_race[J_race] + a_zip[J_zip]);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);
}

model { 
  y ~ binomial(n_sample, p_sample);
  intercept ~ normal(0, 5);
  beta[1] ~ normal(0, 3);
  z_race ~ std_normal();
  z_zip ~ std_normal();
  lambda_race ~ normal(0, 3);
  lambda_zip ~ normal(0, 3);
}
  
