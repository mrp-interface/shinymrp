functions {
  /**
   * Component-wise constrain sum-to-zero vectors
   *
   * @param phi unconstrained vector of zero-sum slices
   * @param idxs component start and end indices
   * @param sizes component sizes
   * @return vector phi_ozs, the vector whose slices sum to zero
   */
  vector zero_sum_components_lp(vector phi, array[ , ] int idxs, array[] int sizes) {
    vector[sum(sizes)] phi_ozs;
    int idx_phi = 1;
    int idx_ozs = 1;
    for (i in 1:size(sizes)) {
      int n = sizes[i];
      phi_ozs[idx_ozs : idx_ozs + n - 1] = 
        zero_sum_constrain_lp(segment(phi, idx_phi, n - 1));
      idx_phi += n - 1;
      idx_ozs += n;
    }
    return phi_ozs;
  }

  /**
   * Constrain sum-to-zero vector
   *
   * @param y unconstrained zero-sum parameters
   * @return vector z, the vector whose slices sum to zero
   */
  vector zero_sum_constrain_lp(vector y) {
    int N = num_elements(y);
    vector[N + 1] z = zeros_vector(N + 1);
    real sum_w = 0;
    for (ii in 1:N) {
      int i = N - ii + 1; 
      real n = i;
      real w = y[i] * inv_sqrt(n * (n + 1));
      sum_w += w;
      z[i] += sum_w;     
      z[i + 1] -= w * n;    
    }
    return z;
  }

  //** log PDF of intrinsic CAR prior
  real icar_normal_lpdf(vector phi, array[] int node1, array[] int node2) {
    return -0.5 * dot_self(phi[node1] - phi[node2]);
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

  int<lower=0, upper=N_zip> N_components;
  array[N_components] int<lower=1, upper=N_zip> component_size;
  vector<lower=0>[N_components] scaling_factor;
}

transformed data {
  int N_zip_connected = sum(component_size);
  int N_zip_singletons = N_zip - N_zip_connected;
  if (N_zip_singletons < 0) {
    reject("Inconsistent inputs: sum(component_size) > N");
  }
  vector<lower=0>[N_zip_connected] taus;
  array[N_components, 2] int component_idxs;
  int idx = 1;
  for (n in 1:N_components) {
    taus[idx: component_size[n] + idx - 1]
      = rep_vector(scaling_factor[n], component_size[n]);
    component_idxs[n, 1] = idx;
    component_idxs[n, 2] = component_size[n] + idx - 1;
    idx += component_size[n];
  }
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
  vector[N_zip_connected] theta_zip; 
  vector[N_zip_connected - N_components] phi_zip_raw;
  vector[N_zip_singletons] singletons_re;
}

transformed parameters { 
  real<lower=0> scaled_lambda_race = lambda_race;
  vector[N_race] a_race = z_race * scaled_lambda_race;
  real<lower=0> scaled_lambda_age = lambda_age;
  vector[N_age] a_age = z_age * scaled_lambda_age;
  real<lower=0> scaled_lambda_time = lambda_time;
  vector[N_time] a_time = z_time * scaled_lambda_time;
  real<lower=0> scaled_lambda_zip = lambda_zip;
  vector[N_zip_connected] phi_zip = zero_sum_components_lp(phi_zip_raw, component_idxs, component_size);
  vector[N_zip] z_zip;
  z_zip[1:N_zip_connected] = sqrt(1 - rho_zip) * theta_zip + sqrt(rho_zip * inv(taus)) .* phi_zip;
  z_zip[N_zip_connected + 1 : N_zip] = singletons_re;
  vector[N_zip] a_zip = z_zip * scaled_lambda_zip;
  vector<lower=0, upper=1>[N] p = inv_logit(intercept + X * beta + a_race[J_race] + a_age[J_age] + a_time[J_time] + a_zip[J_zip]);
}

model { 
  y ~ binomial(n_sample, p);
  intercept ~ normal(0, 5);
  if (K > 0) beta ~ normal(0, 3);
  z_race ~ std_normal();
  z_age ~ std_normal();
  z_time ~ std_normal();
  theta_zip ~ std_normal();
  phi_zip ~ icar_normal(node1_zip, node2_zip);
  rho_zip ~ beta(0.5, 0.5);
  lambda_race ~ normal(0, 1);
  lambda_age ~ normal(0, 1);
  lambda_time ~ normal(0, 1);
  lambda_zip ~ normal(0, 1);
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
