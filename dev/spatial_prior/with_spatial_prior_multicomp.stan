// BYM2-like binomial model with ICAR (multi-component) on ZIP/ZCTA lattice.

functions {
  // ICAR log-density with a soft mean/sum-to-zero constraint per component.
  real icar_normal_components_lpdf(vector phi,
                                   int N_nodes,
                                   int N_edges,
                                   array[] int node1,
                                   array[] int node2,
                                   int C,
                                   array[] int comp_sizes,
                                   array[,] int comp_index) {
    // Edge-difference penalty over all edges
    real lp = -0.5 * dot_self(phi[node1] - phi[node2]);

    // Soft per-component centering (identifiability in the null space)
    for (c in 1:C) {
      int nc = comp_sizes[c];
      vector[nc] phi_c;
      for (j in 1:nc)
        phi_c[j] = phi[ comp_index[c, j] ];
      // Using a lattice-wide scale so tiny components aren’t over-constrained.
      lp += normal_lpdf( sum(phi_c) | 0, 0.001 * N_nodes );
    }
    return lp;
  }
}

data {
  // Rows and design
  int<lower=1> N;                 // number of (race,zip) observations
  int<lower=0> K;                 // number of fixed effects
  matrix[N, K] X;

  // Binomial outcomes
  array[N] int y;
  array[N] int n_sample;

  // Race indexing
  int<lower=1> N_race;
  array[N] int<lower=1, upper=N_race> J_race;

  // ZIP/ZCTA indexing for the lattice
  int<lower=1> N_zip;
  array[N] int<lower=1, upper=N_zip> J_zip;

  // Test characteristics (ideally in [0,1])
  real<lower=0> sens;
  real<lower=0> spec;

  // --- Graph structure (BYM2 spatial prior) ---
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=N_zip> node1;
  array[N_edges] int<lower=1, upper=N_zip> node2;

  // Per-node ICAR scaling (constant within each component; 1 for isolates)
  vector<lower=0>[N_zip] inv_sqrt_scale_factor;

  // Connected components (for per-component centering)
  int<lower=1> C;                              // number of components
  int<lower=1> max_comp_size;
  array[C] int<lower=1, upper=max_comp_size> comp_sizes;
  array[C, max_comp_size] int<lower=0, upper=N_zip> comp_index; // 0 = padding

  // Indices of non-isolated nodes (degree > 0)
  int<lower=0> N_noniso;
  array[N_noniso] int<lower=1, upper=N_zip> noniso_idx;
}

parameters {
  // Fixed effects
  real intercept;
  vector[K] beta;

  // Race random effect (iid)
  real<lower=0> lambda_race;
  vector[N_race] z_race;

  // BYM2 at ZIP level
  real<lower=0> sigma_zip;                // overall SD of spatial effect
  real<lower=0, upper=1> rho;             // fraction of variance in structured (ICAR) piece
  vector[N_zip]    theta;                 // iid component ~ N(0,1)
  vector[N_noniso] phi_noniso;            // structured (ICAR) only on non-isolates (unscaled)
}

transformed parameters {
  // Race effect
  vector[N_race] a_race = lambda_race * z_race;

  // Assemble φ on the full lattice: zeros at isolates, free params at non-isolates
  vector[N_zip] phi = rep_vector(0, N_zip);
  if (N_noniso > 0)
    phi[noniso_idx] = phi_noniso;

  // BYM2 combination (scale ICAR to unit variance first)
  vector[N_zip] u_struct = phi ./ inv_sqrt_scale_factor;
  vector[N_zip] a_zip = sigma_zip * ( sqrt(rho) * u_struct + sqrt(1 - rho) * theta );

  // Observation model (with sens/spec adjustment)
  vector<lower=0, upper=1>[N] p = inv_logit(intercept + X * beta + a_race[J_race] + a_zip[J_zip]);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);
}

model {
  y ~ binomial(n_sample, p_sample);

  intercept ~ normal(0, 5);
  beta      ~ normal(0, 3);

  z_race     ~ std_normal();
  lambda_race ~ normal(0, 3);

  theta ~ std_normal();

  // ICAR prior with per-component soft centering.
  target += icar_normal_components_lpdf(phi | N_zip, N_edges, node1, node2,
                                        C, comp_sizes, comp_index);

  sigma_zip ~ normal(0, 1);
  rho       ~ beta(2, 2);
}
