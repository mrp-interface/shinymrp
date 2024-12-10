#' model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
#' @import purrr
#' @import stringr
create_formula <- function(effects, dat) {
  fixed <- c(names(effects$fixed), names(effects$interaction$fixed_slope))
  varying_intercept <- c(names(effects$varying), names(effects$interaction$varying_intercept))
  varying_slope <- names(effects$interaction$varying_slope)

  s_fixed <- if(length(fixed) > 0) paste(paste0(" + ", fixed), collapse = '') else ''
  s_varying_intercept <- if(length(varying_intercept) > 0) paste(paste0(" + (1 | ", varying_intercept, ")"), collapse = '') else ''
  s_varying_slope <- if(length(varying_slope) > 0) paste(map(varying_slope, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(paste0(" + (", ss[2], " | ", ss[1], ')'))
  }), collapse = '') else ''


  formula <- paste0("1", s_fixed, s_varying_intercept, s_varying_slope)

  return(formula)
}


check_prior_syntax <- function(s) {
  if(s == "") {
    return(TRUE)
  }

  patterns <- list(
    normal = "^normal\\([0-9]+, [1-9][0-9]*\\)$",
    student_t = "^student_t\\([1-9][0-9]*, [0-9]+, [1-9][0-9]*)$",
    structured = "^structured$"
  )

  dist <- strsplit(s, '\\(')[[1]][1]

  if(dist %in% names(patterns)) {
    return(grepl(patterns[[dist]], s))
  } else {
    return(FALSE)
  }
}

# filter interactions for structured prior
filter_interactions <- function(interactions, fixed_effects, dat) {
  bool <- map_lgl(interactions, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- data_type(dat[[ss[1]]])
    type2 <- data_type(dat[[ss[2]]])
    
    return((type1 == "cat" & !ss[1] %in% fixed_effects) |
           (type2 == "cat" & !ss[2] %in% fixed_effects))
  })
  
  return(interactions[bool])
}

# Keep interaction in expected order for Stan code generation:
# binary first, then categorical, then continuous.
# For interaction between 2 categorical variables,
# the one included as a fixed effect comes first.
sort_interactions <- function(interactions, fixed_effects, dat) {
  interactions <- map_chr(interactions, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- data_type(dat[[ss[1]]], num = TRUE)
    type2 <- data_type(dat[[ss[2]]], num = TRUE)
    
    if(type1 > type2 | (type1 == 2 & type2 == 2 & ss[2] %in% fixed_effects)) {
      s <- paste0(ss[2], ':', ss[1])
    }
    
    return(s)
  })
  
  return(interactions)
}

create_interactions <- function(fixed_effects, varying_effects, dat) {
  main_effects <- c(fixed_effects, varying_effects)
  
  if(n_distinct(main_effects) <= 1) {
    return(list())
  }

  # create unique pairs
  df <- expand.grid(
    eff1 = main_effects,
    eff2 = main_effects 
  ) |>
    mutate_all(as.character) |>
    filter(eff1 != eff2)

  df <- df[apply(df, 1, function(x) x[1] <= x[2]), ]
  int <- paste0(df$eff1, ":", df$eff2)
  
  return(int)
}

interaction_levels <- function(levels1, levels2) {
  numcat1 <- n_distinct(levels1)
  numcat2 <- n_distinct(levels2)

  if(numcat1 == 2 | numcat2 == 2) {
    levels_interaction <- levels1 * levels2
    levels_interaction[levels_interaction == 0] <- 1
  } else {
    levels_interaction <- (levels1 - 1) * numcat2 + levels2
  }

  return(levels_interaction)
}

group_fixed <- function(fixed, dat) {
  out <- list(
    cat = list(),
    bincont = list()
  )

  for(s in names(fixed)) {
    if(data_type(dat[[s]]) == "cat") {
      s_raw <- paste0(s, "_raw")
      levels <- unique(dat[[s_raw]])

      dummy <- paste0(s, ".", levels[2:length(levels)])
      for(d in dummy) {
        out$cat[[d]] <- fixed[[s]]
      }
    } else {
      out$bincont[[s]] <- fixed[[s]]
    }
  }

  return(out)
}

# Note: this function assumes the interaction terms are in the specific order
# produced by create_interactions
group_interactions <- function(interactions, fixed_effects, dat) {
  out <- list(
    fixed_slope = list(),
    varying_slope = list(),
    varying_intercept = list(),
    varying_intercept_special = list()
  )

  for(s in names(interactions)) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- data_type(dat[[ss[1]]])
    type2 <- data_type(dat[[ss[2]]])

    # binary x continuous or continuous x continuous
    if((type1 == "cont" & type2 == "cont") |
       (type1 == "bin" & type2 == "bin") |
       (type1 == "cont" & type2 == "bin") |
       (type1 == "bin" & type2 == "cont")) {
      out$fixed_slope[[s]] <- interactions[[s]]

      # categorical x continuous
    } else if (type1 == "cat" & type2 == "cont") {
      out$varying_slope[[s]] <- interactions[[s]]
      
      # binary x categorical
    } else if (type1 == "bin" & type2 == "cat") {
      out$varying_intercept_special[[s]] <- interactions[[s]]
      
      # categorical x categorical
    } else if (type1 == "cat" & type2 == "cat") {
      if (ss[1] %in% names(fixed_effects)) {
        out$varying_intercept_special[[s]] <- interactions[[s]]
      } else {
        out$varying_intercept[[s]] <- interactions[[s]]
      }

    } else {
      print("Unrecognized interaction")
    }
  }

  return(out)
}

group_effects <- function(effects, dat) {
  out <- list()

  # global intercept
  out$Intercept <- effects$Intercept

  # fixed main effects
  out$fixed <- group_fixed(effects$fixed, dat)

  # varying main effects
  out$varying <- effects$varying
  
  # reorder terms in interactions
  if(!is.null(effects$interaction)) {
    names(effects$interaction) <- sort_interactions(
      names(effects$interaction),
      names(effects$fixed),
      dat
    ) 
  }

  # interactions without structured priors
  wo_struct <- effects$interaction[effects$interaction != "structured"]
  out$interaction <- group_interactions(wo_struct, effects$fixed, dat)

  # interactions with structured priors
  w_struct <- effects$interaction[effects$interaction == "structured"]
  out$structured <- group_interactions(w_struct, effects$fixed, dat)


  return(out)
}

ungroup_effects <- function(effects) {
  # for cleaner code
  out <- list(
    Intercept = effects$Intercept,
    m_fix_bc = effects$fixed$bincont,
    m_fix_c = effects$fixed$cat,
    m_var = effects$varying,
    i_fixsl = effects$interaction$fixed_slope,
    i_varsl = effects$interaction$varying_slope,
    i_varit = effects$interaction$varying_intercept,
    i_varits = effects$interaction$varying_intercept_special,
    s_varsl = effects$structured$varying_slope,
    s_varit = effects$structured$varying_intercept,
    s_varits = effects$structured$varying_intercept_special
  )

  return(out)
}



# Stan code and data generation functions
data_ <- function(effects) {
  scode <- "
  int<lower=1> N;
  array[N] int y;
  array[N] int n_sample;
  int<lower=0> K;
  matrix[N, K] X;
  int<lower=1> N_pop;
  int<lower=0> K_pop;
  matrix[N_pop, K_pop] X_pop;"

  for(s in names(effects$m_var)) {
    scode <- paste0(scode, str_interp("
  int<lower=1> N_${s};
  array[N] int<lower=1, upper=N_${s}> J_${s};
  int<lower=1> N_${s}_pop;
  array[N_pop] int<lower=1, upper=N_${s}_pop> J_${s}_pop;"))
  }

  int <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  for(s in gsub(':', '', names(int))) {
    scode <- paste0(scode, str_interp("
  int<lower=1> N_${s};
  array[N] int<lower=1, upper=N_${s}> J_${s};
  int<lower=1> N_${s}_pop;
  array[N_pop] int<lower=1, upper=N_${s}_pop> J_${s}_pop;
  array[N_${s}] int<lower=1, upper=N_${s}_pop> I_${s};"))
  }

  scode <- paste0(scode, "
  real<lower=0> sens;
  real<lower=0> spec;")

  return(scode)
}

parameters_ <- function(effects) {
  scode <- "
  real Intercept;
  vector[K] beta;"

  # varying main effect
  for(s in names(effects$m_var)) {
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${s};
  vector[N_${s}] z_${s};"))
  }

  # varying-intercept interaction without structured prior
  for(s in names(c(effects$i_varit, effects$i_varits))) {
    v <- gsub(':', '', s)
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${v};
  vector[N_${v}] z_${v};"))
  }

  # varying-slope interaction without structured prior
  for(s in names(effects$i_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda2_${ss[1]};
  vector[N_${ss[1]}] z2_${ss[1]};"))
  }

  # varying-intercept interaction with structured prior
  for(s in names(c(effects$s_varit, effects$s_varits))) {
    v <- gsub(':', '', s)
    scode <- paste0(scode, str_interp("
  vector[N_${v}] z_${v};"))
  }

  # varying-slope interaction with structured prior
  for(s in names(effects$s_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    scode <- paste0(scode, str_interp("
  vector[N_${ss[1]}] z2_${ss[1]};"))
  }

  # include the parameters below if structured prior is used
  int_struct <- c(effects$s_varsl, effects$s_varit, effects$s_varits)
  if(length(int_struct) > 0) {
    scode <- paste0(scode, "
  real<lower=0> tau;
  real<lower=0> delta;")
  }

  return(scode)
}

transformed_parameters_ <- function(effects) {
  scode <- ""

  struct_effects <- c(
    names(effects$s_varsl) |>
      map(function(s) strsplit(s, ':')[[1]][1]),
    names(c(effects$s_varit, effects$s_varits)) |>
      map(function(s) strsplit(s, ':')[[1]]) %>%
      do.call(c, .)
  )

  # varying main effects
  for(s in names(effects$m_var)) {
    if(s %in% struct_effects) {
      scode <- paste0(scode, str_interp("
  real<lower=0> scaled_lambda_${s} = lambda_${s} * tau;"))
    } else {
      scode <- paste0(scode, str_interp("
  real<lower=0> scaled_lambda_${s} = lambda_${s};"))
    }

    scode <- paste0(scode, str_interp("
  vector[N_${s}] a_${s} = z_${s} * scaled_lambda_${s};"))
  }

  # varying-intercept interaction without structured prior
  for(s in names(c(effects$i_varit, effects$i_varits))) {
    v <- gsub(':', '', s)
    scode <- paste0(scode, str_interp("
  vector[N_${v}] a_${v} = z_${v} * lambda_${v};"))
  }

  # varying-slope interaction without structured prior
  for(s in names(effects$i_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    scode <- paste0(scode, str_interp("
  vector[N_${ss[1]}] b_${ss[1]} = z2_${ss[1]} * lambda2_${ss[1]};"))
  }

  # varying-intercept interaction with structured prior
  for(s in names(c(effects$s_varit))) {
    ss <- strsplit(s, split = ':')[[1]]
    v <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${v} = lambda_${ss[1]} * lambda_${ss[2]} * delta * tau;
  vector[N_${v}] a_${v} = z_${v} * lambda_${v};"))
  }

  # varying-intercept interaction with structured prior (with binary variable)
  for(s in names(c(effects$s_varits))) {
    ss <- strsplit(s, split = ':')[[1]]
    v <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${v} = lambda_${ss[2]} * delta * tau;
  vector[N_${v}] a_${v} = z_${v} * lambda_${v};"))
  }

  # varying-slope interaction with structured prior
  for(s in names(effects$s_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda2_${ss[1]} = lambda_${ss[1]} * delta * tau;
  vector[N_${ss[1]}] b_${ss[1]} = z2_${ss[1]} * lambda2_${ss[1]};"))
  }


  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)

  scode <- paste0(scode, sprintf("
  vector<lower=0, upper=1>[N] p = inv_logit(Intercept + X * beta%s%s%s);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);",
  paste(map(names(effects$m_var), ~ str_interp(" + a_${.x}[J_${.x}]")), collapse = ""),
  paste(map(gsub(':', '', names(int_varit)), ~ str_interp(" + a_${.x}[J_${.x}]")), collapse = ""),
  paste(map(names(int_varsl), function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(str_interp(" + b_${ss[1]}[J_${ss[1]}] .* X[:, ${which(names(effects$m_fix_bc) == ss[2])}]"))
  }), collapse = "")))

  return(scode)
}

model_ <- function(effects) {
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)
  int_varsl_wo_struct <- effects$i_varsl
  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varit_wo_struct <- c(effects$i_varit, effects$i_varits)

  scode <- paste0("
  y ~ binomial(n_sample, p_sample);",
  str_interp("\n  Intercept ~ ${effects$Intercept$Intercept};"),
  if(length(fixed) > 0) paste(map(1:length(fixed), ~ str_interp("\n  beta[${.x}] ~ ${fixed[[.x]]};")), collapse = ""),
  paste(map(names(effects$m_var), ~ str_interp("\n  z_${.x} ~ std_normal();")), collapse = ""),
  paste(map(names(int_varit), ~ str_interp("\n  z_${gsub(':', '', .x)} ~ std_normal();")), collapse = ""),
  paste(map(names(int_varsl), ~ str_interp("\n  z2_${strsplit(.x, split = ':')[[1]][1]} ~ std_normal();")), collapse = ""),
  paste(map(names(effects$m_var), ~ str_interp("\n  lambda_${.x} ~ ${effects$m_var[[.x]]};")), collapse = ""),
  paste(map(names(int_varit_wo_struct), ~ str_interp("\n  lambda_${gsub(':', '', .x)} ~ ${int_varit[[.x]]};")), collapse = ""),
  paste(map(names(int_varsl_wo_struct), ~ str_interp("\n  lambda2_${strsplit(.x, split = ':')[[1]][1]} ~ ${int_varsl[[.x]]};")), collapse = "")
  )

  # include the parameters below if structured prior is used
  int_struct <- c(effects$s_varsl, effects$s_varit, effects$s_varits)
  if(length(int_struct) > 0) {
    scode <- paste0(scode, str_interp("
  tau ~ ${GLOBAL$default_priors$global_scale};
  delta ~ ${GLOBAL$default_priors$local_scale};"))
  }

  return(scode)
}

generated_quantities_ <- function(effects) {
  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)
  scode <- ""

  for(s in gsub(':', '', names(int_varit))) {
    scode <- paste0(scode, str_interp("
  vector[N_${s}_pop] a_${s}_pop;
  if(N_${s} == N_${s}_pop) {
    a_${s}_pop = a_${s};
  } else {
    a_${s}_pop = to_vector(normal_rng(rep_vector(0, N_${s}_pop), rep_vector(lambda_${s}, N_${s}_pop)));
    a_${s}_pop[I_${s}] = a_${s};
  }"))
  }

  scode <- paste0(scode, sprintf("
  vector<lower=0, upper=1>[N_pop] p_pop = inv_logit(Intercept + X_pop * beta%s%s%s);",
  paste(map(names(effects$m_var), ~ str_interp(" + a_${.x}[J_${.x}_pop]")), collapse = ""),
  paste(map(gsub(':', '', names(int_varit)), ~ str_interp(" + a_${.x}_pop[J_${.x}_pop]")), collapse = ""),
  paste(map(names(int_varsl), function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(str_interp(" + b_${ss[1]}[J_${ss[1]}_pop] .* X_pop[:, ${which(names(effects$m_fix_bc) == ss[2])}]"))
  }), collapse = "")))


  scode <- paste0(scode, "
  array[N] int<lower = 0> y_rep = binomial_rng(n_sample, p_sample);
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = binomial_lpmf(y[n] | n_sample[n], p_sample[n]);
  }")

  return(scode)
}

make_stancode <- function(effects) {

  scode <- str_interp("
data { ${data_(effects)}
}

parameters { ${parameters_(effects)}
}

transformed parameters { ${transformed_parameters_(effects)}
}

model { ${model_(effects)}
}

generated quantities { ${generated_quantities_(effects)}
}
  ")

  return(scode)
}

make_standata <- function(
  input_data,
  new_data,
  effects,
  sens,
  spec
) {
  stan_data <- list(
    N = nrow(input_data),
    N_pop = nrow(new_data),
    y = input_data$positive,
    n_sample = input_data$total,
    sens = sens,
    spec = spec
  )

  holder <- list(
    input = input_data,
    new = new_data
  )

  for(name in names(holder)) {
    dat <- holder[[name]]
    subfix <- if(name == "input") '' else "_pop"

    # fixed main effects (continuous & binary)
    X_cont <- dat |>
      select(all_of(names(effects$m_fix_bc))) |>
      data.matrix()

    # fixed main effects (categorical)
    X_cat <- map(names(effects$m_fix_c), function(s) {
      ss <- strsplit(s, split = '\\.')[[1]]
      return(as.integer(dat[[paste0(ss[1], "_raw")]] == ss[2]))
    }) %>%
      do.call(cbind, .)

    # interaction between fixed effects
    X_int <- map(names(effects$i_fixsl), function(s) {
      ss <- strsplit(s, split = ':')[[1]]
      return(dat[[ss[1]]] * dat[[ss[2]]])
    }) %>%
      do.call(cbind, .)

    # all fixed effects
    X <- cbind(X_cont, X_cat, X_int)
    stan_data[[paste0('X', subfix)]] <- X
    stan_data[[paste0('K', subfix)]] <- ncol(X)

    # varying main effects
    for(s in names(c(effects$m_var))) {
      stan_data[[str_interp("N_${s}${subfix}")]] <- n_distinct(dat[[s]])
      stan_data[[str_interp("J_${s}${subfix}")]] <- dat[[s]]
    }

    # interactions
    int <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
    for(s in names(int)) {
      ss <- strsplit(s, split = ':')[[1]]
      v <- paste0(ss[1], ss[2])
      int_lvls <- interaction_levels(dat[[ss[1]]], dat[[ss[2]]])
      unq_int_lvls <- sort(unique(int_lvls))
      n_int_lvls <- length(unq_int_lvls)
      stan_data[[str_interp("N_${v}${subfix}")]] <- n_int_lvls
      stan_data[[str_interp("J_${v}${subfix}")]] <- factor(int_lvls, levels = unq_int_lvls, labels = 1:n_int_lvls) |> as.numeric()

      if(name == "input") {
        stan_data[[str_interp("I_${v}")]] <- unq_int_lvls
      }
    }
  }

  return(stan_data)
}

run_stan <- function(
  input_data,
  new_data,
  effects,
  n_iter = 1000,
  n_chains = 4,
  seed = NULL,
  sens = 0.7,
  spec = 0.999,
  code_fout = NULL
) {

  stan_code <- make_stancode(effects)
  stan_data <- make_standata(input_data, new_data, effects, sens, spec)

  if(!is.null(code_fout)) {
    writeLines(stan_code, code_fout)
  }

  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stan_code),
    cpp_options = list(stan_threads = TRUE)
  )

  fit <- mod$sample(
    data = stan_data,
    iter_warmup = n_iter/2,
    iter_sampling = n_iter/2,
    chains = n_chains,
    parallel_chains = n_chains,
    threads_per_chain = 1,
    refresh = n_iter / 10,
    seed = seed
  )


  return(list(fit, stan_code))
}

extract_predict <- function(fit, ppc_samples=10) {
  pred_mat <- fit$draws(
    variables = "p_pop",
    format = "draws_matrix"
  )|> t()

  # generate replicated data
  yrep_mat <- fit$draws(
    variables = "y_rep",
    format = "draws_matrix"
  )|> t()

  yrep_mat <- yrep_mat[, sample(ncol(yrep_mat), ppc_samples)]

  return(list(pred_mat, yrep_mat))
}

extract_parameters <- function(fit, effects) {
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)

  # fixed main effects + fixed-slope interaction
  df_fixed <- data.frame()
  if(length(fixed) > 0) {
    df_fixed <- fit$summary(variables = c("Intercept", "beta")) |>
      select(mean, sd, q5, q95, rhat, ess_bulk, ess_tail) |>
      as.data.frame()

    names(df_fixed) <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Convergence", "Bulk_ESS", "Tail_ESS")
    row.names(df_fixed) <- c("Intercept", names(fixed))
  }

  # varying effects
  df_varying <- data.frame()
  row_names <- c()

  if(length(effects$m_var)) {
    df_varying <- rbind(df_varying, fit$summary(variables = paste0("scaled_lambda_", names(effects$m_var))))
    row_names <- c(row_names, paste0(names(effects$m_var), " (intercept)"))
  }

  if(length(int_varit) > 0) {
    df_varying <- rbind(df_varying, fit$summary(variables = paste0("lambda_", gsub(':', '', names(int_varit)))))
    row_names <- c(row_names, paste0(names(int_varit), " (intercept)"))
  }

  if(length(int_varsl) > 0) {
    df_varying <- rbind(df_varying, fit$summary(variables = paste0("lambda2_", sapply(names(int_varsl), function(s) strsplit(s, split = ':')[[1]][1]))))
    row_names <- c(row_names, paste0(names(int_varsl), " (slope)"))
  }

  if(nrow(df_varying) > 0) {
    df_varying <- df_varying |>
      select(mean, sd, q5, q95, rhat, ess_bulk, ess_tail) |>
      as.data.frame()

    names(df_varying) <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Convergence", "Bulk_ESS", "Tail_ESS")
    row.names(df_varying) <- row_names
  }

  return(list(df_fixed, df_varying))
}

process_pred <- function(
  brms_new,
  pred_mat,
  by_time
) {

  group_cols <- c("factor")
  out_cols <- c("factor", "est", "std")
  if(by_time) {
    group_cols <- c("time", group_cols)
    out_cols <- c("time", out_cols)
  }

  pstrat <- brms_new |>
    group_by(!!!syms(group_cols)) |>
    mutate(pop_prop = total / sum(total))

  df_out <- (pstrat$pop_prop * pred_mat) |>
    as.data.frame() |>
    mutate(
      factor = brms_new$factor,
      time = if(by_time) brms_new$time else NULL
    ) |>
    group_by(!!!syms(group_cols)) |>
    summarize_all(sum) |>
    ungroup()

  df_out <- df_out |>
    mutate(
      est = df_out |> select(-group_cols) |> apply(1, mean),
      std = df_out |> select(-group_cols) |> apply(1, sd)
    ) |>
    select(all_of(out_cols))

  return(df_out)
}

process_yrep <- function(
  yrep_mat,
  brms_input,
  by_time,
  summarize = FALSE,
  pred_interval = 0.95
) {

  qlower <- (1 - pred_interval) / 2
  qupper <- 1 - qlower

  if(by_time) {
    agg_df <- yrep_mat |>
      as.data.frame() |>
      mutate(
        time = brms_input$time,
        total = brms_input$total
      ) |>
      group_by(time) |>
      summarise_all(sum) |>
      ungroup()

    agg_tests <- agg_df$total
    time <- agg_df$time

    est <- agg_df |>
      select(-c(time, total)) |>
      mutate_all(function(c) c / agg_tests)

    if(summarize) {
      est <- data.frame(
        time = time,
        upper = est |> apply(1, quantile, qlower),
        lower = est |> apply(1, quantile, qupper),
        median = est |> apply(1, quantile, 0.5)
      )
    } else {
      est <- est |> mutate(time = time)
    }

  } else {
    est <- colSums(yrep_mat) / sum(brms_input$total)

    if(summarize) {
      est <- data.frame(
        upper  = quantile(est, qlower),
        lower  = quantile(est, qupper),
        median = quantile(est, 0.5)
      )
    }

  }

  return(est)
}
