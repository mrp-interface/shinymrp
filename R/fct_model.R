#' model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
create_formula <- function(effects, dat) {
  fixed <- c(names(effects$fixed), names(effects$interaction$fixed_slope))
  varying_intercept <- c(names(effects$varying), names(effects$interaction$varying_intercept))
  varying_slope <- names(effects$interaction$varying_slope)

  s_fixed <- if(length(fixed) > 0) paste(paste0(" + ", fixed), collapse = '') else ''
  s_varying_intercept <- if(length(varying_intercept) > 0) paste(paste0(" + (1 | ", varying_intercept, ")"), collapse = '') else ''
  s_varying_slope <- if(length(varying_slope) > 0) paste(purrr::map(varying_slope, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(paste0(" + (", ss[2], " | ", ss[1], ')'))
  }), collapse = '') else ''


  formula <- paste0("1", s_fixed, s_varying_intercept, s_varying_slope)

  return(formula)
}


check_prior_syntax <- function(s) {
  if(s == "") {
    return(FALSE)
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

create_interactions <- function(effects, dat) {
  if(n_distinct(effects) == 1) {
    return(c())
  }

  # create unique pairs
  df <- expand.grid(eff1 = effects, eff2 = effects)
  df <- df |> filter(eff1 != eff2)
  df <- df[apply(df, 1, function(x) x[1] <= x[2]), ]

  # reverse if cont:cat
  for(i in 1:nrow(df)) {
    if(is.double(dat[[df$eff1[i]]]) & is.integer(dat[[df$eff2[i]]])) {
      temp <- df$eff1[i]
      df$eff1[i] <- df$eff2[i]
      df$eff2[i] <- temp
    }
  }

  int <- paste0(df$eff1, ":", df$eff2)

  return(int)
}

sort_interactions <- function(interactions, dat) {
  categorized <- list()

  for(s in names(interactions)) {
    ss <- strsplit(s, split = ':')[[1]]
    col1 <- dat[[ss[1]]]
    col2 <- dat[[ss[2]]]
    if((is.double(col1) & is.double(col2)) |
       (is.double(col1) & n_distinct(col2) == 2) |
       (is.double(col2) & n_distinct(col1) == 2)) {
      categorized$fixed_slope[[s]] <- interactions[[s]]
    } else if (is.double(col2) & n_distinct(col1) > 2) {
      categorized$varying_slope[[s]] <- interactions[[s]]
    } else {
      categorized$varying_intercept[[s]] <- interactions[[s]]
    }
  }

  return(categorized)
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

  for(s in names(effects$varying)) {
    scode <- paste0(scode, stringr::str_interp("
  int<lower=1> N_${s};
  array[N] int<lower=1, upper=N_${s}> J_${s};
  int<lower=1> N_${s}_pop;
  array[N_pop] int<lower=1, upper=N_${s}_pop> J_${s}_pop;"))
  }

  for(s in gsub(':', '', names(effects$interaction$varying_intercept))) {
    scode <- paste0(scode, stringr::str_interp("
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
  varying <- effects$varying
  int_varslope <- effects$interaction$varying_slope
  int_varint <- effects$interaction$varying_intercept

  scode <- "
  real Intercept;
  vector[K] beta;"

  for(s in names(varying)) {
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${s};
  vector[N_${s}] z_${s};"))
  }

  for(s in names(int_varslope)) {
    ss <- strsplit(s, split = ':')[[1]]

    if(int_varslope[[s]] != "structured") {
      scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda2_${ss[1]};"))
    }

    scode <- paste0(scode, stringr::str_interp("
  vector[N_${ss[1]}] z2_${ss[1]};"))
  }

  for(s in names(int_varint)) {
    v <- gsub(':', '', s)
    if(int_varint[[s]] != "structured") {
      scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${v};"))
    }

    scode <- paste0(scode, stringr::str_interp("
  vector[N_${v}] z_${v};"))
  }

  if("structured" %in% c(int_varint, int_varslope)) {
    scode <- paste0(scode, "
  real<lower=0> tau;
  real<lower=0> delta;")
  }

  return(scode)
}

transformed_parameters_ <- function(effects) {
  fixed <- effects$fixed
  varying <- effects$varying
  int_varslope <- effects$interaction$varying_slope
  int_varint <- effects$interaction$varying_intercept

  scode <- ""

  struct_effects <- c(
    names(int_varslope)[int_varslope == "structured"] |>
      purrr::map(function(s) strsplit(s, ':')[[1]][1]),
    names(int_varint)[int_varint == "structured"] |>
      purrr::map(function(s) strsplit(s, ':')[[1]]) %>%
      do.call(c, .)
  )

  for(s in names(varying)) {
    if(s %in% struct_effects) {
      scode <- paste0(scode, stringr::str_interp("
  real<lower=0> scaled_lambda_${s} = lambda_${s} * tau;"))
    } else {
      scode <- paste0(scode, stringr::str_interp("
  real<lower=0> scaled_lambda_${s} = lambda_${s};"))
    }

    scode <- paste0(scode, stringr::str_interp("
  vector[N_${s}] a_${s} = z_${s} * scaled_lambda_${s};"))
  }

  for(s in names(int_varint)) {
    ss <- strsplit(s, split = ':')[[1]]
    v <- paste0(ss[1], ss[2])
    if(int_varint[[s]] == "structured") {
      if(ss[1] == "sex") {
        scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${v} = lambda_${ss[2]} * delta * tau;"))
      } else if(ss[2] == "sex") {
        scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${v} = lambda_${ss[1]} * delta * tau;"))
      } else {
        scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${v} = lambda_${ss[1]} * lambda_${ss[2]} * delta * tau;"))
      }
    }

    scode <- paste0(scode, stringr::str_interp("
  vector[N_${v}] a_${v} = z_${v} * lambda_${v};"))
  }

  for(s in names(int_varslope)) {
    ss <- strsplit(s, split = ':')[[1]]
    if(int_varslope[[s]] == "structured") {
      scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda2_${ss[1]} = lambda_${ss[1]} * delta * tau;"))
    }

    scode <- paste0(scode, stringr::str_interp("
  vector[N_${ss[1]}] b_${ss[1]} = z2_${ss[1]} * lambda2_${ss[1]};"))
  }

  scode <- paste0(scode, sprintf("
  vector<lower=0, upper=1>[N] p = inv_logit(Intercept + X * beta%s%s%s);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);",
  paste(purrr::map(names(varying), ~ stringr::str_interp(" + a_${.x}[J_${.x}]")), collapse = ""),
  paste(purrr::map(gsub(':', '', names(int_varint)), ~ stringr::str_interp(" + a_${.x}[J_${.x}]")), collapse = ""),
  paste(purrr::map(names(int_varslope), function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(stringr::str_interp(" + b_${ss[1]}[J_${ss[1]}] .* X[:, ${which(names(fixed) == ss[2])}]"))
  }), collapse = "")))

  return(scode)
}

model_ <- function(effects) {
  fixed <- c(effects$fixed, effects$interaction$fixed_slope)
  varying <- effects$varying
  int_varslope <- effects$interaction$varying_slope
  int_varslope_nostruct <- int_varslope[int_varslope != "structured"]
  int_varint <- effects$interaction$varying_intercept
  int_varint_nostruct <- int_varint[int_varint != "structured"]

  scode <- paste0("
  y ~ binomial(n_sample, p_sample);",
  stringr::str_interp("\n  Intercept ~ ${effects$Intercept};"),
  if(!is.null(fixed)) paste(purrr::map(1:length(fixed), ~ stringr::str_interp("\n  beta[${.x}] ~ ${fixed[[.x]]};")), collapse = ""),
  paste(purrr::map(names(varying), ~ stringr::str_interp("\n  z_${.x} ~ std_normal();")), collapse = ""),
  paste(purrr::map(names(int_varint), ~ stringr::str_interp("\n  z_${gsub(':', '', .x)} ~ std_normal();")), collapse = ""),
  paste(purrr::map(names(int_varslope), ~ stringr::str_interp("\n  z2_${strsplit(.x, split = ':')[[1]][1]} ~ std_normal();")), collapse = ""),
  paste(purrr::map(names(varying), ~ stringr::str_interp("\n  lambda_${.x} ~ ${varying[[.x]]};")), collapse = ""),
  paste(purrr::map(names(int_varint_nostruct), ~ stringr::str_interp("\n  lambda_${gsub(':', '', .x)} ~ ${int_varint[[.x]]};")), collapse = ""),
  paste(purrr::map(names(int_varslope_nostruct), ~ stringr::str_interp("\n  lambda2_${strsplit(.x, split = ':')[[1]][1]} ~ ${int_varslope[[.x]]};")), collapse = "")
  )

  if("structured" %in% c(int_varslope, int_varint)) {
    scode <- paste0(scode, "
  tau ~ cauchy(0 , 1);
  delta ~ normal(0, 1);")
  }

  return(scode)
}

generated_quantities_ <- function(effects) {
  fixed <- effects$fixed
  varying <- effects$varying
  int_varslope <- effects$interaction$varying_slope
  int_varint <- effects$interaction$varying_intercept


  scode <- ""

  for(s in gsub(':', '', names(int_varint))) {
    scode <- paste0(scode, stringr::str_interp("
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
  paste(purrr::map(names(varying), ~ stringr::str_interp(" + a_${.x}[J_${.x}_pop]")), collapse = ""),
  paste(purrr::map(gsub(':', '', names(int_varint)), ~ stringr::str_interp(" + a_${.x}_pop[J_${.x}_pop]")), collapse = ""),
  paste(purrr::map(names(int_varslope), function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(stringr::str_interp(" + b_${ss[1]}[J_${ss[1]}_pop] .* X_pop[:, ${which(names(fixed) == ss[2])}]"))
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

  scode <- stringr::str_interp("
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
  sens = 0.7,
  spec = 0.999
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

    # fixed effects & interaction between fixed effects
    X <- dat |> select(all_of(names(effects$fixed)))
    for(s in names(effects$interaction$fixed_slope)) {
      ss <- strsplit(s, split = ':')[[1]]
      X[[s]] <- dat[[ss[1]]] * dat[[ss[2]]]
    }

    stan_data[[paste0('X', subfix)]] <- data.matrix(X)
    stan_data[[paste0('K', subfix)]] <- ncol(X)

    # varying effects
    for(s in names(effects$varying)) {
      stan_data[[stringr::str_interp("N_${s}${subfix}")]] <- n_distinct(dat[[s]])
      stan_data[[stringr::str_interp("J_${s}${subfix}")]] <- dat[[s]]
    }

    # interactions
    for(s in names(effects$interaction$varying_intercept)) {
      ss <- strsplit(s, split = ':')[[1]]
      v <- paste0(ss[1], ss[2])
      int_lvls <- interaction_levels(dat[[ss[1]]], dat[[ss[2]]])
      unq_int_lvls <- sort(unique(int_lvls))
      n_int_lvls <- length(unq_int_lvls)
      stan_data[[stringr::str_interp("N_${v}${subfix}")]] <- n_int_lvls
      stan_data[[stringr::str_interp("J_${v}${subfix}")]] <- factor(int_lvls, levels = unq_int_lvls, labels = 1:n_int_lvls) |> as.numeric()

      if(name == "input") {
        stan_data[[stringr::str_interp("I_${v}")]] <- unq_int_lvls
      }
    }
  }

  return(stan_data)
}

run_stan <- function(
  input_data,
  new_data,
  effects,
  n_iter,
  n_chains,
  sens,
  spec,
  stan_path = "model_temp.stan"
) {

  stan_code <- make_stancode(effects)
  stan_data <- make_standata(input_data, new_data, effects, sens, spec)

  writeLines(stan_code, stan_path)
  mod <- cmdstanr::cmdstan_model(stan_path, cpp_options = list(stan_threads = TRUE))

  fit <- mod$sample(
    data = stan_data,
    iter_warmup = n_iter/2,
    iter_sampling = n_iter/2,
    chains = n_chains,
    parallel_chains = n_chains,
    threads_per_chain = 1,
    refresh = n_iter / 10
  )

  pred_mat <- fit$draws(
    variables = "p_pop",
    format = "draws_matrix"
  )|> t()

  # generate replicated data
  yrep_mat <- fit$draws(
    variables = "y_rep",
    format = "draws_matrix"
  )|> t()

  yrep_mat <- yrep_mat[, sample(ncol(yrep_mat), 10)]


  return(list(fit, pred_mat, yrep_mat, stan_code))
}

extract_parameters <- function(fit, effects) {
  main_fixed <- effects$fixed
  main_varying <- effects$varying
  int_fixed <- effects$interaction$fixed_slope
  int_varslope <- effects$interaction$varying_slope
  int_varint <- effects$interaction$varying_intercept

  # data for fixed-effect table
  df_fixed <- data.frame()
  if(length(main_fixed) > 0) {
    df_fixed <- fit$summary(variables = c("Intercept", "beta")) |>
      select(mean, sd, q5, q95, rhat, ess_bulk, ess_tail) |>
      as.data.frame()

    names(df_fixed) <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", "Convergence", "Bulk_ESS", "Tail_ESS")
    row.names(df_fixed) <- c("Intercept", names(main_fixed), names(int_fixed))
  }

  # data for varying-effect table
  df_varying <- data.frame()
  row_names <- c()

  if(length(main_varying) > 0) {
    df_varying <- rbind(df_varying, fit$summary(variables = paste0("scaled_lambda_", names(main_varying))))
    row_names <- c(row_names, paste0(names(main_varying), " (intercept)"))
  }

  if(length(int_varint) > 0) {
    df_varying <- rbind(df_varying, fit$summary(variables = paste0("lambda_", gsub(':', '', names(int_varint)))))
    row_names <- c(row_names, paste0(names(int_varint), " (intercept)"))
  }

  if(length(int_varslope) > 0) {
    df_varying <- rbind(df_varying, fit$summary(variables = paste0("lambda2_", unlist(purrr::map(names(int_varslope), function(s) strsplit(s, split = ':')[[1]][1])))))
    row_names <- c(row_names, paste0(names(int_varslope), " (slope)"))
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
