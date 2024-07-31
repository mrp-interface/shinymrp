#' model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
check_prior_syntax <- function(s) {
  if(s == "") {
    return(FALSE)
  }

  return(TRUE)
}

create_interactions <- function(effects) {
  df <- expand.grid(eff1 = effects, eff2 = effects)
  df <- df |> filter(eff1 != eff2)
  df <- df[apply(df, 1, function(x) x[1] <= x[2]), ]
  df <- df |> mutate(int = paste0(eff2, ":", eff1))

  return(unique(df$int))
}

interaction_levels <- function(levels1, levels2) {
  numcat1 <- n_distinct(levels1)
  numcat2 <- n_distinct(levels2)

  numcat_interaction <- numcat1 * numcat2
  levels_interaction <- (levels1 - 1) * numcat2 + levels2

  return(list(levels_interaction, numcat_interaction))
}


# Stan code and data generation functions
data_ <- function(effects) {
  s <- "
  int<lower=1> N;
  array[N] int y;
  array[N] int n_sample;
  int<lower=0> K;
  matrix[N, K] X;"

  for(v in names(effects$varying)) {
    s <- paste0(s, stringr::str_interp("
  int<lower=1> N_${v};
  array[N] int<lower=1, upper=N_${v}> J_${v};"))
  }

  for(int in gsub(':', '', names(effects$interaction))) {
    s <- paste0(s, stringr::str_interp("
  int<lower=1> N_${int};
  array[N] int<lower=1, upper=N_${int}> J_${int};"))
  }

  s <- paste0(s, "
  real<lower=0> intercept_prior_mean;
  real<lower=0> intercept_prior_scale;
  real<lower=0> coef_prior_scale;
  real<lower=0> lambda_scale;
  real<lower=0> sens;
  real<lower=0> spec;")

  return(s)
}

transformed_data_ <- function() {
  s <- "
  matrix[N, K] Xc;
  for (i in 1:K) {
    Xc[, i] = X[, i] - mean(X[, i]);
  }"

  # s <- "
  # matrix[N, K] Xc = X;"

  return(s)
}

parameters_ <- function(effects) {
  s <- "
  vector[K] b;
  real Intercept;"

  for(v in names(effects$varying)) {
    s <- paste0(s, stringr::str_interp("
  vector<lower=0.0001>[N_${v}] lambda_${v};
  vector[N_${v}] z_${v};"))
  }

  for(int in gsub(':', '', names(effects$interaction))) {
    s <- paste0(s, stringr::str_interp("
  vector<lower=0.0001>[N_${int}] lambda_${int};
  vector[N_${int}] z_${int};"))
  }

  return(s)
}

transformed_parameters_ <- function(effects) {
  varying <- names(effects$varying)
  interaction <- gsub(':', '', names(effects$interaction))

  s <- ""

  for(v in varying) {
    s <- paste0(s, stringr::str_interp("
  vector[N_${v}] a_${v} = z_${v} .* lambda_${v};"))
  }

  for(int in interaction) {
    s <- paste0(s, stringr::str_interp("
  vector[N_${int}] a_${int} = z_${int} .* lambda_${int};"))
  }

  s <- paste0(s, sprintf("
  vector<lower=0, upper=1>[N] p = inv_logit(Intercept + Xc * b%s%s);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);",
  paste(purrr::map(varying, ~ stringr::str_interp(" + a_${.x}[J_${.x}]")), collapse = ""),
  paste(purrr::map(interaction, ~ stringr::str_interp(" + a_${.x}[J_${.x}]")), collapse = "")))

  return(s)
}

model_ <- function(effects) {
  s <- paste0("
  y ~ binomial(n_sample, p_sample);",
  stringr::str_interp("\n  Intercept ~ ${effects$Intercept};"),
  if(!is.null(effects$fixed)) paste(purrr::map(1:length(effects$fixed), ~ stringr::str_interp("\n  b[${.x}] ~ ${effects$fixed[[.x]]};")), collapse = ""),
  paste(purrr::map(names(effects$varying), ~ stringr::str_interp("\n  z_${.x} ~ std_normal();")), collapse = ""),
  paste(purrr::map(names(effects$interaction), ~ stringr::str_interp("\n  z_${gsub(':', '', .x)} ~ std_normal();")), collapse = ""),
  paste(purrr::map(names(effects$varying), ~ stringr::str_interp("\n  lambda_${.x} ~ ${effects$varying[[.x]]};")), collapse = ""),
  paste(purrr::map(names(effects$interaction), ~ stringr::str_interp("\n  lambda_${gsub(':', '', .x)} ~ ${effects$interaction[[.x]]};")), collapse = "")
  )

  return(s)
}

make_stancode <- function(effects) {

  s <- stringr::str_interp("
data { ${data_(effects)}
}

transformed data { ${transformed_data_()}
}

parameters { ${parameters_(effects)}
}

transformed parameters { ${transformed_parameters_(effects)}
}

model { ${model_(effects)}
}
  ")

  return(s)
}

make_standata <- function(
  dat,
  effects,
  sens = 0.7,
  spec = 0.999,
  hyper_params = list(
    intercept_prior_mean = 0,
    intercept_prior_scale = 5,
    coef_prior_scale = 1,
    lambda_scale = 1,
    delta_scale = 1,
    tau_scale = 1
  )
) {

  sdat <- list(
    N = nrow(dat),
    y = dat$positive,
    n_sample = dat$total,
    K = length(effects$fixed),
    X = dat |> select(all_of(names(effects$fixed))) |> data.matrix(),
    intercept_prior_mean = hyper_params$intercept_prior_mean,
    intercept_prior_scale = hyper_params$intercept_prior_scale,
    coef_prior_scale = hyper_params$coef_prior_scale,
    lambda_scale = hyper_params$lambda_scale,
    delta_scale = hyper_params$delta_scale,
    tau_scale = hyper_params$tau_scale,
    sens = sens,
    spec = spec
  )

  for(v in names(effects$varying)) {
    sdat[[stringr::str_interp("N_${v}")]] <- n_distinct(dat[[v]])
    sdat[[stringr::str_interp("J_${v}")]] <- dat[[v]]
  }

  for(i in names(effects$interaction)) {
    varname <- gsub(':', '', i)
    effects <- strsplit(i, split = ':')[[1]]
    c(levels, numcat) %<-% interaction_levels(dat[[effects[1]]], dat[[effects[2]]])
    sdat[[stringr::str_interp("N_${varname}")]] <- numcat
    sdat[[stringr::str_interp("J_${varname}")]] <- levels
  }

  return(sdat)
}

run_stan <- function(
  dat,
  effects,
  stan_path = "extdata/model.stan"
) {

  scode <- make_stancode(effects)
  sdata <<- make_standata(dat, effects)

  # writeLines(scode, "/Users/tntoan/Downloads/model.stan")
  mod <- cmdstanr::cmdstan_model("/Users/tntoan/Downloads/model.stan", cpp_options = list(stan_threads = TRUE))

  fit <<- mod$sample(
    data = sdata,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4,
    threads_per_chain = 1,
    refresh = 200
  )
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
