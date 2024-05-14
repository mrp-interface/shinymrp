#' model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
log_lik_binomial_sens_spec <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  trials <- prep$data$vint1[i]
  sens <- prep$data$vreal1[i]
  spec <- prep$data$vreal2[i]
  y <- prep$data$Y[i]

  binomial_sens_spec_lpmf(y, mu, trials, sens, spec)
}

posterior_predict_binomial_sens_spec <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  trials <- prep$data$vint1[i]

  binomial_sens_spec_rng(mu, trials)
}

run_brms <- function(
    fstr,
    input_data,
    new_data,
    hyper_param = list(
      intercept_prior_mean = 0,
      intercept_prior_scale = 5,
      coef_prior_scale = 2.5
    ),
    n_iter = 1000,
    n_chains = 4,
    spec = 0.999,
    sens = 0.7,
    max_draw = 10
) {

  formula <- as.formula(fstr)

  # add specificity and sensitivity
  input_data <- input_data |> mutate(spec = spec, sens = sens)
  new_data <- new_data |> mutate(spec = spec, sens = sens)

  binomial_sens_spec_vec <- brms::custom_family(
    "binomial_sens_spec", dpars = c("mu"),
    links = c("logit"), lb = c(NA),
    type="int", vars= c("vint1", "vreal1", "vreal2"), loop = FALSE
  )

  stan_density_vec <- "
    real binomial_sens_spec_lpmf(array[] int y, vector mu, array[] int N, array[] real vreal1, array[] real vreal2) {
      return binomial_lpmf(y | N, mu * vreal1[1] + (1 - mu) * (1 - vreal2[1]));
    }
    array[] int binomial_sens_spec_rng(vector mu, array[] int N) {
      return binomial_rng(N, mu);
    }
  "

  stanvars_vec <- brms::stanvar(scode = stan_density_vec, block = "functions")

  default_prior <- brms::get_prior(formula, input_data, binomial_sens_spec_vec)
  classes <- unique(default_prior$class)
  custom_prior <- brms::set_prior(paste0("normal(", hyper_param$intercept_prior_mean, ",", hyper_param$intercept_prior_scale, ")"), class = "Intercept")
  if("b" %in% classes) {
    custom_prior <- custom_prior + brms::set_prior(paste0("normal(0,", hyper_param$coef_prior_scale, ")"), class = "b")
  }
  if("sd" %in% classes) {
    custom_prior <- custom_prior + brms::set_prior(paste0("normal(0,", hyper_param$coef_prior_scale, ")"), class = "sd")
  }


  fit <- brms::brm(formula = formula,
                   data = input_data,
                   family = binomial_sens_spec_vec,
                   prior = custom_prior,
                   stanvars = stanvars_vec,
                   chains = n_chains,
                   cores = n_chains,
                   iter = n_iter,
                   backend = "rstan")

  brms::expose_functions(fit, vectorize = TRUE)

  pred_mat <- brms::posterior_linpred(
    fit,
    newdata = new_data,
    transform = TRUE,
    allow_new_levels = TRUE,
    ndraws = min(max_draw, round(n_iter/2))
  ) |> t()

  # generate replicated data
  yrep_mat <- brms::posterior_predict(
    fit,
    ndraws = min(max_draw, round(n_iter/2)),
    cores = n_chains
  ) |> t()

  return(list(fit, pred_mat, yrep_mat))
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
