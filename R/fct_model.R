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
create_formula <- function(effects) {
  m_fix_c <- names(effects$m_fix_c) |>
    purrr::map_chr(function(s) strsplit(s, "\\.")[[1]][1]) |>
    unique()

  fixed <- c(names(effects$m_fix_bc), m_fix_c, names(effects$i_fixsl))
  varsl <- c(names(effects$i_varsl), names(effects$s_varsl))
  varit <- c(names(effects$m_var), names(effects$i_varit), names(effects$i_varits), names(effects$s_varit), names(effects$s_varits))
  

  s_fixed <- if(length(fixed) > 0) paste(paste0(" + ", fixed), collapse = '') else ''
  s_varit <- if(length(varit) > 0) paste(paste0(" + (1 | ", varit, ")"), collapse = '') else ''
  s_varsl <- if(length(varsl) > 0) paste(map(varsl, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(paste0(" + (0 + ", ss[2], " | ", ss[1], ')'))
  }), collapse = '') else ''


  formula <- paste0("1", s_fixed, s_varit, s_varsl)

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

pair_setdiff <- function(pairs1, pairs2, sep = ":") {
  # helper to normalize a single "a:b" → "a:b" or "b:a" → "a:b"
  norm_pair <- function(p) {
    parts <- strsplit(p, sep, fixed = TRUE)[[1]]
    paste(sort(parts), collapse = sep)
  }

  # precompute the normalized set of pairs2
  norm2 <- vapply(pairs2, norm_pair, FUN.VALUE = character(1))

  # keep those in pairs1 whose normalized form is NOT in norm2
  keep <- !vapply(pairs1, norm_pair, FUN.VALUE = character(1)) %in% norm2

  return(pairs1[keep])
}



# filter interactions for structured prior
filter_interactions <- function(interactions, fixed_cat_dummies, dat) {
  fixed_cat <- purrr::map_chr(
    fixed_cat_dummies,
    function(s) strsplit(s, split = "\\.")[[1]][1]
  ) |>
    unique()
  bool <- map_lgl(interactions, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- data_type(dat[[ss[1]]])
    type2 <- data_type(dat[[ss[2]]])
    
    return((type1 == "cat" | type2 == "cat") &&
           length(intersect(ss, fixed_cat)) == 0)
  })
  
  return(interactions[bool])
}

# Keep interaction in expected order for Stan code generation:
# binary first, then categorical, then continuous.
sort_interactions <- function(interactions, dat) {
  interactions <- map_chr(interactions, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- data_type(dat[[ss[1]]], num = TRUE)
    type2 <- data_type(dat[[ss[2]]], num = TRUE)
    
    if(type1 > type2) {
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
      levels <- sort(unique(dat[[s]]))
      dummies <- paste0(s, ".", levels[2:length(levels)])   # first level is the reference level
      for(d in dummies) {
        out$cat[[d]] <- fixed[[s]]
      }
    } else {
      out$bincont[[s]] <- fixed[[s]]
    }
  }
  
  return(out)
}

group_interactions <- function(interactions, dat) {
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
    if((type1 == "cont" && type2 == "cont") |
       (type1 == "bin" && type2 == "bin") |
       (type1 == "cont" && type2 == "bin") |
       (type1 == "bin" && type2 == "cont")) {
      out$fixed_slope[[s]] <- interactions[[s]]
      
      # categorical x continuous
    } else if (type1 == "cat" && type2 == "cont") {
      out$varying_slope[[s]] <- interactions[[s]]
      
      # binary x categorical
    } else if (type1 == "bin" && type2 == "cat") {
      out$varying_intercept_special[[s]] <- interactions[[s]]
      
      # categorical x categorical
    } else if (type1 == "cat" && type2 == "cat") {
      out$varying_intercept[[s]] <- interactions[[s]]

    } else {
      stop("Unrecognized interaction")
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
    names(effects$interaction) <- sort_interactions(names(effects$interaction), dat) 
  }
  
  # interactions without structured priors
  wo_struct <- effects$interaction[effects$interaction != "structured"]
  out$interaction <- group_interactions(wo_struct, dat)
  
  # interactions with structured priors
  w_struct <- effects$interaction[effects$interaction == "structured"]
  out$structured <- group_interactions(w_struct, dat)
  
  
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
data_ <- function(effects, gq_data) {
  scode <- "
  int<lower=1> N;
  array[N] int y;
  array[N] int n_sample;
  int<lower=0> K;
  matrix[N, K] X;
  int<lower=1> N_pop;
  int<lower=0> K_pop;
  matrix[N_pop, K_pop] X_pop;
  "
  
  # varying effects & fixed effects of categorical variables
  m_fix_c_names <- purrr::map_chr(
    names(effects$m_fix_c),
    function(s) strsplit(s, split = "\\.")[[1]][1]
  ) |> 
    unique()
  for(s in union(m_fix_c_names, names(effects$m_var))) {
    scode <- paste0(scode, str_interp("
  int<lower=1> N_${s};
  array[N] int<lower=1, upper=N_${s}> J_${s};
  int<lower=1> N_${s}_pop;
  array[N_pop] int<lower=1, upper=N_${s}_pop> J_${s}_pop;
  "))
  }
  
  # interactions
  int <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  for(s in gsub(':', '', names(int))) {
    scode <- paste0(scode, str_interp("
  int<lower=1> N_${s};
  array[N] int<lower=1, upper=N_${s}> J_${s};
  int<lower=1> N_${s}_pop;
  array[N_pop] int<lower=1, upper=N_${s}_pop> J_${s}_pop;
  array[N_${s}] int<lower=1, upper=N_${s}_pop> I_${s};
  "))
  }
  
  # for poststratification
  scode <- paste0(scode, "
  vector<lower=0, upper=1>[N_pop] P_overall_pstrat;
  ")
  
  for(s in gq_data$subgroups) {
    scode <- paste0(scode, str_interp("
  int<lower=1> N_${s}_pstrat;
  array[N_pop] int<lower=1, upper=N_${s}_pstrat> J_${s}_pstrat;
  vector<lower=0, upper=1>[N_pop] P_${s}_pstrat;
  ")) 
  }
  
  if(gq_data$temporal) {
    scode <- paste0(scode, "
  int<lower=1> N_time_pstrat;
  array[N_pop] int<lower=1, upper=N_time_pstrat> J_time_pstrat;
  ")
  }
  
  # sensitivity and specificity
  scode <- paste0(scode, "
  real<lower=0> sens;
  real<lower=0> spec;")
  
  return(scode)
}

parameters_ <- function(effects) {
  scode <- "
  real Intercept;"
  
  if(length(c(effects$m_fix_bc, effects$m_fix_c)) > 0) {
    scode <- paste0(scode, "\n  vector[K] beta;")
  }
  
  # varying main effect
  for(s in names(effects$m_var)) {
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${s};
  vector[N_${s}] z_${s};"))
  }
  
  # varying-intercept interaction without structured prior
  for(s in names(c(effects$i_varit, effects$i_varits))) {
    s <- gsub(':', '', s)
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${s};
  vector[N_${s}] z_${s};"))
  }
  
  # varying-slope interaction without structured prior
  for(s in names(effects$i_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda2_${s};
  vector[N_${ss[1]}] z2_${s};"))
  }
  
  # varying-intercept interaction with structured prior
  for(s in names(c(effects$s_varit, effects$s_varits))) {
    s <- gsub(':', '', s)
    scode <- paste0(scode, str_interp("
  vector[N_${s}] z_${s};"))
  }
  
  # varying-slope interaction with structured prior
  for(s in names(effects$s_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  vector[N_${ss[1]}] z2_${s};"))
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
  ) |>
    unlist() |>
    unique()
  
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
    s <- gsub(':', '', s)
    scode <- paste0(scode, str_interp("
  vector[N_${s}] a_${s} = z_${s} * lambda_${s};"))
  }
  
  # varying-slope interaction without structured prior
  for(s in names(effects$i_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  vector[N_${ss[1]}] b_${s} = z2_${s} * lambda2_${s};"))
  }
  
  # varying-intercept interaction with structured prior
  for(s in names(c(effects$s_varit))) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${s} = lambda_${ss[1]} * lambda_${ss[2]} * delta * tau;
  vector[N_${s}] a_${s} = z_${s} * lambda_${s};"))
  }
  
  # varying-intercept interaction with structured prior (with binary variable)
  for(s in names(c(effects$s_varits))) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda_${s} = lambda_${ss[2]} * delta * tau;
  vector[N_${s}] a_${s} = z_${s} * lambda_${s};"))
  }
  
  # varying-slope interaction with structured prior
  for(s in names(effects$s_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, str_interp("
  real<lower=0> lambda2_${s} = lambda_${ss[1]} * delta * tau;
  vector[N_${ss[1]}] b_${s} = z2_${s} * lambda2_${s};"))
  }
  
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)
  
  scode <- paste0(scode, sprintf("
  vector<lower=0, upper=1>[N] p = inv_logit(Intercept%s%s%s%s);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);",
                                if(length(fixed) > 0) " + X * beta" else "",
                                paste(map(names(effects$m_var), ~ str_interp(" + a_${.x}[J_${.x}]")), collapse = ""),
                                paste(map(gsub(':', '', names(int_varit)), ~ str_interp(" + a_${.x}[J_${.x}]")), collapse = ""),
                                paste(map(names(int_varsl), function(s) {
                                  ss <- strsplit(s, split = ':')[[1]]
                                  s <- paste0(ss[1], ss[2])
                                  return(str_interp(" + b_${s}[J_${ss[1]}] .* X[:, ${which(names(effects$m_fix_bc) == ss[2])}]"))
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
    paste(map(names(int_varsl), ~ str_interp("\n  z2_${gsub(':', '', .x)} ~ std_normal();")), collapse = ""),
    paste(map(names(effects$m_var), ~ str_interp("\n  lambda_${.x} ~ ${effects$m_var[[.x]]};")), collapse = ""),
    paste(map(names(int_varit_wo_struct), ~ str_interp("\n  lambda_${gsub(':', '', .x)} ~ ${int_varit[[.x]]};")), collapse = ""),
    paste(map(names(int_varsl_wo_struct), ~ str_interp("\n  lambda2_${gsub(':', '', .x)} ~ ${int_varsl[[.x]]};")), collapse = "")
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


gq_loo <- function() {
  return("
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = binomial_lpmf(y[n] | n_sample[n], p_sample[n]);
  }")
}

gq_ppc <- function() {
  return("
  array[N] int<lower = 0> y_rep = binomial_rng(n_sample, p_sample);")
}

gq_pstrat <- function(effects, gq_data) {
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)
  scode <- ""
  
  # sample from posterior for parameters of new levels
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
  
  # poststratification
  if(gq_data$temporal) {
    init_overall <- "vector<lower=0, upper=1>[N_time_pstrat] p_overall_pop = rep_vector(0, N_time_pstrat);"
    init_marginal <- paste(map(gq_data$subgroups, ~ str_interp("
  matrix<lower=0, upper=1>[N_${.x}_pstrat, N_time_pstrat] p_${.x}_pop = rep_matrix(0, N_${.x}_pstrat, N_time_pstrat);")), collapse = "")
  } else {
    init_overall <- "real<lower=0, upper=1> p_overall_pop;"
    init_marginal <- paste(map(gq_data$subgroups, ~ str_interp("
  vector<lower=0, upper=1>[N_${.x}_pstrat] p_${.x}_pop = rep_vector(0, N_${.x}_pstrat);")), collapse = "")
  }
  
  p_pop <- sprintf("
    vector[N_pop] p_pop;
    vector[N_pop] p_pop_scaled;
    p_pop = inv_logit(Intercept%s%s%s%s);",
                  if(length(fixed) > 0) " + X_pop * beta" else "",
                  paste(map(names(effects$m_var), ~ str_interp(" + a_${.x}[J_${.x}_pop]")), collapse = ""),
                  paste(map(gsub(':', '', names(int_varit)), ~ str_interp(" + a_${.x}_pop[J_${.x}_pop]")), collapse = ""),
                  paste(map(names(int_varsl), function(s) {
                    ss <- strsplit(s, split = ':')[[1]]
                    s <- paste0(ss[1], ss[2])
                    return(str_interp(" + b_${s}[J_${ss[1]}_pop] .* X_pop[:, ${which(names(effects$m_fix_bc) == ss[2])}]"))
                  }), collapse = "")
  )
  
  if(gq_data$temporal) {
    est_overall <- "
    p_pop_scaled = p_pop .* P_overall_pstrat;
    for (i in 1:N_pop) {
      p_overall_pop[J_time_pstrat[i]] += p_pop_scaled[i];
    }"
    
    est_marginal <- paste(map(gq_data$subgroups, ~ str_interp("
    p_pop_scaled = p_pop .* P_${.x}_pstrat;
    for (i in 1:N_pop) {
      p_${.x}_pop[J_${.x}_pstrat[i], J_time_pstrat[i]] += p_pop_scaled[i];
    }")), collapse = "")
  } else {
    est_overall <- str_interp("
    p_pop_scaled = p_pop .* P_overall_pstrat;
    p_overall_pop = sum(p_pop_scaled);")
    
    est_marginal <- paste(map(gq_data$subgroups, ~ str_interp("
    p_pop_scaled = p_pop .* P_${.x}_pstrat;
    for (i in 1:N_pop) {
      p_${.x}_pop[J_${.x}_pstrat[i]] += p_pop_scaled[i];
    }")), collapse = "") 
  }
  
  scode <- paste0(scode, str_interp("
  ${init_overall}
  ${init_marginal}
  {  ${p_pop}
     ${est_overall}
     ${est_marginal}
  }                
  "))

  
  return(scode)
}


make_stancode_mcmc <- function(effects, gq_data) {
  
  scode <- str_interp("
data { ${data_(effects, gq_data)}
}

parameters { ${parameters_(effects)}
}

transformed parameters { ${transformed_parameters_(effects)}
}

model { ${model_(effects)}
}
  ")
  
  return(scode)
}

make_stancode_gq <- function(effects, gq_data, gq_type = c("loo", "ppc", "pstrat")) {
  gq_type <- match.arg(gq_type)
  if(gq_type == "loo") {
    gq <- gq_loo()
  } else if (gq_type == "ppc") {
    gq <- gq_ppc()
  } else if (gq_type == "pstrat")
    gq <- gq_pstrat(effects, gq_data)
  
  scode <- str_interp("
data { ${data_(effects, gq_data)}
}

parameters { ${parameters_(effects)}
}

transformed parameters { ${transformed_parameters_(effects)}
}

generated quantities { ${gq}
}
  ")
  
  return(scode)
}

make_standata <- function(
    input_data,
    new_data,
    effects,
    gq_data,
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

    # fixed main effects (continuous && binary)
    X_cont <- dat |>
      select(all_of(names(effects$m_fix_bc))) |>
      data.matrix()

    # fixed main effects (categorical)
    X_cat <- map(names(effects$m_fix_c), function(s) {
      ss <- strsplit(s, split = '\\.')[[1]]
      as.integer(dat[[paste0(ss[1], "_raw")]] == ss[2])
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
    
    # varying effects & fixed effects of categorical variables
    m_fix_c_names <- purrr::map_chr(
      names(effects$m_fix_c),
      function(s) strsplit(s, split = "\\.")[[1]][1]
    ) |> 
      unique()
    for(s in union(m_fix_c_names, names(effects$m_var))) {
      stan_data[[str_interp("N_${s}${subfix}")]] <- n_distinct(dat[[s]])
      stan_data[[str_interp("J_${s}${subfix}")]] <- dat[[s]]
    }
    
    # interactions
    int <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
    for(s in names(int)) {
      ss <- strsplit(s, split = ':')[[1]]
      s <- paste0(ss[1], ss[2])
      int_lvls <- interaction_levels(dat[[ss[1]]], dat[[ss[2]]])
      unq_int_lvls <- sort(unique(int_lvls))
      n_int_lvls <- length(unq_int_lvls)
      stan_data[[str_interp("N_${s}${subfix}")]] <- n_int_lvls
      stan_data[[str_interp("J_${s}${subfix}")]] <- factor(int_lvls, levels = unq_int_lvls, labels = 1:n_int_lvls) |> as.numeric()
      
      if(name == "input") {
        stan_data[[str_interp("I_${s}")]] <- unq_int_lvls
      }
    }
  }

  # poststratification
  pstrat_data <- new_data |> 
    mutate(
      sex = sex + 1,
      overall = 1
    )
  for(s in c("overall", gq_data$subgroups)) {
    group_cols <- if(gq_data$temporal) c("time", s) else c(s)
    
    pop_prop <- pstrat_data |>
      group_by(!!!syms(group_cols)) |>
      mutate(prop = total / sum(total))
    
    if(s != "overall") {
      stan_data[[str_interp("N_${s}_pstrat")]] <- n_distinct(pstrat_data[[s]])
      stan_data[[str_interp("J_${s}_pstrat")]] <- pstrat_data[[s]] 
    }
    stan_data[[str_interp("P_${s}_pstrat")]] <- pop_prop$prop
  }
  
  if(gq_data$temporal) {
    stan_data$N_time_pstrat <- n_distinct(new_data$time)
    stan_data$J_time_pstrat <- new_data$time
  }

  return(stan_data)
}

run_mcmc <- function(
    input_data,
    new_data,
    effects,
    gq_data,
    n_iter = 1000,
    n_chains = 4,
    seed = NULL,
    sens = 1,
    spec = 1,
    code_fout = NULL
) {

  stan_code <- list()
  stan_code$mcmc <- make_stancode_mcmc(effects, gq_data)
  stan_code$ppc <- make_stancode_gq(effects, gq_data, "ppc")
  stan_code$loo <- make_stancode_gq(effects, gq_data, "loo")
  stan_code$pstrat <- make_stancode_gq(effects, gq_data, "pstrat")
  
  stan_data <- make_standata(input_data, new_data, effects, gq_data, sens, spec)
  
  if(!is.null(code_fout)) {
    writeLines(stan_code$mcmc, code_fout)
  }

  mod_mcmc <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stan_code$mcmc),
    cpp_options = list(stan_threads = TRUE)
  )

  fit <- list()
  fit$mcmc <- mod_mcmc$sample(
    data = stan_data,
    iter_warmup = n_iter/2,
    iter_sampling = n_iter/2,
    chains = n_chains,
    parallel_chains = n_chains,
    threads_per_chain = 1,
    refresh = n_iter / 10,
    diagnostics = NULL,
    seed = seed
  )
  
  return(list(fit, stan_data, stan_code))
}

run_gq <- function(
    fit_mcmc,
    stan_code,
    stan_data,
    n_chains
  ) {
  
  suppressMessages({
    mod_gq <- cmdstanr::cmdstan_model(
      stan_file = cmdstanr::write_stan_file(stan_code),
      cpp_options = list(stan_threads = TRUE)
    )
  })
  
  capture.output({
    fit_gq <- mod_gq$generate_quantities(
      fit_mcmc,
      data = stan_data,
      parallel_chains = n_chains,
      threads_per_chain = 1
    )
  })
  
  return(fit_gq)
}

add_ref_lvl <- function(df_fixed, effects, input_data) {
  ### include reference levels for binary variables
  m_fix_bc_names <- names(effects$m_fix_bc) |>
    purrr::map_chr(function(s) {
      if (data_type(input_data[[s]]) == "bin") {
        df <- data.frame(x = input_data[[s]]) |> stan_factor()
        eq1 <- unique(df$x_raw[df$x == 1])
        paste0(s, ".", eq1)
      } else {
        s
      }
    })

  row.names(df_fixed) <- c("Intercept", c(m_fix_bc_names, names(effects$m_fix_c), names(effects$i_fixsl)))

  ### include reference levels for categorical variables
  # get variable names
  m_fix_c_vars <- names(effects$m_fix_c) |>
    purrr::map_chr(function(s) strsplit(s, split = '\\.')[[1]][1]) |>
    unique()

  # dummy variables including reference levels
  m_fix_c_names <- m_fix_c_vars |>
    purrr::map(function(s) {
      levels <- sort(unique(input_data[[s]]))
      paste0(s, ".", levels)
    }) |>
    unlist(use.names = FALSE)

  row_names <- c("Intercept", m_fix_bc_names, m_fix_c_names, names(effects$i_fixsl))
  idx <- match(row_names, row.names(df_fixed))
  df_fixed <- df_fixed[idx, ] # NA indices are given rows with NA values
  row.names(df_fixed) <- row_names

  return(df_fixed)
}

extract_parameters <- function(fit, effects, input_data) {
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)

  ### fixed main effects & fixed-slope interaction
  df_fixed <- data.frame()
  if(length(fixed) > 0) {
    # extract summary table
    df_fixed <- fit$summary(
      variables = c("Intercept", "beta"),
      posterior::default_summary_measures()[1:4],
      quantiles = ~ posterior::quantile2(., probs = c(0.025, 0.975)),
      posterior::default_convergence_measures()
    ) |>
      select(mean, sd, `q2.5`, `q97.5`, rhat, ess_bulk, ess_tail) |>
      as.data.frame()

    # rename columns and rows
    names(df_fixed) <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", "R-hat", "Bulk_ESS", "Tail_ESS")
    df_fixed <- add_ref_lvl(df_fixed, effects, input_data)
  }

  ### varying effects
  df_varying <- data.frame()
  row_names <- c()

  if(length(effects$m_var)) {
    df_varying <- rbind(
      df_varying,
      fit$summary(
        variables = paste0("scaled_lambda_", names(effects$m_var)),
        posterior::default_summary_measures()[1:4],
        quantiles = ~ posterior::quantile2(., probs = c(0.025, 0.975)),
        posterior::default_convergence_measures()
      )
    )
    row_names <- c(row_names, paste0(names(effects$m_var), " (intercept)"))
  }

  if(length(int_varit) > 0) {
    df_varying <- rbind(
      df_varying, 
      fit$summary(
        variables = paste0("lambda_", gsub(':', '', names(int_varit))),
        posterior::default_summary_measures()[1:4],
        quantiles = ~ posterior::quantile2(., probs = c(0.025, 0.975)),
        posterior::default_convergence_measures()
      )
    )
    row_names <- c(row_names, paste0(names(int_varit), " (intercept)"))
  }

  if(length(int_varsl) > 0) {
    df_varying <- rbind(
      df_varying, 
      fit$summary(
        variables = paste0("lambda2_", purrr::map_chr(names(int_varsl), function(s) gsub(':', '', s))),
        posterior::default_summary_measures()[1:4],
        quantiles = ~ posterior::quantile2(., probs = c(0.025, 0.975)),
        posterior::default_convergence_measures()
      )
    )
    row_names <- c(row_names, paste0(names(int_varsl), " (slope)"))
  }

  if(nrow(df_varying) > 0) {
    df_varying <- df_varying |>
      select(mean, sd, `q2.5`, `q97.5`, rhat, ess_bulk, ess_tail) |>
      as.data.frame()

    names(df_varying) <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", "R-hat", "Bulk_ESS", "Tail_ESS")
    row.names(df_varying) <- row_names
  }

  return(list(df_fixed, df_varying))
}

extract_diagnostics <- function(fit, total_transitions, max_depth = 10) {
  # Get diagnostic summary
  diag_summary <- fit$diagnostic_summary(quiet = TRUE)
  
  # Calculate metrics
  total_divergent <- sum(diag_summary$num_divergent)
  total_max_treedepth <- sum(diag_summary$num_max_treedepth)
  low_ebfmi_count <- sum(diag_summary$ebfmi < 0.3)
  total_chains <- length(diag_summary$ebfmi)
  
  # Calculate percentages
  divergent_percent <- round(total_divergent / total_transitions * 100, 1)
  
  # Create formatted messages
  divergent_msg <- sprintf("%d of %d (%.1f%%) transitions ended with a divergence", 
                           total_divergent, total_transitions, divergent_percent)
  
  treedepth_msg <- sprintf("%d of %d transitions hit the maximum tree depth limit of %d", 
                          total_max_treedepth, total_transitions, max_depth)
  
  ebfmi_msg <- sprintf("%d of %d chains had an E-BFMI less than 0.3", 
                       low_ebfmi_count, total_chains)
  
  # Create and return data frame
  result <- data.frame(
    Metric = c("Divergence", "Maximum tree depth", "E-BFMI"),
    Message = c(divergent_msg, treedepth_msg, ebfmi_msg),
    stringsAsFactors = FALSE
  )

  show_warnings <- total_divergent > 0
  
  return(list(result, show_warnings))
}

extract_est <- function(
  fit,
  new_data,
  gq_data
) {

  new_data <- new_data |> mutate(overall = 1)
  est <- list()

  for(s in c("overall", gq_data$subgroups)) {
    pred_mat <- fit$draws(
      variables = str_interp("p_${s}_pop"),
      format = "draws_matrix"
    ) |> t()
    
    cols <- if(gq_data$temporal) c("time", s) else c(s)
    
    est[[s]] <- new_data |>
      distinct(!!!syms(cols)) |>
      mutate(
        est = pred_mat |> apply(1, mean),
        std = pred_mat |> apply(1, sd)
      ) |>
      rename("factor" := !!s)
  }

  return(est)
}

extract_yrep <- function(
  fit,
  input_data,
  gq_data,
  N = 10,
  summarize = FALSE,
  pred_interval = 0.95
) {

  # get draws from cmdstanr fit
  yrep_mat <- fit$draws(
    variables = "y_rep",
    format = "draws_matrix"
  )|> t()
  
  yrep_mat <- yrep_mat[, sample(ncol(yrep_mat), N)]   # subset draws
  
  qlower <- (1 - pred_interval) / 2
  qupper <- 1 - qlower

  if(gq_data$temporal) {
    agg_df <- yrep_mat |>
      as.data.frame() |>
      mutate(
        time = input_data$time,
        total = input_data$total
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
    est <- colSums(yrep_mat) / sum(input_data$total)

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
