#' Create Model Formula from Effects Structure
#'
#' @description Creates a formula string for multilevel regression models based on
#' the effects structure. Combines fixed effects, varying intercepts, and varying
#' slopes into a single formula suitable for statistical modeling frameworks.
#'
#' @param effects A list containing model effects structure with components:
#'   \itemize{
#'     \item m_fix_bc: Binary/continuous fixed main effects
#'     \item m_fix_c: Categorical fixed main effects
#'     \item i_fixsl: Fixed-slope interactions
#'     \item i_varsl: Varying-slope interactions
#'     \item s_varsl: Structured varying-slope interactions
#'     \item m_var: Varying main effects
#'     \item i_varit: Varying-intercept interactions
#'     \item i_varits: Special varying-intercept interactions
#'     \item s_varit: Structured varying-intercept interactions
#'     \item s_varits: Special structured varying-intercept interactions
#'   }
#'
#' @return A character string representing the model formula with fixed effects,
#'   varying intercepts (1 | group), and varying slopes (0 + variable | group)
#'
#' @noRd
#'
#' @importFrom stringr str_interp
create_formula <- function(effects) {
  m_fix_c <- names(effects$m_fix_c) %>%
    purrr::map_chr(function(s) strsplit(s, "\\.")[[1]][1]) %>%
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

#' Clean Prior Distribution Syntax
#'
#' @description Standardizes prior distribution syntax by removing whitespace
#' and converting to lowercase for consistent parsing and validation.
#'
#' @param s A character string representing a prior distribution specification
#'
#' @return A cleaned character string with whitespace removed and converted to lowercase
#'
#' @noRd
clean_prior_syntax <- function(s) {
  # Remove whitespace
  s <- gsub("\\s+", "", s)

  # Convert to lowercase
  s <- tolower(s)

  return(s)
}

#' Validate Prior Distribution Syntax
#'
#' @description Validates that a prior distribution string follows the expected
#' syntax patterns for normal, student_t, or structured priors. Returns TRUE
#' for valid syntax or NULL inputs.
#'
#' @param s A character string representing a prior distribution specification.
#'   Expected formats:
#'   \itemize{
#'     \item normal(mean, sd): e.g., "normal(0,1)"
#'     \item student_t(df, location, scale): e.g., "student_t(3,0,1)"
#'     \item structured: "structured"
#'   }
#'
#' @return Logical value indicating whether the syntax is valid (TRUE) or invalid (FALSE)
#'
#' @noRd
check_prior_syntax <- function(s) {
  if(is.null(nullify(s))) {
    return(TRUE)
  }

  patterns <- list(
    normal = "^normal\\([0-9]+,[1-9][0-9]*\\)$",
    student_t = "^student_t\\([1-9][0-9]*,[0-9]+,[1-9][0-9]*)$",
    structured = "^structured$"
  )
  
  dist <- strsplit(s, '\\(')[[1]][1]
  
  if(dist %in% names(patterns)) {
    return(grepl(patterns[[dist]], s))
  } else {
    return(FALSE)
  }
}

#' Set Difference for Interaction Pairs
#'
#' @description Computes set difference between two sets of interaction pairs,
#' treating "a:b" and "b:a" as equivalent by normalizing pair order before comparison.
#' Useful for filtering interaction terms in model specifications.
#'
#' @param pairs1 Character vector of interaction pairs (e.g., c("age:gender", "income:education"))
#' @param pairs2 Character vector of interaction pairs to exclude
#' @param sep Character separator used in interaction pairs (default ":")
#'
#' @return Character vector containing pairs from pairs1 that are not present in pairs2,
#'   accounting for order-invariant matching
#'
#' @noRd
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
#' Filter Interactions for Structured Priors
#'
#' @description Filters interaction terms to identify those suitable for structured
#' priors. An interaction is kept if at least one variable is categorical and not
#' included as a fixed effect, enabling hierarchical modeling of the interaction.
#'
#' @param interactions Character vector of interaction terms in "var1:var2" format
#' @param fixed_effects Character vector of variable names included as fixed effects
#' @param dat Data frame containing the variables referenced in interactions
#'
#' @return Character vector of filtered interaction terms suitable for structured priors
#'
#' @noRd
#'
#' @importFrom purrr map_lgl
filter_interactions <- function(interactions, fixed_effects, dat) {
  bool <- map_lgl(interactions, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- data_type(dat[[ss[1]]])
    type2 <- data_type(dat[[ss[2]]])
    
    return((type1 == "cat" && !ss[1] %in% fixed_effects) ||
           (type2 == "cat" && !ss[2] %in% fixed_effects))
  })
  
  return(interactions[bool])
}

# Keep interaction in expected order for Stan code generation:
# binary first, then categorical, then continuous.
#' Sort Interaction Terms by Variable Type
#'
#' @description Reorders variables within interaction terms to follow Stan code
#' generation conventions: binary variables first, then categorical, then continuous.
#' This ensures consistent parameter ordering in the generated Stan model.
#'
#' @param interactions Character vector of interaction terms in "var1:var2" format
#' @param dat Data frame containing the variables referenced in interactions
#'
#' @return Character vector of interaction terms with variables reordered within
#'   each term according to type hierarchy (binary < categorical < continuous)
#'
#' @noRd
#'
#' @importFrom purrr map_chr
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

#' Create All Possible Two-Way Interactions
#'
#' @description Generates all unique two-way interaction terms from the combination
#' of fixed and varying effects. Returns empty list if fewer than 2 main effects
#' are available for interaction.
#'
#' @param fixed_effects Character vector of fixed effect variable names
#' @param varying_effects Character vector of varying effect variable names
#' @param dat Data frame containing the variables (used for validation)
#'
#' @return Character vector of interaction terms in "var1:var2" format, where
#'   var1 <= var2 alphabetically to ensure uniqueness
#'
#' @noRd
#'
#' @importFrom dplyr filter n_distinct
#' @importFrom rlang .data
create_interactions <- function(fixed_effects, varying_effects, dat) {
  main_effects <- c(fixed_effects, varying_effects)
  
  if(n_distinct(main_effects) <= 1) {
    return(list())
  }
  
  # create unique pairs
  df <- expand.grid(
    eff1 = main_effects,
    eff2 = main_effects,
    stringsAsFactors = FALSE
  ) %>%
    filter(.data$eff1 != .data$eff2)
  
  df <- df[apply(df, 1, function(x) x[1] <= x[2]), ]
  int <- paste0(df$eff1, ":", df$eff2)
  
  return(int)
}

#' Compute Interaction Levels for Categorical Variables
#'
#' @description Calculates interaction level indices for two categorical variables,
#' handling the special case where one variable is binary (2 levels) differently
#' from general categorical interactions.
#'
#' @param levels1 Numeric vector of factor levels for first variable
#' @param levels2 Numeric vector of factor levels for second variable
#'
#' @return Numeric vector of interaction level indices. For binary interactions,
#'   uses element-wise multiplication with 0s converted to 1s. For categorical
#'   interactions, uses formula: (levels1 - 1) * n_categories2 + levels2
#'
#' @noRd
#'
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

#' Group Fixed Effects by Variable Type
#'
#' @description Separates fixed effects into categorical and binary/continuous
#' groups. For categorical variables, creates dummy variable names excluding
#' the reference level (first level alphabetically).
#'
#' @param fixed Named list of fixed effects with prior specifications
#' @param dat Data frame containing the variables to determine types
#'
#' @return List with two components:
#'   \itemize{
#'     \item cat: Named list of categorical dummy variables with their priors
#'     \item bincont: Named list of binary/continuous variables with their priors
#'   }
#'
#' @noRd
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

#' Group Interaction Terms by Statistical Type
#'
#' @description Classifies interaction terms into different modeling categories
#' based on the variable types involved. This determines how interactions are
#' implemented in the hierarchical model structure.
#'
#' @param interactions Named list of interaction terms with their prior specifications
#' @param dat Data frame containing the variables to determine types
#'
#' @return List with four components:
#'   \itemize{
#'     \item fixed_slope: Continuous×continuous, binary×binary, binary×continuous interactions
#'     \item varying_slope: Categorical×continuous interactions
#'     \item varying_intercept: Categorical×categorical interactions
#'     \item varying_intercept_special: Binary×categorical interactions
#'   }
#'
#' @noRd
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
    if((type1 == "cont" && type2 == "cont") ||
       (type1 == "bin" && type2 == "bin") ||
       (type1 == "cont" && type2 == "bin") ||
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

#' Group All Model Effects by Type and Prior Structure
#'
#' @description Organizes all model effects (intercept, fixed, varying, interactions)
#' into a structured format suitable for Stan code generation. Separates interactions
#' with and without structured priors.
#'
#' @param effects List containing all model effects:
#'   \itemize{
#'     \item Intercept: Global intercept prior
#'     \item fixed: Fixed effects specifications
#'     \item varying: Varying effects specifications
#'     \item interaction: Interaction effects specifications
#'   }
#' @param dat Data frame containing model variables for type determination
#'
#' @return Structured list with components:
#'   \itemize{
#'     \item Intercept: Global intercept specification
#'     \item fixed: Grouped fixed effects (categorical vs binary/continuous)
#'     \item varying: Varying main effects
#'     \item interaction: Grouped interactions without structured priors
#'     \item structured: Grouped interactions with structured priors
#'   }
#'
#' @noRd
group_effects <- function(effects, dat) {
  out <- list()
  
  # global intercept
  out$Intercept <- effects$Intercept
  
  # fixed main effects
  out$fixed <- group_fixed(effects$fixed, dat)
  
  # varying main effects
  out$varying <- if(is.null(effects$varying)) list() else effects$varying
  
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

#' Ungroup Effects into Flat Structure for Stan Code Generation
#'
#' @description Flattens the grouped effects structure into individual components
#' with standardized naming conventions for easier Stan code generation.
#'
#' @param effects Grouped effects structure from group_effects()
#'
#' @return List with flattened effect components:
#'   \itemize{
#'     \item Intercept: Global intercept
#'     \item m_fix_bc: Binary/continuous fixed main effects
#'     \item m_fix_c: Categorical fixed main effects
#'     \item m_var: Varying main effects
#'     \item i_fixsl: Fixed-slope interactions
#'     \item i_varsl: Varying-slope interactions
#'     \item i_varit: Varying-intercept interactions
#'     \item i_varits: Special varying-intercept interactions
#'     \item s_varsl: Structured varying-slope interactions
#'     \item s_varit: Structured varying-intercept interactions
#'     \item s_varits: Special structured varying-intercept interactions
#'   }
#'
#' @noRd
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
data_ <- function(effects, metadata) {
  scode <- "
  int<lower=1> N;
  int<lower=0> K;
  matrix[N, K] X;
  int<lower=1> N_pop;
  int<lower=0> K_pop;
  matrix[N_pop, K_pop] X_pop;
  "

  # outcome variable
  if (metadata$family == "binomial") {
    scode <- paste0(scode, "
  array[N] int y;
  array[N] int n_sample;
  ")
  } else if (metadata$family == "normal") {
    scode <- paste0(scode, "
  array[N] real y;
  ")
  }
  
  # varying effects & fixed effects of categorical variables
  m_fix_c_names <- purrr::map_chr(
    names(effects$m_fix_c),
    function(s) strsplit(s, split = "\\.")[[1]][1]
  ) %>% 
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
  
  for(s in metadata$pstrat_vars) {
    scode <- paste0(scode, str_interp("
  int<lower=1> N_${s}_pstrat;
  array[N_pop] int<lower=1, upper=N_${s}_pstrat> J_${s}_pstrat;
  vector<lower=0, upper=1>[N_pop] P_${s}_pstrat;
  ")) 
  }
  
  if(metadata$is_timevar) {
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

parameters_ <- function(effects, metadata) {
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

  # include residual SD for normally distributed outcome
  if(metadata$family == "normal") {
    scode <- paste0(scode, "
  real<lower=0> sigma;
  ")
  
  # include the parameters below if structured prior is used
  int_struct <- c(effects$s_varsl, effects$s_varit, effects$s_varits)
  if(length(int_struct) > 0) {
    scode <- paste0(scode, "
  real<lower=0> tau;
  real<lower=0> delta;")
  }
  
  return(scode)
}

transformed_parameters_ <- function(effects, metadata) {
  scode <- ""
  
  struct_effects <- c(
    names(effects$s_varsl) %>%
      map(function(s) strsplit(s, ':')[[1]][1]),
    names(c(effects$s_varit, effects$s_varits)) %>%
      map(function(s) strsplit(s, ':')[[1]]) %>%
      do.call(c, .)
  ) %>%
    unlist() %>%
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
  s_main <- if (metadata$family == "binomial") {
    "
  vector<lower=0, upper=1>[N] p = inv_logit(Intercept%s%s%s%s);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);"
  } else if (metadata$family == "normal") {
    "
  vector[N] mu = Intercept%s%s%s%s;"
  }
  s_fixed <- if(length(fixed) > 0) " + X * beta" else ""
  s_mvar <- paste(map(names(effects$m_var), ~ str_interp(" + a_${.x}[J_${.x}]")), collapse = "")
  s_int_varit <- paste(map(gsub(':', '', names(int_varit)), ~ str_interp(" + a_${.x}[J_${.x}]")), collapse = "")
  s_int_varsl <- paste(map(names(int_varsl), function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    return(str_interp(" + b_${s}[J_${ss[1]}] .* X[:, ${which(names(effects$m_fix_bc) == ss[2])}]"))
  }), collapse = "")
  
  scode <- paste0(scode, sprintf(s_main, s_fixed, s_mvar, s_int_varit, s_int_varsl))
  
  return(scode)
}

model_ <- function(effects, metadata) {
  # group effects
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varsl <- c(effects$i_varsl, effects$s_varsl)
  int_varsl_wo_struct <- effects$i_varsl
  int_varit <- c(effects$i_varit, effects$i_varits, effects$s_varit, effects$s_varits)
  int_varit_wo_struct <- c(effects$i_varit, effects$i_varits)

  # outcome distribution
  scode <- if (metadata$family == "binomial") {
    "
  y ~ binomial(n_sample, p_sample);"
  } else if (metadata$family == "normal") {
    "
  sigma ~ exponential(1/sd(y));
  y ~ normal(mu, sigma);"
  }
  
  scode <- paste0(
    scode, 
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


#' Generate Stan Code for Leave-One-Out Cross-Validation
#'
#' @description Creates generated quantities block code for computing log-likelihood
#' values needed for leave-one-out cross-validation (LOO-CV) model comparison.
#'
#' @return Character string containing Stan generated quantities code that computes
#'   log_lik vector with binomial log probability mass function values for each observation
#'
#' @noRd
gq_loo <- function(metadata) {
  scode <- if (metadata$family == "binomial") {
    "\n  log_lik = binomial_lpmf(y | n_sample, p_sample);"
  } else if (metadata$family == "normal") {
    "\n  log_lik = normal_lpdf(y | mu, rep_vector(sigma, N));"
  }

  return(scode)
}

#' Generate Stan Code for Posterior Predictive Checks
#'
#' @description Creates generated quantities block code for posterior predictive
#' checking by generating replicated datasets from the fitted model.
#'
#' @return Character string containing Stan generated quantities code that generates
#'   y_rep array with binomial random draws using fitted probabilities
#'
#' @noRd
gq_ppc <- function(metadata) {
  scode <- if (metadata$family == "binomial") {
    "\n  array[N] int<lower = 0> y_rep = binomial_rng(n_sample, p_sample);"
  } else if (metadata$family == "normal") {
    "\n  array[N] real y_rep = normal_rng(mu, rep_vector(sigma, N));"
  }

  return(scode)
}

#' Generate Stan Code for Poststratification
#'
#' @description Creates generated quantities block code for multilevel regression
#' and poststratification (MRP). Generates population-level predictions and
#' aggregates them using poststratification weights.
#'
#' @param effects Ungrouped effects structure from ungroup_effects()
#' @param metadata List containing poststratification specifications:
#'   \itemize{
#'     \item pstrat_vars: Character vector of demographic subgroups
#'     \item is_timevar: Logical indicating whether data contains time information
#'   }
#'
#' @return Character string containing Stan generated quantities code for:
#'   \itemize{
#'     \item Sampling new varying effect levels for population prediction
#'     \item Computing population-level probabilities (p_pop)
#'     \item Aggregating by subgroups using poststratification weights
#'     \item Temporal aggregation if specified
#'   }
#'
#' @noRd
#'
#' @importFrom stringr str_interp
gq_pstrat <- function(effects, metadata) {
  if(is.null(metadata)) {
    return("")
  }

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
  if(metadata$is_timevar) {
    init_overall <- "vector<lower=0, upper=1>[N_time_pstrat] p_overall_pop = rep_vector(0, N_time_pstrat);"
    init_marginal <- paste(map(metadata$pstrat_vars, ~ str_interp("
  matrix<lower=0, upper=1>[N_${.x}_pstrat, N_time_pstrat] p_${.x}_pop = rep_matrix(0, N_${.x}_pstrat, N_time_pstrat);")), collapse = "")
  } else {
    init_overall <- "real<lower=0, upper=1> p_overall_pop;"
    init_marginal <- paste(map(metadata$pstrat_vars, ~ str_interp("
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
  
  if(metadata$is_timevar) {
    est_overall <- "
    p_pop_scaled = p_pop .* P_overall_pstrat;
    for (i in 1:N_pop) {
      p_overall_pop[J_time_pstrat[i]] += p_pop_scaled[i];
    }"
    
    est_marginal <- paste(map(metadata$pstrat_vars, ~ str_interp("
    p_pop_scaled = p_pop .* P_${.x}_pstrat;
    for (i in 1:N_pop) {
      p_${.x}_pop[J_${.x}_pstrat[i], J_time_pstrat[i]] += p_pop_scaled[i];
    }")), collapse = "")
  } else {
    est_overall <- str_interp("
    p_pop_scaled = p_pop .* P_overall_pstrat;
    p_overall_pop = sum(p_pop_scaled);")
    
    est_marginal <- paste(map(metadata$pstrat_vars, ~ str_interp("
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


#' Generate Complete Stan Code for MCMC Sampling
#'
#' @description Creates a complete Stan program for MCMC sampling by combining
#' data, parameters, transformed parameters, and model blocks. This is the main
#' function for generating Stan code for model fitting.
#'
#' @param effects Ungrouped effects structure from ungroup_effects()
#' @param metadata Optional list containing generated quantities specifications for
#'   poststratification. If provided, includes additional data declarations.
#'
#' @return Character string containing complete Stan program with data, parameters,
#'   transformed parameters, and model blocks ready for compilation and sampling
#'
#' @noRd
#'
#' @importFrom stringr str_interp
make_stancode_mcmc <- function(effects, metadata=NULL) {
  
  scode <- str_interp("
data { ${data_(effects, metadata)}
}

parameters { ${parameters_(effects, metadata)}
}

transformed parameters { ${transformed_parameters_(effects, metadata)}
}

model { ${model_(effects, metadata)}
}
  ")
  
  return(scode)
}

#' Generate Stan Code for Generated Quantities
#'
#' @description Creates Stan programs for generated quantities including
#' leave-one-out cross-validation, posterior predictive checks, and poststratification.
#' Uses fitted parameters from MCMC to generate additional quantities of interest.
#'
#' @param effects Ungrouped effects structure from ungroup_effects()
#' @param gq_type Character string specifying type of generated quantities:
#'   \itemize{
#'     \item "loo": Leave-one-out cross-validation log-likelihood
#'     \item "ppc": Posterior predictive check replications
#'     \item "pstrat": Poststratification estimates
#'   }
#' @param metadata List containing specifications for poststratification (required if gq_type="pstrat")
#'
#' @return Character string containing complete Stan program with data, parameters,
#'   transformed parameters, and generated quantities blocks
#'
#' @noRd
#'
#' @importFrom stringr str_interp
make_stancode_gq <- function(
  effects,
  metadata,
  gq_type = c("loo", "ppc", "pstrat")
) {
  gq_type <- match.arg(gq_type)
  gq_code <- switch(gq_type,
    "loo" = gq_loo(metadata),
    "ppc" = gq_ppc(metadata),
    "pstrat" = gq_pstrat(effects, metadata)
  )
  
  scode <- str_interp("
data { ${data_(effects, metadata)}
}

parameters { ${parameters_(effects, metadata)}
}

transformed parameters { ${transformed_parameters_(effects, metadata)}
}

generated quantities { ${gq_code}
}
  ")
  
  return(scode)
}

#' Convert Data Frame Variables to Stan-Compatible Format
#'
#' @description Transforms variables in a data frame to formats suitable for Stan
#' modeling. Binary and categorical variables are converted to integer factors,
#' continuous variables are standardized, and original values are preserved with "_raw" suffix.
#'
#' @param df Data frame to transform
#' @param ignore_cols Character vector of column names to exclude from transformation
#'
#' @return Data frame with transformed variables and original values preserved:
#'   \itemize{
#'     \item Binary variables: Converted to 0/1 integers
#'     \item Categorical variables: Converted to 1-based integer factors
#'     \item Continuous variables: Standardized (mean=0, sd=1)
#'     \item Original values: Saved with "_raw" suffix
#'   }
#'
#' @noRd
#' @importFrom dplyr mutate select rename_with bind_cols across all_of
stan_factor <- function(df, ignore_cols = GLOBAL$vars$ignore) {
  # find the columns to mutate
  col_names <- setdiff(names(df), ignore_cols)
  
  # save the “raw” columns
  df_raw <- df %>%
    select(all_of(col_names)) %>%
    rename_with(~ paste0(.x, "_raw"), everything())
  
  # transform the original columns in-place
  df_mutated <- df %>%
    mutate(
      across(
        all_of(col_names),
        ~ {
          vec   <- .
          dtype <- data_type(vec)
          
          if (dtype %in% c("bin", "cat")) {
            if (is.factor(vec)) vec <- as.character(vec)
            enc <- as.numeric(factor(vec))
            if (dtype == "bin") enc <- enc - 1L
            enc
            
          } else if (dtype == "cont") {
            as.numeric(scale(vec))
            
          } else {
            vec
          }
        }
      )
    )
  
  # concatenate the "raw" columns
  df_out <- bind_cols(df_mutated, df_raw)
  
  return(df_out)
}



#' Create Stan Data List for Model Fitting
#'
#' @description Constructs the data list required for Stan model fitting, including
#' observation data, design matrices, grouping variables, and optional poststratification
#' data. Handles both input data for fitting and new data for prediction.
#'
#' @param input_data Data frame containing observations for model fitting with columns:
#'   \itemize{
#'     \item positive: Number of positive outcomes
#'     \item total: Total number of trials
#'     \item Additional variables specified in effects
#'   }
#' @param new_data Data frame containing population data for prediction/poststratification
#' @param effects Ungrouped effects structure from ungroup_effects()
#' @param metadata Optional list for poststratification specifications
#' @param sens Numeric value for sensitivity parameter (default 1)
#' @param spec Numeric value for specificity parameter (default 1)
#'
#' @return Named list containing all data elements required by Stan:
#'   \itemize{
#'     \item N, y, n_sample: Observation counts and sample sizes
#'     \item K, X: Fixed effects design matrix dimensions and values
#'     \item N_pop, K_pop, X_pop: Population data for prediction
#'     \item Grouping variables (N_*, J_*) for varying effects
#'     \item Poststratification weights and indices (if metadata provided)
#'     \item sens, spec: Sensitivity and specificity parameters
#'   }
#'
#' @noRd
#'
#' @importFrom dplyr select mutate group_by n_distinct
#' @importFrom stringr str_interp
#' @importFrom rlang syms .data
make_standata <- function(
    input_data,
    new_data,
    effects,
    metadata,
    sens = 1,
    spec = 1
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
    X_cont <- dat %>%
      select(all_of(names(effects$m_fix_bc))) %>%
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
    ) %>% 
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
      stan_data[[str_interp("J_${s}${subfix}")]] <- factor(int_lvls, levels = unq_int_lvls, labels = 1:n_int_lvls) %>% as.numeric()
      
      if(name == "input") {
        stan_data[[str_interp("I_${s}")]] <- unq_int_lvls
      }
    }
  }

  # poststratification
  if (!is.null(metadata)) {
    pstrat_data <- new_data %>%
      mutate(
        across(everything(), ~ if(n_distinct(.x) == 2 && all(sort(unique(.x)) == c(0, 1))) .x + 1 else .x),
        overall = 1
      )
    for(s in c("overall", metadata$pstrat_vars)) {
      group_cols <- if(metadata$is_timevar) c("time", s) else c(s)
      
      pop_prop <- pstrat_data %>%
        group_by(!!!syms(group_cols)) %>%
        mutate(prop = .data$total / sum(.data$total))
      
      if(s != "overall") {
        stan_data[[str_interp("N_${s}_pstrat")]] <- n_distinct(pstrat_data[[s]])
        stan_data[[str_interp("J_${s}_pstrat")]] <- pstrat_data[[s]] 
      }
      stan_data[[str_interp("P_${s}_pstrat")]] <- pop_prop$prop
    }
    
    if(metadata$is_timevar) {
      stan_data$N_time_pstrat <- n_distinct(new_data$time)
      stan_data$J_time_pstrat <- new_data$time
    }
  }

  return(stan_data)
}

run_mcmc <- function(
    input_data,
    new_data,
    effects,
    metadata,
    n_iter = 1000,
    n_chains = 4,
    seed = NULL,
    sens = 1,
    spec = 1,
    code_fout = NULL,
    silent = FALSE
) {

  stan_code <- list()
  stan_code$mcmc <- make_stancode_mcmc(effects, metadata)
  stan_code$ppc <- make_stancode_gq(effects, metadata, "ppc")
  stan_code$loo <- make_stancode_gq(effects, metadata, "loo")
  stan_code$pstrat <- make_stancode_gq(effects, metadata, "pstrat")
  
  stan_data <- make_standata(input_data, new_data, effects, metadata, sens, spec)

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
    show_messages = !silent,
    show_exception = !silent,
    seed = seed
  )

  return(list(
    fit = fit,
    stan_data = stan_data,
    stan_code = stan_code
  ))
}

run_gq <- function(
    fit_mcmc,
    stan_code,
    stan_data,
    n_chains
  ) {
  

  utils::capture.output({
    mod_gq <- cmdstanr::cmdstan_model(
      stan_file = cmdstanr::write_stan_file(stan_code),
      cpp_options = list(stan_threads = TRUE)
    )
  }, type = "message")

  
  utils::capture.output({
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
  m_fix_bc_names <- names(effects$m_fix_bc) %>%
    purrr::map_chr(function(s) {
      if (data_type(input_data[[s]]) == "bin") {
        df <- data.frame(x = input_data[[s]]) %>% stan_factor()
        eq1 <- unique(df$x_raw[df$x == 1])
        paste0(s, ".", eq1)
      } else {
        s
      }
    })

  row.names(df_fixed) <- c("Intercept", c(m_fix_bc_names, names(effects$m_fix_c), names(effects$i_fixsl)))

  ### include reference levels for categorical variables
  # get variable names
  m_fix_c_vars <- names(effects$m_fix_c) %>%
    purrr::map_chr(function(s) strsplit(s, split = '\\.')[[1]][1]) %>%
    unique()

  # dummy variables including reference levels
  m_fix_c_names <- m_fix_c_vars %>%
    purrr::map(function(s) {
      levels <- sort(unique(input_data[[s]]))
      paste0(s, ".", levels)
    }) %>%
    unlist(use.names = FALSE)

  row_names <- c("Intercept", m_fix_bc_names, m_fix_c_names, names(effects$i_fixsl))
  idx <- match(row_names, row.names(df_fixed))
  df_fixed <- df_fixed[idx, ] # NA indices are given rows with NA values
  row.names(df_fixed) <- row_names

  return(df_fixed)
}

#' @importFrom dplyr select
#' @importFrom rlang .data
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
    ) %>%
      select(.data$mean, .data$sd, .data$`q2.5`, .data$`q97.5`, .data$rhat, .data$ess_bulk, .data$ess_tail) %>%
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
    df_varying <- df_varying %>%
      select(.data$mean, .data$sd, .data$`q2.5`, .data$`q97.5`, .data$rhat, .data$ess_bulk, .data$ess_tail) %>%
      as.data.frame()

    names(df_varying) <- c("Estimate", "Est.Error", "l-95% CI", "u-95% CI", "R-hat", "Bulk_ESS", "Tail_ESS")
    row.names(df_varying) <- row_names
  }

  return(list(
    fixed = df_fixed,
    varying = df_varying
  ))
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
  summary <- data.frame(
    Metric = c("Divergence", "Maximum tree depth", "E-BFMI"),
    Message = c(divergent_msg, treedepth_msg, ebfmi_msg),
    stringsAsFactors = FALSE
  )
  
  return(list(
    summary = summary,
    show_warnings = total_divergent > 0
  ))
}

#' @importFrom dplyr select mutate arrange distinct across all_of
extract_est <- function(
  fit,
  new_data,
  metadata
) {
  
  # convert new data to numeric factors
  col_names <- if(metadata$is_timevar) c(metadata$pstrat_vars, "time") else metadata$pstrat_vars
  new_data <- new_data %>%
    select(all_of(col_names)) %>%
    mutate(overall = "overall") %>%  # add placeholder column for overall estimates
    stan_factor()

  est <- list()

  for(s in c("overall", metadata$pstrat_vars)) {
    # get posterior draws for each subgroup
    pred_mat <- fit$draws(
      variables = str_interp("p_${s}_pop"),
      format = "draws_matrix"
    ) %>% t()

    col_names <- if(metadata$is_timevar) c("time", s) else c(s)
    raw_col_names <- paste0(col_names, "_raw")
    new_col_names <- if(metadata$is_timevar) c("time", "factor") else c("factor")

    # Order raw levels based on numeric levels to match order of posterior draws matrix
    est[[s]] <- new_data %>%
      arrange(across(all_of(col_names))) %>%
      distinct(across(all_of(raw_col_names))) %>%
      stats::setNames(new_col_names) %>%
      mutate(
        est = pred_mat %>% apply(1, mean),
        std = pred_mat %>% apply(1, stats::sd)
      )
  }

  return(est)
}

#' Extract Posterior Predictive Replications
#'
#' @description Extracts posterior predictive replications (y_rep) from generated
#' quantities for posterior predictive checking. Can aggregate temporally and
#' provide summary statistics or full posterior draws.
#'
#' @param fit CmdStanR generated quantities fit object from posterior predictive checks
#' @param input_data Original input data frame used for model fitting
#' @param boolean indicator for whether data contains time information
#' @param N Integer number of posterior draws to extract (default 10)
#' @param summarize Logical whether to return summary statistics (default FALSE)
#' @param pred_interval Numeric prediction interval coverage (default 0.95)
#'
#' @return Posterior predictive replications:
#'   \itemize{
#'     \item If is_timevar=FALSE & summarize=FALSE: Numeric vector of proportion estimates
#'     \item If is_timevar=FALSE & summarize=TRUE: Data frame with quantiles (upper, lower, median)
#'     \item If is_timevar=TRUE & summarize=FALSE: Data frame with time and proportion columns
#'     \item If is_timevar=TRUE & summarize=TRUE: Data frame with time and quantile summaries
#'   }
#'
#' @noRd
#'
#' @importFrom rlang .data
#' @importFrom dplyr select mutate group_by summarise_all ungroup mutate_all
extract_yrep <- function(
  fit,
  input_data,
  is_timevar,
  N = 10,
  summarize = FALSE,
  pred_interval = 0.95
) {

  # get draws from cmdstanr fit
  yrep_mat <- fit$draws(
    variables = "y_rep",
    format = "draws_matrix"
  )%>% t()
  
  yrep_mat <- yrep_mat[, sample(ncol(yrep_mat), N)]   # subset draws
  
  qlower <- (1 - pred_interval) / 2
  qupper <- 1 - qlower

  if(is_timevar) {
    agg_df <- yrep_mat %>%
      as.data.frame() %>%
      mutate(
        time = input_data$time,
        total = input_data$total
      ) %>%
      group_by(.data$time) %>%
      summarise_all(sum) %>%
      ungroup()

    agg_tests <- agg_df$total
    time <- agg_df$time

    est <- agg_df %>%
      select(-c("time", "total")) %>%
      mutate_all(function(c) c / agg_tests)

    if(summarize) {
      est <- data.frame(
        time = time,
        upper = est %>% apply(1, stats::quantile, qlower),
        lower = est %>% apply(1, stats::quantile, qupper),
        median = est %>% apply(1, stats::quantile, 0.5)
      )
    } else {
      est <- est %>% mutate(time = time)
    }

  } else {
    est <- colSums(yrep_mat) / sum(input_data$total)

    if(summarize) {
      est <- data.frame(
        upper  = stats::quantile(est, qlower),
        lower  = stats::quantile(est, qupper),
        median = stats::quantile(est, 0.5)
      )
    }

  }

  return(est)
}
