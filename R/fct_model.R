#' Clean Prior Distribution Syntax
#'
#' @description Standardizes prior distribution syntax by removing whitespace
#' and converting to lowercase for consistent parsing and validation. Used
#' internally to normalize user input before validation.
#'
#' @param s A character string representing a prior distribution specification
#'   (e.g., "Normal(0, 1)" or "student_t(3, 0, 1)").
#'
#' @return A cleaned character string with whitespace removed and converted to
#'   lowercase (e.g., "normal(0,1)" or "student_t(3,0,1)").
#' @noRd
#' @keywords internal
.clean_prior_syntax <- function(s) {
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
#' for valid syntax or NULL inputs. Used to ensure user-specified priors are
#' properly formatted before Stan code generation.
#'
#' @param s A character string representing a prior distribution specification.
#'   Expected formats:
#'   \itemize{
#'     \item normal(mean, sd): e.g., "normal(0,1)" - mean can be 0, sd must be positive
#'     \item student_t(df, location, scale): e.g., "student_t(3,0,1)" - df must be positive
#'     \item structured: "structured" - for hierarchical structured priors
#'     \item icar: "icar" - for intrinsic conditional autoregressive priors
#'     \item bym2: "bym2" - for BYM2 priors
#'   }
#'
#' @return Logical value indicating whether the syntax is valid (TRUE) or invalid (FALSE).
#'   Returns TRUE for NULL inputs (no prior specified).
#' @noRd
#' @keywords internal
.check_prior_syntax <- function(s) {
  if (is.null(.nullify(s))) {
    return(TRUE)
  }

  # decimal patterns:
  decimal <- "[0-9]+(?:\\.[0-9]+)?"    # e.g. 0, 3.14, 42
  signed_decimal <- "-?[0-9]+(?:\\.[0-9]+)?" # e.g. -1,  0.5, -2.718

  patterns <- list(
    normal = paste0("^normal\\(",
                    signed_decimal, ",",    # mean can be negative or positive decimal
                    decimal,                # sd must be non-negative decimal
                    "\\)$"),
    student_t = paste0("^student_t\\(",
                       "[1-9][0-9]*,",         # df: positive integer
                       signed_decimal, ",",    # location (μ): signed decimal
                       decimal,                # scale (σ): non-negative decimal
                       "\\)$"),
    structured = "^structured$",
    icar = "^icar$",
    bym2 = "^bym2$"
  )

  dist <- strsplit(s, "\\(", fixed = FALSE)[[1]][1]

  if (dist %in% names(patterns)) {
    grepl(patterns[[dist]], s)
  } else {
    FALSE
  }
}


#' Set Default Prior Distributions for Model Effects
#'
#' @description Replaces NULL prior specifications with default priors from the
#' global configuration. Ensures all model effects have valid prior distributions
#' before Stan code generation. Uses .const()$default_priors to provide sensible
#' defaults for different effect types.
#'
#' @param effects List containing model effects with potentially NULL prior specifications:
#'   \itemize{
#'     \item intercept: Global intercept prior
#'     \item fixed: Fixed effects priors
#'     \item varying: Varying effects priors
#'     \item interaction: Interaction effects priors
#'   }
#'
#' @return List with same structure as input but with NULL priors replaced by
#'   appropriate defaults from .const()$default_priors. Each effect type gets its
#'   corresponding default prior distribution.
#' @noRd
#' @keywords internal
.set_default_priors <- function(effects) {
  for (type in c("intercept", .const()$args$effect_types)) {
    effects[[type]] <- purrr::map(
      effects[[type]],
      ~ .nullify(.x) %||% .const()$default_priors[[type]]
    )
  }

  return(effects)
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
#' @keywords internal
.group_fixed <- function(fixed, dat) {
  out <- list(
    cat = list(),
    bincont = list()
  )

  for(s in names(fixed)) {
    if(.data_type(dat[[s]]) == "cat") {
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

# ' Group Varying Effects
#'
#' @description Simply returns the varying effects as is. Placeholder for
#' potential future processing or validation of varying effects.
#' 
#' @param varying Named list of varying effects with prior specifications
#' @param dat Data frame containing the variables to determine types (not used currently)
#'
#' @return Named list of varying effects unchanged.
#' @noRd
#' @keywords internal 
.group_varying <- function(varying, dat) {
  out <- list()
  
  if (length(varying) > 0) {
    out <- varying
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
#' @noRd
#' @keywords internal
.group_interactions <- function(interactions, dat) {
  out <- list(
    fixed_slope = list(),
    varying_slope = list(),
    varying_intercept = list(),
    varying_intercept_special = list()
  )
  
  for(s in names(interactions)) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- .data_type(dat[[ss[1]]])
    type2 <- .data_type(dat[[ss[2]]])
    
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
#'     \item intercept: Global intercept prior
#'     \item fixed: Fixed effects specifications
#'     \item varying: Varying effects specifications
#'     \item interaction: Interaction effects specifications
#'   }
#' @param dat Data frame containing model variables for type determination
#'
#' @return Structured list with components:
#'   \itemize{
#'     \item intercept: Global intercept specification
#'     \item fixed: Grouped fixed effects (categorical vs binary/continuous)
#'     \item varying: Varying main effects
#'     \item interaction: Grouped interactions without structured priors
#'     \item structured: Grouped interactions with structured priors
#'   }
#' @noRd
#' @keywords internal
.group_effects <- function(effects, dat) {
  out <- list()
  
  # global intercept
  out$intercept <- effects$intercept
  
  # fixed main effects
  out$fixed <- .group_fixed(effects$fixed, dat)
  
  # varying main effects with Stan built-in priors
  cond <- !effects$varying %in% .const()$custom_priors
  var_reg <- effects$varying[cond]
  out$varying <- .group_varying(var_reg, dat)

  # varying main effects with ICAR prior
  var_icar <- effects$varying[effects$varying == "icar"]
  out$varying_icar <- .group_varying(var_icar, dat)

  # varying main effects with BYM2 prior
  var_bym2 <- effects$varying[effects$varying == "bym2"]
  out$varying_bym2 <- .group_varying(var_bym2, dat)

  # reorder terms in interactions
  if(!is.null(effects$interaction)) {
    names(effects$interaction) <- .sort_interactions(names(effects$interaction), dat) 
  }
  
  # interactions with Stan built-in priors
  cond <- !effects$interaction %in% .const()$custom_priors
  itr_reg <- effects$interaction[cond]
  out$interaction <- .group_interactions(itr_reg, dat)

  # interactions with structured priors
  itr_struct <- effects$interaction[effects$interaction == "structured"]
  out$interaction_structured <- .group_interactions(itr_struct, dat)


  return(out)
}

#' Ungroup Effects into Flat Structure for Stan Code Generation
#'
#' @description Flattens the grouped effects structure into individual components
#' with standardized naming conventions for easier Stan code generation.
#'
#' @param effects Grouped effects structure from .group_effects()
#'
#' @return List with flattened effect components:
#'   \itemize{
#'     \item intercept: Global intercept
#'     \item m_fix_bc: Binary/continuous fixed main effects
#'     \item m_fix_c: Categorical fixed main effects
#'     \item m_var: Varying main effects
#'     \item m_var_icar: Varying main effects with ICAR prior
#'     \item m_var_bym2: Varying main effects with BYM2 prior
#'     \item i_fixsl: Fixed-slope interactions
#'     \item i_varsl: Varying-slope interactions
#'     \item i_varit: Varying-intercept interactions
#'     \item i_varits: Special varying-intercept interactions
#'     \item i_varsl_str: Structured varying-slope interactions
#'     \item i_varit_str: Structured varying-intercept interactions
#'     \item i_varits_str: Special structured varying-intercept interactions
#'   }
#' @noRd
#' @keywords internal
.ungroup_effects <- function(effects) {
  # for cleaner code
  out <- list(
    intercept = effects$intercept,
    m_fix_bc = effects$fixed$bincont,
    m_fix_c = effects$fixed$cat,
    m_var = effects$varying,
    m_var_icar = effects$varying_icar,
    m_var_bym2 = effects$varying_bym2,
    i_fixsl = effects$interaction$fixed_slope,
    i_varsl = effects$interaction$varying_slope,
    i_varit = effects$interaction$varying_intercept,
    i_varits = effects$interaction$varying_intercept_special,
    i_varsl_str = effects$interaction_structured$varying_slope,
    i_varit_str = effects$interaction_structured$varying_intercept,
    i_varits_str = effects$interaction_structured$varying_intercept_special
  )

  return(out)
}


#' Check if Higher Precision is Needed for Stan
#' 
#' @noRd 
#' @keywords internal
.check_if_needs_higher_precision <- function(effects) {
  if (length(effects$m_var_icar) +
      length(effects$m_var_bym2) > 0) {
    return(TRUE)
  }

  return(FALSE)
}

#' Return numerical precision for Stan
#' 
#' @noRd
#' @keywords internal
.get_stan_precision <- function(effects) {
  if (.check_if_needs_higher_precision(effects)) {
    return(15)
  }

  return(NULL)
}

#' Stan code generation helper for "functions" block
#' 
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#'  all model specifications.
#' @param metadata List containing model specifications including family type
#' for determining the likelihood function and poststratification details.
#'
#' @noRd 
#' @keywords internal
.functions_stan <- function(effects, metadata) {
  scode <- ""

  if (length(effects$m_var_icar) > 0) {
    scode <- paste0(scode, "
  real icar_normal_lpdf(vector phi, array[] int node1, array[] int node2) {
    return -0.5 * dot_self(phi[node1] - phi[node2]);
  }")
  }

  return(scode)
}

#' Stan code generation helper for "data" block
#' 
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#'  all model specifications.
#' @param metadata List containing model specifications including family type
#' for determining the likelihood function and poststratification details.
#' 
#' @return Character string containing Stan "data" block code.
#' 
#' @noRd
#' @keywords internal
.data_stan <- function(effects, metadata) {
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
  for(s in c(m_fix_c_names,
             names(effects$m_var),
             names(effects$m_var_icar),
             names(effects$m_var_bym2))) {
    scode <- paste0(scode, stringr::str_interp("
  int<lower=1> N_${s};
  array[N] int<lower=1, upper=N_${s}> J_${s};
  int<lower=1> N_${s}_pop;
  array[N_pop] int<lower=1, upper=N_${s}_pop> J_${s}_pop;
  "))
  }
  
  # interactions
  int <- c(effects$i_varit, effects$i_varits, effects$i_varit_str, effects$i_varits_str)
  for(s in gsub(':', '', names(int))) {
    scode <- paste0(scode, stringr::str_interp("
  int<lower=1> N_${s};
  array[N] int<lower=1, upper=N_${s}> J_${s};
  int<lower=1> N_${s}_pop;
  array[N_pop] int<lower=1, upper=N_${s}_pop> J_${s}_pop;
  array[N_${s}] int<lower=1, upper=N_${s}_pop> I_${s};
  "))
  }

  # neighborhood graph for ICAR/BYM2 priors
  for (s in names(effects$m_var_icar)) {
    scode <- paste0(scode, stringr::str_interp("
  int<lower=0> N_edges_${s};
  array[N_edges_${s}] int<lower=1, upper=N_${s}> node1_${s};
  array[N_edges_${s}] int<lower=1, upper=N_${s}> node2_${s};
  "))
  }

  # scaling factor for BYM2 priors
  for (s in names(effects$m_var_bym2)) {
    scode <- paste0(scode, stringr::str_interp("
  int<lower=0> N_pos_${s};
  matrix[N_${s}, N_pos_${s}] R_${s};
  "))
  }

  # for poststratification
  scode <- paste0(scode, "
  vector<lower=0, upper=1>[N_pop] P_overall_pstrat;
  ")
  
  for(s in metadata$pstrat_vars) {
    scode <- paste0(scode, stringr::str_interp("
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
  real<lower=0, upper=1> sens;
  real<lower=0, upper=1> spec;")

  return(scode)
}

#' Stan code generation helper for "parameters" block
#' 
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#' all model specifications.
#' @param metadata List containing model specifications including family type
#' for determining the likelihood function and poststratification details.
#' 
#' @return Character string containing Stan "parameters" block code.
#' 
#' @noRd
#' @keywords internal
.parameters_stan <- function(effects, metadata) {
  scode <- "
  real intercept;"
  
  if(length(c(effects$m_fix_bc, effects$m_fix_c)) > 0) {
    scode <- paste0(scode, "\n  vector[K] beta;")
  }
  
  # varying main effect with Stan built-in prior
  for(s in names(effects$m_var)) {
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${s};
  vector[N_${s}] z_${s};"))
  }

  # varying main effect with ICAR prior
  for(s in names(effects$m_var_icar)) {
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${s};
  sum_to_zero_vector[N_${s}] z_${s};"))
  }

  # varying main effect with BYM2 prior
  for(s in names(effects$m_var_bym2)) {
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${s};
  real<lower=0, upper=1> rho_${s};
  vector[N_${s}] theta_${s};
  vector[N_pos_${s}] eta_${s};"))
  }

  # varying-intercept interaction without structured prior
  for(s in names(c(effects$i_varit, effects$i_varits))) {
    s <- gsub(':', '', s)
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${s};
  vector[N_${s}] z_${s};"))
  }
  
  # varying-slope interaction without structured prior
  for(s in names(effects$i_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda2_${s};
  vector[N_${ss[1]}] z2_${s};"))
  }
  
  # varying-intercept interaction with structured prior
  for(s in names(c(effects$i_varit_str, effects$i_varits_str))) {
    s <- gsub(':', '', s)
    scode <- paste0(scode, stringr::str_interp("
  vector[N_${s}] z_${s};"))
  }
  
  # varying-slope interaction with structured prior
  for(s in names(effects$i_varsl_str)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, stringr::str_interp("
  vector[N_${ss[1]}] z2_${s};"))
  }

  # include residual SD for normally distributed outcome
  if(metadata$family == "normal") {
    scode <- paste0(scode, "
  real<lower=0> sigma;")
  }
  
  # include the parameters below if structured prior is used
  int_struct <- c(effects$i_varsl_str, effects$i_varit_str, effects$i_varits_str)
  if(length(int_struct) > 0) {
    scode <- paste0(scode, "
  real<lower=0> tau;
  real<lower=0> delta;")
  }
  
  return(scode)
}

#' Stan code generation helper for "transformed_parameters" block
#' 
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#' all model specifications.
#' @param metadata List containing model specifications including family type
#' for determining the likelihood function and poststratification details.
#' 
#' @return Character string containing Stan "transformed_parameters" block code.
#' 
#' @noRd
#' @keywords internal
.transformed_parameters_stan <- function(effects, metadata) {
  scode <- ""
  
  struct_effects <- c(
    names(effects$i_varsl_str) %>%
      purrr::map(function(s) strsplit(s, ':')[[1]][1]),
    names(c(effects$i_varit_str, effects$i_varits_str)) %>%
      purrr::map(function(s) strsplit(s, ':')[[1]]) %>%
      do.call(c, .)
  ) %>%
    unlist() %>%
    unique()
  
  # scale of varying main effects
  for(s in c(names(effects$m_var),
             names(effects$m_var_icar),
             names(effects$m_var_bym2))) {
    if(s %in% struct_effects) {
      scode <- paste0(scode, stringr::str_interp("
  real<lower=0> scaled_lambda_${s} = lambda_${s} * tau;"))
    } else {
      scode <- paste0(scode, stringr::str_interp("
  real<lower=0> scaled_lambda_${s} = lambda_${s};"))
    }
  }

  # varying main effects with Stan built-in and ICAR priors
  for(s in c(names(effects$m_var), names(effects$m_var_icar))) {
    scode <- paste0(scode, stringr::str_interp("
  vector[N_${s}] a_${s} = z_${s} * scaled_lambda_${s};"))
  }

  # varying main effects with BYM2 prior
  for(s in names(effects$m_var_bym2)) {
    scode <- paste0(scode, stringr::str_interp("
  vector[N_${s}] z_${s} = sqrt(rho_${s}) * R_${s} * eta_${s} + sqrt(1 - rho_${s}) * theta_${s};
  vector[N_${s}] a_${s} = z_${s} * scaled_lambda_${s};"))
  }
  
  # varying-intercept interaction without structured prior
  for(s in names(c(effects$i_varit, effects$i_varits))) {
    s <- gsub(':', '', s)
    scode <- paste0(scode, stringr::str_interp("
  vector[N_${s}] a_${s} = z_${s} * lambda_${s};"))
  }
  
  # varying-slope interaction without structured prior
  for(s in names(effects$i_varsl)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, stringr::str_interp("
  vector[N_${ss[1]}] b_${s} = z2_${s} * lambda2_${s};"))
  }
  
  # varying-intercept interaction with structured prior
  for(s in names(c(effects$i_varit_str))) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${s} = lambda_${ss[1]} * lambda_${ss[2]} * delta * tau;
  vector[N_${s}] a_${s} = z_${s} * lambda_${s};"))
  }
  
  # varying-intercept interaction with structured prior (with binary variable)
  for(s in names(c(effects$i_varits_str))) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda_${s} = lambda_${ss[2]} * delta * tau;
  vector[N_${s}] a_${s} = z_${s} * lambda_${s};"))
  }
  
  # varying-slope interaction with structured prior
  for(s in names(effects$i_varsl_str)) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    scode <- paste0(scode, stringr::str_interp("
  real<lower=0> lambda2_${s} = lambda_${ss[1]} * delta * tau;
  vector[N_${ss[1]}] b_${s} = z2_${s} * lambda2_${s};"))
  }
  
  # build predictor
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  mvar <- c(effects$m_var, effects$m_var_icar, effects$m_var_bym2)
  int_varit <- c(effects$i_varit, effects$i_varits, effects$i_varit_str, effects$i_varits_str)
  int_varsl <- c(effects$i_varsl, effects$i_varsl_str)
  s_formula <- if (metadata$family == "binomial") {
    "
  vector<lower=0, upper=1>[N] p = inv_logit(intercept%s%s%s%s);
  vector<lower=0, upper=1>[N] p_sample = p * sens + (1 - p) * (1 - spec);"
  } else if (metadata$family == "normal") {
    "
  vector[N] mu = intercept%s%s%s%s;"
  }
  s_fixed <- if(length(fixed) > 0) " + X * beta" else ""
  s_mvar <- paste(purrr::map(names(mvar), ~ stringr::str_interp(" + a_${.x}[J_${.x}]")), collapse = "")
  s_int_varit <- paste(purrr::map(gsub(':', '', names(int_varit)), ~ stringr::str_interp(" + a_${.x}[J_${.x}]")), collapse = "")
  s_int_varsl <- paste(purrr::map(names(int_varsl), function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    return(stringr::str_interp(" + b_${s}[J_${ss[1]}] .* X[:, ${which(names(effects$m_fix_bc) == ss[2])}]"))
  }), collapse = "")
  
  scode <- paste0(scode, sprintf(s_formula, s_fixed, s_mvar, s_int_varit, s_int_varsl))
  
  return(scode)
}

#' Stan code generation helper for "model" block
#' 
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#'   all model specifications and prior distributions.
#' @param metadata List containing model specifications including family type
#'   for determining the likelihood function.
#'
#' @return Character string containing Stan "model" block code.
#'
#' @noRd
#' @keywords internal
.model_stan <- function(effects, metadata) {
  # group effects
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varsl <- c(effects$i_varsl, effects$i_varsl_str)
  int_varsl_wo_struct <- effects$i_varsl
  int_varit <- c(effects$i_varit, effects$i_varits, effects$i_varit_str, effects$i_varits_str)
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
  
  dp <- .const()$default_priors

  scode <- paste0(
    scode, 
    stringr::str_interp("\n  intercept ~ ${effects$intercept$intercept};"),
    if(length(fixed) > 0) paste(purrr::map(1:length(fixed), ~ stringr::str_interp("\n  beta[${.x}] ~ ${fixed[[.x]]};")), collapse = ""),
    paste(purrr::map(names(effects$m_var), ~ stringr::str_interp("\n  z_${.x} ~ std_normal();")), collapse = ""),
    paste(purrr::map(names(effects$m_var_icar), ~ stringr::str_interp("\n  z_${.x} ~ icar_normal(node1_${.x}, node2_${.x});")), collapse = ""),
    paste(purrr::map(names(effects$m_var_bym2), ~ stringr::str_interp("\n  theta_${.x} ~ std_normal();\n  eta_${.x} ~ std_normal();")), collapse = ""),
    paste(purrr::map(names(int_varit), ~ stringr::str_interp("\n  z_${gsub(':', '', .x)} ~ std_normal();")), collapse = ""),
    paste(purrr::map(names(int_varsl), ~ stringr::str_interp("\n  z2_${gsub(':', '', .x)} ~ std_normal();")), collapse = ""),
    paste(purrr::map(names(effects$m_var), ~ stringr::str_interp("\n  lambda_${.x} ~ ${effects$m_var[[.x]]};")), collapse = ""),
    paste(purrr::map(names(effects$m_var_icar), ~ stringr::str_interp("\n  lambda_${.x} ~ ${dp$icar_scale};")), collapse = ""),
    paste(purrr::map(names(effects$m_var_bym2), ~ stringr::str_interp("\n  lambda_${.x} ~ ${dp$bym2_scale};\n  rho_${.x} ~ ${dp$bym2_rho};")), collapse = ""),
    paste(purrr::map(names(int_varit_wo_struct), ~ stringr::str_interp("\n  lambda_${gsub(':', '', .x)} ~ ${int_varit[[.x]]};")), collapse = ""),
    paste(purrr::map(names(int_varsl_wo_struct), ~ stringr::str_interp("\n  lambda2_${gsub(':', '', .x)} ~ ${int_varsl[[.x]]};")), collapse = "")
  )
  
  # include the parameters below if structured prior is used
  int_struct <- c(effects$i_varsl_str, effects$i_varit_str, effects$i_varits_str)
  if(length(int_struct) > 0) {
    scode <- paste0(scode, stringr::str_interp("
  tau ~ ${dp$global_scale};
  delta ~ ${dp$local_scale};"))
  }
  
  return(scode)
}

#' Generate Stan Code for Leave-One-Out Cross-Validation
#'
#' @description Creates generated quantities block code for computing log-likelihood
#' values needed for leave-one-out cross-validation (LOO-CV) model comparison.
#'
#' @param metadata List containing model specifications, including outcome distribution family
#'
#' @return Character string containing Stan generated quantities code that computes
#'   log_lik vector with binomial log probability mass function values for each observation
#' @noRd
#' @keywords internal
.gq_loo <- function(metadata) {
  lpf <- switch(metadata$family,
    binomial = "binomial_lpmf(y[n] | n_sample[n], p_sample[n])",
    normal = "normal_lpdf(y[n] | mu[n], sigma)"
  )

  scode <- stringr::str_interp("
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = ${lpf};
  }")

  return(scode)
}

#' Generate Stan Code for Posterior Predictive Checks
#'
#' @description Creates generated quantities block code for posterior predictive
#' checking by generating replicated datasets from the fitted model.
#'
#' @return Character string containing Stan generated quantities code that generates
#'   y_rep array with binomial random draws using fitted probabilities
#' @noRd
#' @keywords internal
.gq_ppc <- function(metadata) {
  scode <- switch(metadata$family,
    binomial = "\n  array[N] int<lower = 0> y_rep = binomial_rng(n_sample, p_sample);",
    normal = "\n  array[N] real y_rep = normal_rng(mu, sigma);"
  )

  return(scode)
}

#' Generate Stan Code for Poststratification
#'
#' @description Creates generated quantities block code for multilevel regression
#' and poststratification (MRP). Generates population-level predictions and
#' aggregates them using poststratification weights.
#'
#' @param effects Ungrouped effects structure from .ungroup_effects()
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
#' @noRd
#' @keywords internal
.gq_pstrat <- function(effects, metadata) {
  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varit <- c(effects$i_varit, effects$i_varits, effects$i_varit_str, effects$i_varits_str)
  int_varsl <- c(effects$i_varsl, effects$i_varsl_str)
  scode <- ""
  
  # sample from posterior for parameters of new levels
  for(s in gsub(':', '', names(int_varit))) {
    scode <- paste0(scode, stringr::str_interp("
  vector[N_${s}_pop] a_${s}_pop;
  if(N_${s} == N_${s}_pop) {
    a_${s}_pop = a_${s};
  } else {
    a_${s}_pop = to_vector(normal_rng(rep_vector(0, N_${s}_pop), rep_vector(lambda_${s}, N_${s}_pop)));
    a_${s}_pop[I_${s}] = a_${s};
  }"))
  }
  
  ### poststratification
  # initialize vectors
  if(metadata$is_timevar) {
    init_overall <- "vector[N_time_pstrat] theta_overall_pop = rep_vector(0, N_time_pstrat);"
    init_marginal <- paste(purrr::map(metadata$pstrat_vars, ~ stringr::str_interp("
  matrix[N_${.x}_pstrat, N_time_pstrat] theta_${.x}_pop = rep_matrix(0, N_${.x}_pstrat, N_time_pstrat);")), collapse = "")
  } else {
    init_overall <- "real theta_overall_pop;"
    init_marginal <- paste(purrr::map(metadata$pstrat_vars, ~ stringr::str_interp("
  vector[N_${.x}_pstrat] theta_${.x}_pop = rep_vector(0, N_${.x}_pstrat);")), collapse = "")
  }
  
  # propulation estimates
  est_cell <- "
    vector[N_pop] theta_pop_scaled;"

  if (metadata$family == "binomial") {
    est_cell <- paste0(est_cell, "
    vector[N_pop] theta_pop = inv_logit(intercept%s%s%s%s);")
  } else if (metadata$family == "normal") {
    est_cell <- paste0(est_cell, "
    vector[N_pop] theta_pop = intercept%s%s%s%s;")
  }

  s_fixed <- if(length(fixed) > 0) " + X_pop * beta" else ""
  s_mvar <- paste(purrr::map(c(names(effects$m_var), names(effects$m_var_str)), ~ stringr::str_interp(" + a_${.x}[J_${.x}_pop]")), collapse = "")
  s_int_varit <- paste(purrr::map(gsub(':', '', names(int_varit)), ~ stringr::str_interp(" + a_${.x}_pop[J_${.x}_pop]")), collapse = "")
  s_int_varsl <- paste(purrr::map(names(int_varsl), function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    s <- paste0(ss[1], ss[2])
    return(stringr::str_interp(" + b_${s}[J_${ss[1]}_pop] .* X_pop[:, ${which(names(effects$m_fix_bc) == ss[2])}]"))
  }), collapse = "")

  est_cell <- sprintf(est_cell, s_fixed, s_mvar, s_int_varit, s_int_varsl)
  
  if(metadata$is_timevar) {
    est_overall <- "
    theta_pop_scaled = theta_pop .* P_overall_pstrat;
    for (i in 1:N_pop) {
      theta_overall_pop[J_time_pstrat[i]] += theta_pop_scaled[i];
    }"
    
    est_marginal <- paste(purrr::map(metadata$pstrat_vars, ~ stringr::str_interp("
    theta_pop_scaled = theta_pop .* P_${.x}_pstrat;
    for (i in 1:N_pop) {
      theta_${.x}_pop[J_${.x}_pstrat[i], J_time_pstrat[i]] += theta_pop_scaled[i];
    }")), collapse = "")
  } else {
    est_overall <- stringr::str_interp("
    theta_pop_scaled = theta_pop .* P_overall_pstrat;
    theta_overall_pop = sum(theta_pop_scaled);")
    
    est_marginal <- paste(purrr::map(metadata$pstrat_vars, ~ stringr::str_interp("
    theta_pop_scaled = theta_pop .* P_${.x}_pstrat;
    for (i in 1:N_pop) {
      theta_${.x}_pop[J_${.x}_pstrat[i]] += theta_pop_scaled[i];
    }")), collapse = "")
  }
  
  scode <- paste0(scode, stringr::str_interp("
  ${init_overall}
  ${init_marginal}
  {  ${est_cell}
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
#' function for generating Stan code for model fitting, assembling all components
#' into a compilable Stan program.
#'
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#'   all model specifications (fixed, varying, interactions).
#' @param metadata List containing model specifications including family type,
#'   poststratification variables, and time-varying flags. Used for data block
#'   generation and outcome distribution specification.
#'
#' @return Character string containing complete Stan program with:
#'   \itemize{
#'     \item data block: Variable declarations and constraints
#'     \item parameters block: Model parameters to be estimated
#'     \item transformed parameters block: Derived quantities and model predictions
#'     \item model block: Likelihood and prior specifications
#'   }
#'   Ready for compilation and MCMC sampling.
#' @noRd
#' @keywords internal
.make_stancode_mcmc <- function(effects, metadata=NULL) {
  
  scode <- stringr::str_interp("
functions { ${.functions_stan(effects, metadata)}
}

data { ${.data_stan(effects, metadata)}
}

parameters { ${.parameters_stan(effects, metadata)}
}

transformed parameters { ${.transformed_parameters_stan(effects, metadata)}
}

model { ${.model_stan(effects, metadata)}
}
  ")
  
  return(scode)
}

#' Generate Stan Code for Generated Quantities
#'
#' @description Creates Stan programs for generated quantities including
#' leave-one-out cross-validation, posterior predictive checks, and poststratification.
#' Uses fitted parameters from MCMC to generate additional quantities of interest
#' without re-estimating parameters.
#'
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#'   model specifications needed for generated quantities.
#' @param metadata List containing model specifications including family type
#'   and poststratification variables.
#' @param gq_type Character string specifying type of generated quantities:
#'   \itemize{
#'     \item "loo": Leave-one-out cross-validation log-likelihood values
#'     \item "ppc": Posterior predictive check replicated datasets
#'     \item "pstrat": Poststratification population-level estimates
#'   }
#'
#' @return Character string containing complete Stan program with:
#'   \itemize{
#'     \item data block: Same as MCMC program
#'     \item parameters block: Same as MCMC program
#'     \item transformed parameters block: Same as MCMC program
#'     \item generated quantities block: Specific to gq_type
#'   }
#'   Ready for use with generate_quantities() method.
#' @noRd
#' @keywords internal
.make_stancode_gq <- function(
  effects,
  metadata,
  gq_type = c("loo", "ppc", "pstrat")
) {
  gq_type <- match.arg(gq_type)
  gq_code <- switch(gq_type,
    "loo" = .gq_loo(metadata),
    "ppc" = .gq_ppc(metadata),
    "pstrat" = .gq_pstrat(effects, metadata)
  )
  
  scode <- stringr::str_interp("
data { ${.data_stan(effects, metadata)}
}

parameters { ${.parameters_stan(effects, metadata)}
}

transformed parameters { ${.transformed_parameters_stan(effects, metadata)}
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
#' Uses .const()$vars$ignore to determine which columns to exclude from transformation.
#'
#' @param df Data frame to transform with variables of different types.
#'
#' @return Data frame with transformed variables and original values preserved:
#'   \itemize{
#'     \item Binary variables: Converted to 0/1 integers (first level = 0, second = 1)
#'     \item Categorical variables: Converted to 1-based integer factors
#'     \item Continuous variables: Standardized using scale() (mean=0, sd=1)
#'     \item Original values: Saved with "_raw" suffix for reference
#'     \item Ignored columns: Left unchanged (outcome variables, totals, etc.)
#'   }
#' @noRd
#' @keywords internal
.stan_factor <- function(df) {
  # find the columns to mutate
  col_names <- setdiff(names(df), .const()$vars$ignore)
  
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
          dtype <- .data_type(vec)
          
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
#' @description Constructs the comprehensive data list required for Stan model fitting,
#' including observation data, design matrices, grouping variables, interaction indices,
#' and poststratification data. Handles both input data for fitting and new data for
#' prediction with proper indexing and formatting.
#'
#' @param input_data Data frame containing observations for model fitting with columns:
#'   \itemize{
#'     \item positive: Number of positive outcomes (binomial family)
#'     \item outcome: Continuous outcome values (normal family)
#'     \item total: Total number of trials/observations
#'     \item Predictor variables as specified in effects structure
#'   }
#' @param new_data Data frame containing population data for prediction/poststratification
#'   with same predictor variables and population counts (total column).
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#'   all model components (fixed, varying, interactions).
#' @param metadata List containing model specifications including family, pstrat_vars,
#'   and is_timevar flag.
#'
#' @return Named list containing all data elements required by Stan:
#'   \itemize{
#'     \item N, N_pop: Sample sizes for input and population data
#'     \item y, n_sample: Outcome variables (family-dependent)
#'     \item X, X_pop: Design matrices for fixed effects
#'     \item K, K_pop: Number of fixed effect columns
#'     \item J_*, N_*: Grouping variables and group sizes for varying effects
#'     \item Poststratification weights and indices
#'     \item sens, spec: Sensitivity and specificity parameters
#'   }
#' @noRd
#' @keywords internal
.make_standata <- function(
    input_data,
    new_data,
    effects,
    metadata
) {

  stan_data <- list(
    N = nrow(input_data),
    N_pop = nrow(new_data),
    sens = if(!is.null(metadata$extra$sens)) metadata$extra$sens else 1,
    spec = if(!is.null(metadata$extra$spec)) metadata$extra$spec else 1
  )

  # outcome variable
  if (metadata$family == "binomial") {
    stan_data$y <- input_data$positive
    stan_data$n_sample <- input_data$total
  } else if (metadata$family == "normal") {
    stan_data$y <- input_data$outcome
  }
  
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
    X_cat <- purrr::map(names(effects$m_fix_c), function(s) {
      ss <- strsplit(s, split = '\\.')[[1]]
      as.integer(dat[[paste0(ss[1], "_raw")]] == ss[2])
    }) %>%
      do.call(cbind, .)
    
    # interaction between fixed effects
    X_int <- purrr::map(names(effects$i_fixsl), function(s) {
      ss <- strsplit(s, split = ':')[[1]]
      return(dat[[ss[1]]] * dat[[ss[2]]])
    }) %>%
      do.call(cbind, .)
    
    # all fixed effects
    X <- cbind(X_cont, X_cat, X_int)
    stan_data[[paste0('X', subfix)]] <- X
    stan_data[[paste0('K', subfix)]] <- ncol(X)

    # varying effects and
    # fixed effects of categorical variables (for structured prior)
    m_fix_c_names <- purrr::map_chr(
      names(effects$m_fix_c),
      function(s) strsplit(s, split = "\\.")[[1]][1]
    ) %>% 
      unique()
    for(s in c(m_fix_c_names,
               names(effects$m_var),
               names(effects$m_var_icar),
               names(effects$m_var_bym2))) {
      stan_data[[stringr::str_interp("N_${s}${subfix}")]] <- n_distinct(dat[[s]])
      stan_data[[stringr::str_interp("J_${s}${subfix}")]] <- dat[[s]]
    }

    
    # interactions
    int <- c(effects$i_varit, effects$i_varits, effects$i_varit_str, effects$i_varits_str)
    for(s in names(int)) {
      ss <- strsplit(s, split = ':')[[1]]
      s <- paste0(ss[1], ss[2])
      int_lvls <- .interaction_levels(dat[[ss[1]]], dat[[ss[2]]])
      unq_int_lvls <- sort(unique(int_lvls))
      n_int_lvls <- length(unq_int_lvls)
      stan_data[[stringr::str_interp("N_${s}${subfix}")]] <- n_int_lvls
      stan_data[[stringr::str_interp("J_${s}${subfix}")]] <- factor(int_lvls, levels = unq_int_lvls, labels = 1:n_int_lvls) %>% as.numeric()
      
      if(name == "input") {
        stan_data[[stringr::str_interp("I_${s}")]] <- unq_int_lvls
      }
    }
  }

  # ICAR graph
  for (s in c(names(effects$m_var_icar))) {
    out <- .build_graph(dat[[paste0(s, "_raw")]], geo_scale = s)
    g <- out$stan_graph[c("N_edges", "node1", "node2")]
    names(g) <- paste0(names(g), "_", s)
    stan_data <- modifyList(stan_data, g)
  }

  # BYM2 graph
  for (s in c(names(effects$m_var_bym2))) {
    out <- .build_graph(dat[[paste0(s, "_raw")]], geo_scale = s)
    g <- out$stan_graph[c("N_edges", "node1", "node2", "N_pos", "R")]
    names(g) <- paste0(names(g), "_", s)
    stan_data <- modifyList(stan_data, g)
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
        group_by(!!!rlang::syms(group_cols)) %>%
        mutate(prop = .data$total / sum(.data$total))
      
      if(s != "overall") {
        stan_data[[stringr::str_interp("N_${s}_pstrat")]] <- n_distinct(pstrat_data[[s]])
        stan_data[[stringr::str_interp("J_${s}_pstrat")]] <- pstrat_data[[s]]
      }
      stan_data[[stringr::str_interp("P_${s}_pstrat")]] <- pop_prop$prop
    }
    
    if(metadata$is_timevar) {
      stan_data$N_time_pstrat <- n_distinct(new_data$time)
      stan_data$J_time_pstrat <- new_data$time
    }
  }

  return(stan_data)
}

#' Run MCMC Sampling for Multilevel Regression Model
#'
#' @description Fits a multilevel regression model using MCMC sampling via Stan.
#' This is the main function for model fitting in the shinymrp package. Generates
#' Stan code, compiles the model, and runs MCMC sampling with the specified
#' parameters.
#'
#' @param input_data Data frame containing preprocessed sample data with columns:
#'   positive (number of successes), total (number of trials), and predictor variables.
#' @param new_data Data frame containing poststratification data with population
#'   counts and the same predictor variables as input_data.
#' @param effects Ungrouped effects structure from .ungroup_effects() specifying
#'   model formula components (fixed effects, varying effects, interactions).
#' @param metadata List containing model specifications including:
#'   \itemize{
#'     \item family: "binomial" or "normal"
#'     \item pstrat_vars: Variables for poststratification
#'     \item is_timevar: Whether data includes time variation
#'   }
#' @param n_iter Integer. Total number of MCMC iterations per chain (default: 1000).
#'   Half are used for warmup, half for sampling.
#' @param n_chains Integer. Number of MCMC chains to run (default: 4).
#'   for COVID models (sens, spec).
#' @param seed Integer. Random seed for reproducible results (default: NULL).
#' @param code_fout Character. File path to save generated Stan code (default: NULL).
#' @param ... Additional arguments passed to the CmdStanR sample() method.
#'
#' @return List containing:
#'   \item{fit}{CmdStanR fit object with MCMC samples}
#'   \item{stan_data}{List of data passed to Stan}
#'   \item{stan_code}{List of generated Stan code for mcmc, ppc, loo, and pstrat}
#' @noRd
#' @keywords internal
.run_mcmc <- function(
    input_data,
    new_data,
    effects,
    metadata,
    n_iter = 1000,
    n_chains = 4,
    seed = NULL,
    code_fout = NULL,
    ...
) {
  .require_cmdstanr_cmdstan()

  stan_code <- list()
  stan_code$mcmc <- .make_stancode_mcmc(effects, metadata)
  stan_code$ppc <- .make_stancode_gq(effects, metadata, "ppc")
  stan_code$loo <- .make_stancode_gq(effects, metadata, "loo")
  stan_code$pstrat <- .make_stancode_gq(effects, metadata, "pstrat")

  stan_data <- .make_standata(input_data, new_data, effects, metadata)

  if(!is.null(code_fout)) {
    writeLines(stan_code$mcmc, code_fout)
  }

  mod_mcmc <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stan_code$mcmc),
    cpp_options = list(stan_threads = TRUE)
  )

  sig_figs <- .get_stan_precision(effects)

  fit <- mod_mcmc$sample(
    data = stan_data,
    iter_warmup = n_iter/2,
    iter_sampling = n_iter/2,
    chains = n_chains,
    parallel_chains = n_chains,
    threads_per_chain = 1,
    refresh = n_iter / 10,
    seed = seed,
    sig_figs = sig_figs,
    ...
  )

  return(list(
    fit = fit,
    stan_data = stan_data,
    stan_code = stan_code
  ))
}

#' Run Generated Quantities for Additional Model Outputs
#'
#' @description Runs generated quantities using fitted MCMC parameters to compute
#' additional model outputs such as posterior predictive checks, leave-one-out
#' cross-validation, or poststratification estimates. Uses the generate_quantities
#' method from CmdStanR.
#'
#' @param fit_mcmc CmdStanR fit object from .run_mcmc() containing MCMC samples.
#' @param stan_code Character string containing Stan code for generated quantities
#'   (from stan_code$ppc, stan_code$loo, or stan_code$pstrat).
#' @param stan_data List of data in Stan format (from .run_mcmc() output).
#' @param n_chains Integer. Number of chains to use for generated quantities
#'   (should match the number used in MCMC fitting).
#'
#' @return CmdStanR generated quantities fit object containing the requested
#'   generated quantities (e.g., y_rep for PPC, log_lik for LOO, theta_pop for MRP).
#' @noRd
#' @keywords internal
.run_gq <- function(
    fit_mcmc,
    stan_code,
    stan_data,
    n_chains
  ) {

  .require_cmdstanr_cmdstan()
  
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

#' Add Reference Levels to Fixed Effects Summary
#'
#' @description Adds reference level information to fixed effects parameter
#' summary tables, ensuring all categorical variable levels are represented.
#' For binary variables, identifies which level corresponds to 1, and for
#' categorical variables, includes the reference level (first alphabetically)
#' with NA values.
#'
#' @param df_fixed Data frame containing fixed effects parameter summaries
#'   with row names corresponding to parameter names.
#' @param effects Ungrouped effects structure from .ungroup_effects() containing
#'   fixed effects specifications.
#' @param input_data Original input data frame used for model fitting, needed
#'   to determine variable types and levels.
#'
#' @return Data frame with expanded row names and structure:
#'   \itemize{
#'     \item Binary variables: Labeled with the level that equals 1
#'     \item Categorical variables: Reference level added with NA parameter values
#'     \item Continuous variables: Unchanged
#'     \item Row names: Updated to include variable.level format
#'   }
#' @noRd
#' @keywords internal
.add_ref_lvl <- function(df_fixed, effects, input_data) {
  ### include reference levels for binary variables
  m_fix_bc_names <- names(effects$m_fix_bc) %>%
    purrr::map_chr(function(s) {
      if (.data_type(input_data[[s]]) == "bin") {
        df <- data.frame(x = input_data[[s]]) %>% .stan_factor()
        eq1 <- unique(df$x_raw[df$x == 1])
        paste0(s, ".", eq1)
      } else {
        s
      }
    })

  row.names(df_fixed) <- c("intercept", c(m_fix_bc_names, names(effects$m_fix_c), names(effects$i_fixsl)))

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

  row_names <- c("intercept", m_fix_bc_names, m_fix_c_names, names(effects$i_fixsl))
  idx <- match(row_names, row.names(df_fixed))
  df_fixed <- df_fixed[idx, ] # NA indices are given rows with NA values
  row.names(df_fixed) <- row_names

  return(df_fixed)
}

#' Extract Parameter Summary Statistics from CmdStanR Fit
#'
#' @description Extracts comprehensive summary statistics for specified parameters
#' from a CmdStanR fit object, including posterior means, standard deviations,
#' quantiles, and convergence diagnostics.
#'
#' @param fit CmdStanR fit object containing MCMC samples.
#' @param variables Character vector of parameter names to summarize.
#' @param probs Numeric vector of quantile probabilities for credible intervals
#'   (default: c(0.025, 0.975) for 95% intervals).
#'
#' @return Data frame with summary statistics for each parameter:
#'   \itemize{
#'     \item mean: Posterior mean
#'     \item sd: Posterior standard deviation
#'     \item quantiles: Specified quantile values
#'     \item rhat: R-hat convergence diagnostic
#'     \item ess_bulk, ess_tail: Effective sample size measures
#'   }
#' @noRd
#' @keywords internal
.get_params_summary <- function(fit, variables, probs = c(0.025, 0.975)) {
  fit$summary(
    variables = variables,
    posterior::default_summary_measures()[1:4],
    quantiles = ~ posterior::quantile2(., probs = probs),
    posterior::default_convergence_measures()
  )
}

#' Format Parameter Summary Table for Display
#'
#' @description Formats parameter summary statistics into a clean table with
#' standardized column names and optional custom row names. Converts technical
#' column names to user-friendly labels suitable for reporting.
#'
#' @param df Data frame containing parameter summary statistics from .get_params_summary().
#' @param row_names Optional character vector of custom row names to replace
#'   default parameter names (default: NULL uses existing row names).
#' @param probs Numeric vector of quantile probabilities used for credible
#'   interval labeling (default: c(0.025, 0.975)).
#'
#' @return Data frame with formatted columns and names:
#'   \itemize{
#'     \item Estimate: Posterior mean
#'     \item Est.Error: Posterior standard deviation
#'     \item l-XX% CI, u-XX% CI: Lower and upper credible interval bounds
#'     \item R-hat: Convergence diagnostic
#'     \item Bulk_ESS, Tail_ESS: Effective sample size measures
#'   }
#' @noRd
#' @keywords internal
.format_params_summary <- function(
    df,
    row_names = NULL,
    probs = c(0.025, 0.975)
) {
  # select relevant columns
  col_names <- c(
    "mean", "sd",
    paste0("q", probs * 100),
    "rhat", "ess_bulk", "ess_tail"
  )
  df <- df %>%
    select(all_of(col_names)) %>%
    as.data.frame()

  # rename columns
  p <- (probs[2] - probs[1]) * 100
  names(df) <- c(
    "Estimate", "Est.Error",
    sprintf("l-%d%% CI", p), sprintf("u-%s%% CI", p),
    "R-hat", "Bulk_ESS", "Tail_ESS"
  )

  if (!is.null(row_names)) {
    row.names(df) <- row_names
  }

  return(df)
}

#' Extract Parameter Estimates from MCMC Fit
#'
#' @description Extracts and formats parameter estimates from MCMC fit object,
#' organizing them into fixed effects, varying effects, and other parameters.
#' Includes reference levels for categorical variables and proper labeling.
#'
#' @param fit CmdStanR fit object from .run_mcmc() containing MCMC samples.
#' @param effects Ungrouped effects structure from .ungroup_effects() used in model fitting.
#' @param input_data Original input data frame used for model fitting, needed
#'   for determining reference levels and variable types.
#' @param metadata List containing model specifications including family type.
#'
#' @return List containing formatted parameter summary tables:
#'   \item{fixed}{Data frame with fixed effects estimates including:
#'     \itemize{
#'       \item intercept and main effects
#'       \item Reference levels for categorical variables
#'       \item Fixed-slope interactions
#'       \item Columns: Estimate, Est.Error, credible intervals, R-hat, ESS
#'     }
#'   }
#'   \item{varying}{Data frame with varying effects standard deviations:
#'     \itemize{
#'       \item Varying intercepts (lambda parameters)
#'       \item Varying slopes (lambda2 parameters)
#'       \item Interaction effects
#'     }
#'   }
#'   \item{other}{Data frame with additional parameters:
#'     \itemize{
#'       \item Residual standard deviation (for normal family)
#'       \item Other model-specific parameters
#'     }
#'   }
#' @noRd
#' @keywords internal
.get_parameters <- function(
  fit,
  effects,
  input_data,
  metadata
) {
  .require_cmdstanr_cmdstan()

  fixed <- c(effects$m_fix_bc, effects$m_fix_c, effects$i_fixsl)
  int_varit <- c(effects$i_varit, effects$i_varits, effects$i_varit_str, effects$i_varits_str)
  int_varsl <- c(effects$i_varsl, effects$i_varsl_str)

  ### fixed main effects & fixed-slope interaction
  df_fixed <- data.frame()
  if(length(fixed) > 0) {
    # extract summary table
    df_fixed <- .get_params_summary(fit, variables = c("intercept", "beta"))

    # format the summary table
    df_fixed <- .format_params_summary(df_fixed)

    # add reference levels
    df_fixed <- .add_ref_lvl(df_fixed, effects, input_data)
  }

  ### varying effects
  df_varying <- data.frame()
  row_names <- c()

  m_var_names <- c(names(effects$m_var),
                   names(effects$m_var_icar),
                   names(effects$m_var_bym2))
  if(length(m_var_names) > 0) {
    df_varying <- rbind(
      df_varying,
      .get_params_summary(fit, variables = paste0("scaled_lambda_", m_var_names))
    )
    row_names <- c(row_names, paste0(m_var_names, " (intercept)"))
  }

  if(length(int_varit) > 0) {
    df_varying <- rbind(
      df_varying, 
      .get_params_summary(fit, variables = paste0("lambda_", gsub(':', '', names(int_varit))))
    )
    row_names <- c(row_names, paste0(names(int_varit), " (intercept)"))
  }

  if(length(int_varsl) > 0) {
    df_varying <- rbind(
      df_varying, 
      .get_params_summary(fit, variables = paste0("lambda2_", gsub(':', '', names(int_varsl))))
    )
    row_names <- c(row_names, paste0(names(int_varsl), " (slope)"))
  }

  if(nrow(df_varying) > 0) {
    df_varying <- .format_params_summary(df_varying, row_names = row_names)
  }


  ### other parameters
  df_other <- data.frame()
  row_names <- c()

  if (metadata$family == "normal") {
    df_other <- rbind(
      df_other,
      .get_params_summary(fit, variables = "sigma")
    )
    row_names <- c(row_names, "Residual SD")
  }

  if (nrow(df_other) > 0) {
    df_other <- .format_params_summary(df_other, row_names = row_names)
  }

  return(list(
    fixed   = df_fixed,
    varying = df_varying,
    other   = df_other
  ))
}

#' Extract MCMC Diagnostics from Fit Object
#'
#' @description Extracts and summarizes MCMC diagnostics including divergent
#' transitions, maximum tree depth hits, and energy Bayesian fraction of missing
#' information (E-BFMI). Provides formatted diagnostic messages for model assessment.
#'
#' @param fit CmdStanR fit object from .run_mcmc() containing MCMC samples.
#' @param total_transitions Integer. Total number of MCMC transitions across
#'   all chains (typically n_iter/2 * n_chains for post-warmup samples).
#' @param max_depth Integer. Maximum tree depth limit used in sampling (default: 10).
#'
#' @return List containing:
#'   \item{summary}{Data frame with diagnostic metrics and formatted messages:
#'     \itemize{
#'       \item Divergence: Number and percentage of divergent transitions
#'       \item Maximum tree depth: Number of transitions hitting depth limit
#'       \item E-BFMI: Number of chains with low energy fraction
#'     }
#'   }
#'   \item{show_warnings}{Logical indicating whether any diagnostic issues were found}
#' @noRd
#' @keywords internal
.get_diagnostics <- function(
  fit,
  total_transitions,
  summarize,
  max_depth = 10
) {
  .require_cmdstanr_cmdstan()

  # Get diagnostic summary
  diag_summary <- fit$diagnostic_summary(quiet = TRUE)

  if (!summarize) {
    return(diag_summary)
  }
  
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
  
  return(summary)
}

#' Extract Poststratification Estimates from Generated Quantities
#'
#' @description Extracts population-level estimates from poststratification
#' generated quantities, organizing them by demographic subgroups and time
#' periods. Provides point estimates and standard errors for each subgroup.
#'
#' @param fit CmdStanR generated quantities fit object from poststratification
#'   (output of .run_gq with pstrat code).
#' @param new_data Data frame containing poststratification data with demographic
#'   variables and population counts.
#' @param metadata List containing model specifications including:
#'   \itemize{
#'     \item pstrat_vars: Character vector of demographic variables for subgroup analysis
#'     \item is_timevar: Logical indicating whether estimates vary over time
#'   }
#' @param interval Confidence interval level for the estimates (default is 0.95)
#'
#' @return Named list of data frames, one for each demographic subgroup plus overall:
#'   \item{overall}{Overall population estimates}
#'   \item{[pstrat_var]}{Estimates by demographic subgroup}
#'
#'   Each data frame contains:
#'   \itemize{
#'     \item factor: Demographic category levels
#'     \item time: Time periods (if is_timevar=TRUE)
#'     \item est: Posterior mean estimate
#'     \item std: Posterior standard deviation
#'   }
#' @noRd
#' @keywords internal
.get_estimates <- function(
  fit,
  new_data,
  metadata,
  interval = 0.95
) {
  .require_cmdstanr_cmdstan()

  out <- .check_interval(interval)
  
  # convert new data to numeric factors
  col_names <- if(metadata$is_timevar) c(metadata$pstrat_vars, "time") else metadata$pstrat_vars
  new_data <- new_data %>%
    select(all_of(col_names)) %>%
    mutate(overall = "overall") %>%  # add placeholder column for overall estimates
    .stan_factor()

  est <- list()

  for(s in c("overall", metadata$pstrat_vars)) {
    # get posterior draws for each subgroup
    pred_mat <- fit$draws(
      variables = stringr::str_interp("theta_${s}_pop"),
      format = "draws_matrix"
    ) %>% t()

    col_names <- if(metadata$is_timevar) c("time", s) else c(s)
    raw_col_names <- paste0(col_names, "_raw")
    new_col_names <- if(metadata$is_timevar) c("time", "factor") else c("factor")

    # Order raw levels based on numeric levels to match order of posterior draws matrix
    est_ <- new_data %>%
      arrange(across(all_of(col_names))) %>%
      distinct(across(all_of(raw_col_names))) %>%
      stats::setNames(new_col_names)

    if (out$is_ci) {
      est[[s]] <- est_ %>%
        mutate(
          est = pred_mat %>% apply(1, mean),
          lower = pred_mat %>% apply(1, stats::quantile, probs = out$qlower),
          upper = pred_mat %>% apply(1, stats::quantile, probs = out$qupper)
        )

    } else {
      est[[s]] <- est_ %>%
        mutate(
          est = pred_mat %>% apply(1, mean),
          std = pred_mat %>% apply(1, stats::sd),
          lower = .data$est - out$n_sd * .data$std,
          upper = .data$est + out$n_sd * .data$std
        ) %>%
        select(-"std")
    }
  }

  return(est)
}

#' Extract Posterior Predictive Replications
#'
#' @description Extracts posterior predictive replications (y_rep) from generated
#' quantities for posterior predictive checking. Aggregates data temporally if
#' time-varying and provides replicated datasets for model validation.
#'
#' @param fit CmdStanR generated quantities fit object from posterior predictive
#'   checks (output of .run_gq with ppc code).
#' @param input_data Original input data frame used for model fitting with
#'   columns: positive, total, and time (if time-varying).
#' @param metadata List containing model specifications including:
#'   \itemize{
#'     \item is_timevar: Logical indicating whether data varies over time
#'     \item family: Distribution family ("binomial" or "normal")
#'   }
#' @param N Integer. Number of posterior draws to extract for replication (default: 10).
#'
#' @return Posterior predictive replications:
#'   \itemize{
#'     \item If is_timevar=FALSE: Numeric vector of overall proportion estimates across N draws
#'     \item If is_timevar=TRUE: Data frame with columns:
#'       \itemize{
#'         \item time: Time periods
#'         \item V1, V2, ..., VN: Proportion estimates for each posterior draw
#'       }
#'   }
#' @noRd
#' @keywords internal
.get_replicates <- function(
  fit,
  input_data,
  metadata,
  N = 10
) {
  .require_cmdstanr_cmdstan()

  # get draws from cmdstanr fit
  yrep_mat <- fit$draws(
    variables = "y_rep",
    format = "draws_matrix"
  )%>% t()
  
  # subset draws
  yrep_mat <- yrep_mat[, sample(ncol(yrep_mat), N)]

  yrep <- if (metadata$is_timevar) {
    yrep_mat %>%
      as.data.frame() %>%
      mutate(
        time = input_data$time,
        total = input_data$total
      ) %>%
      group_by(.data$time) %>%
      summarize_all(sum) %>%
      ungroup() %>%
      mutate(
        across(
          -c(.data$time, .data$total),
          ~ .x / .data$total
        )
      ) %>%
      select(-"total")
  } else {
    colSums(yrep_mat) / sum(input_data$total)
  }

  return(yrep)
}
