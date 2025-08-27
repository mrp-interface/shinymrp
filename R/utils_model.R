#' Normalize Interaction Pair Order
#'
#' @description Standardizes the order of variables in interaction pairs by
#' sorting them alphabetically. Ensures "a:b" and "b:a" are treated as equivalent
#' by converting both to the same canonical form.
#'
#' @param p Character string representing an interaction pair (e.g., "age:gender").
#' @param sep Character separator used in the interaction pair (default ":").
#'
#' @return Character string with variables sorted alphabetically within the pair.
#'   For example, "gender:age" becomes "age:gender".
#' @noRd
# helper to normalize a single "a:b" → "a:b" or "b:a" → "a:b"
.norm_pair <- function(p, sep = ":") {
  parts <- strsplit(p, sep, fixed = TRUE)[[1]]
  paste(sort(parts), collapse = sep)
}

#' Find Intersection of Interaction Pairs
#'
#' @description Computes the intersection between two sets of interaction pairs,
#' treating "a:b" and "b:a" as equivalent by normalizing pair order before comparison.
#' Useful for finding common interaction terms between different model specifications.
#'
#' @param pairs1 Character vector of interaction pairs (e.g., c("age:gender", "income:education")).
#' @param pairs2 Character vector of interaction pairs to intersect with pairs1.
#' @param sep Character separator used in interaction pairs (default ":").
#'
#' @return Character vector containing normalized interaction pairs that appear
#'   in both pairs1 and pairs2, accounting for order-invariant matching.
#' @noRd
.pair_intersect <- function(pairs1, pairs2, sep = ":") {
  # normalize pairs
  norms1 <- vapply(pairs1, .norm_pair, FUN.VALUE = character(1), sep = sep)
  norms2 <- vapply(pairs2, .norm_pair, FUN.VALUE = character(1), sep = sep)

  return(intersect(norms1, norms2))
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
#' @noRd
.pair_setdiff <- function(pairs1, pairs2, sep = ":") {
  # precompute the normalized sets of pairs
  norm1 <- vapply(pairs1, .norm_pair, FUN.VALUE = character(1), sep = sep)
  norm2 <- vapply(pairs2, .norm_pair, FUN.VALUE = character(1), sep = sep)

  # keep those in pairs1 whose normalized form is NOT in norm2
  keep <- !norm1 %in% norm2

  return(pairs1[keep])
}


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
#' @noRd
.filter_interactions <- function(interactions, fixed_effects, data) {
  bool <- purrr::map_lgl(interactions, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- .data_type(data[[ss[1]]])
    type2 <- .data_type(data[[ss[2]]])
    is_cat <- c(type1, type2) == "cat"

    any(is_cat) && all(!ss[is_cat] %in% fixed_effects)
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
#' @noRd
.sort_interactions <- function(interactions, dat) {
  interactions <- purrr::map_chr(interactions, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    type1 <- .data_type(dat[[ss[1]]], num = TRUE)
    type2 <- .data_type(dat[[ss[2]]], num = TRUE)
    
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
#' @importFrom rlang .data
.create_interactions <- function(fixed_effects, varying_effects, dat) {
  main_effects <- c(fixed_effects, varying_effects)
  
  if(dplyr::n_distinct(main_effects) <= 1) {
    return(list())
  }
  
  # create unique pairs
  df <- expand.grid(
    eff1 = main_effects,
    eff2 = main_effects,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(.data$eff1 != .data$eff2)

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
.interaction_levels <- function(levels1, levels2) {
  numcat1 <- dplyr::n_distinct(levels1)
  numcat2 <- dplyr::n_distinct(levels2)

  if(numcat1 == 2 | numcat2 == 2) {
    levels_interaction <- levels1 * levels2
    levels_interaction[levels_interaction == 0] <- 1
  } else {
    levels_interaction <- (levels1 - 1) * numcat2 + levels2
  }
  
  return(levels_interaction)
}

#' Check MCMC iteration and chain parameters
#'
#' @description Validates MCMC sampling parameters including number of iterations,
#' chains, and seed values to ensure they fall within acceptable ranges and are
#' of correct numeric types.
#'
#' @param n_iter Numeric. Number of MCMC iterations to validate
#' @param n_iter_range Numeric vector of length 2. Minimum and maximum allowed iterations
#' @param n_chains Numeric. Number of MCMC chains to validate
#' @param n_chains_range Numeric vector of length 2. Minimum and maximum allowed chains
#' @param seed Numeric. Random seed value to validate
#'
#' @return A list containing:
#'   \item{valid}{Logical indicating if all parameters are valid}
#'   \item{msg}{Character vector of validation error messages, empty if valid}
#'
#' @noRd
.check_iter_chain <- function(n_iter, n_iter_range, n_chains, n_chains_range, seed) {
  flag <- TRUE
  msg <- c()
  
  if(is.numeric(n_iter) && is.numeric(n_chains) && is.numeric(seed)) {
    if(n_iter < n_iter_range[1] | n_iter > n_iter_range[2]) {
      msg <- c(msg, paste0("The number of iterations must be between ", n_iter_range[1], " and ", n_iter_range[2], "."))
      flag <- FALSE
    }
    
    if(n_chains < n_chains_range[1] | n_chains > n_chains_range[2]) {
      msg <- c(msg, paste0("The number of chains must be between ", n_chains_range[1], " and ", n_chains_range[2], "."))
      flag <- FALSE
    }
  } else {
    flag <- FALSE
    
    if(!is.numeric(n_iter)) {
      msg <- c(msg, "The number of iterations must be a numeric value.")
    }
    
    if(!is.numeric(n_chains)) {
      msg <- c(msg, "The number of chains must be a numeric value.")
    }
    
    if(!is.numeric(seed)) {
      msg <- c(msg, "The seed must be a numeric value.")
    }
  }
  return(list(
    valid = flag, 
    msg = msg
  ))
}

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
#' @noRd
.create_formula <- function(effects) {

  m_fix_c <- names(effects$m_fix_c) %>%
    purrr::map_chr(function(s) strsplit(s, "\\.")[[1]][1]) %>%
    unique()

  fixed <- c(names(effects$m_fix_bc), m_fix_c, names(effects$i_fixsl))
  varsl <- c(names(effects$i_varsl), names(effects$s_varsl))
  varit <- c(names(effects$m_var), names(effects$i_varit), names(effects$i_varits), names(effects$s_varit), names(effects$s_varits))
  

  s_fixed <- if(length(fixed) > 0) paste(paste0(" + ", fixed), collapse = '') else ''
  s_varit <- if(length(varit) > 0) paste(paste0(" + (1 | ", varit, ")"), collapse = '') else ''
  s_varsl <- if(length(varsl) > 0) paste(purrr::map(varsl, function(s) {
    ss <- strsplit(s, split = ':')[[1]]
    return(paste0(" + (0 + ", ss[2], " | ", ss[1], ')'))
  }), collapse = '') else ''


  formula <- paste0("1", s_fixed, s_varit, s_varsl)

  return(formula)
}

#' Check for Divergent MCMC Samples
#'
#' @description Identifies if any divergent transitions occurred during MCMC sampling.
#' Divergent transitions can indicate issues with model specification or sampling.
#'
#' @param diagnostics A list containing MCMC diagnostic information, including
#'   the number of divergent transitions.
#'
#' @return Logical indicating if any divergent transitions were detected.
#' 
#' @noRd
.check_divergence <- function(diagnostics) {
  return(sum(diagnostics$num_divergent) > 0)
}