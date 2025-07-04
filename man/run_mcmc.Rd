% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_model.R
\name{run_mcmc}
\alias{run_mcmc}
\title{Run MCMC Sampling for Multilevel Regression Model}
\usage{
run_mcmc(
  input_data,
  new_data,
  effects,
  metadata,
  n_iter = 1000,
  n_chains = 4,
  extra = NULL,
  seed = NULL,
  code_fout = NULL,
  silent = FALSE
)
}
\arguments{
\item{input_data}{Data frame containing preprocessed sample data with columns:
positive (number of successes), total (number of trials), and predictor variables.}

\item{new_data}{Data frame containing poststratification data with population
counts and the same predictor variables as input_data.}

\item{effects}{Ungrouped effects structure from ungroup_effects() specifying
model formula components (fixed effects, varying effects, interactions).}

\item{metadata}{List containing model specifications including:
\itemize{
  \item family: "binomial" or "normal"
  \item pstrat_vars: Variables for poststratification
  \item is_timevar: Whether data includes time variation
}}

\item{n_iter}{Integer. Total number of MCMC iterations per chain (default: 1000).
Half are used for warmup, half for sampling.}

\item{n_chains}{Integer. Number of MCMC chains to run (default: 4).}

\item{extra}{Optional list containing sensitivity and specificity parameters
for COVID models (sens, spec).}

\item{seed}{Integer. Random seed for reproducible results (default: NULL).}

\item{code_fout}{Character. File path to save generated Stan code (default: NULL).}

\item{silent}{Logical. Whether to suppress Stan output messages (default: FALSE).}
}
\value{
List containing:
  \item{fit}{CmdStanR fit object with MCMC samples}
  \item{stan_data}{List of data passed to Stan}
  \item{stan_code}{List of generated Stan code for mcmc, ppc, loo, and pstrat}
}
\description{
Fits a multilevel regression model using MCMC sampling via Stan.
This is the main function for model fitting in the shinymrp package. Generates
Stan code, compiles the model, and runs MCMC sampling with the specified
parameters.
}
\details{
The function:
\enumerate{
  \item Generates Stan code for MCMC sampling and generated quantities
  \item Prepares data in Stan-compatible format
  \item Compiles the Stan model with threading support
  \item Runs MCMC sampling with specified parameters
  \item Returns fit object and associated data/code for further analysis
}
}
\examples{
\dontrun{
# Fit a basic multilevel model
result <- run_mcmc(
  input_data = processed_data,
  new_data = pstrat_data,
  effects = model_effects,
  metadata = model_metadata,
  n_iter = 2000,
  n_chains = 4
)
}
}
