% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_model.R
\name{get_parameters}
\alias{get_parameters}
\title{Extract Parameter Estimates from MCMC Fit}
\usage{
get_parameters(fit, effects, input_data, metadata)
}
\arguments{
\item{fit}{CmdStanR fit object from run_mcmc() containing MCMC samples.}

\item{effects}{Ungrouped effects structure from ungroup_effects() used in model fitting.}

\item{input_data}{Original input data frame used for model fitting, needed
for determining reference levels and variable types.}

\item{metadata}{List containing model specifications including family type.}
}
\value{
List containing formatted parameter summary tables:
  \item{fixed}{Data frame with fixed effects estimates including:
    \itemize{
      \item Intercept and main effects
      \item Reference levels for categorical variables
      \item Fixed-slope interactions
      \item Columns: Estimate, Est.Error, credible intervals, R-hat, ESS
    }
  }
  \item{varying}{Data frame with varying effects standard deviations:
    \itemize{
      \item Varying intercepts (lambda parameters)
      \item Varying slopes (lambda2 parameters)
      \item Interaction effects
    }
  }
  \item{other}{Data frame with additional parameters:
    \itemize{
      \item Residual standard deviation (for normal family)
      \item Other model-specific parameters
    }
  }
}
\description{
Extracts and formats parameter estimates from MCMC fit object,
organizing them into fixed effects, varying effects, and other parameters.
Includes reference levels for categorical variables and proper labeling.
}
\details{
The function:
\enumerate{
  \item Extracts parameter summaries with convergence diagnostics
  \item Formats column names and adds reference levels
  \item Organizes parameters by type (fixed, varying, other)
  \item Provides proper labeling for categorical variables
}
}
\examples{
\dontrun{
# Extract parameter estimates
params <- get_parameters(
  fit = mcmc_result$fit,
  effects = model_effects,
  input_data = processed_data,
  metadata = model_metadata
)

# View fixed effects
print(params$fixed)

# View varying effects
print(params$varying)
}
}
