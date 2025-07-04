% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_model.R
\name{get_estimates}
\alias{get_estimates}
\title{Extract Poststratification Estimates from Generated Quantities}
\usage{
get_estimates(fit, new_data, metadata)
}
\arguments{
\item{fit}{CmdStanR generated quantities fit object from poststratification
(output of run_gq with pstrat code).}

\item{new_data}{Data frame containing poststratification data with demographic
variables and population counts.}

\item{metadata}{List containing model specifications including:
\itemize{
  \item pstrat_vars: Character vector of demographic variables for subgroup analysis
  \item is_timevar: Logical indicating whether estimates vary over time
}}
}
\value{
Named list of data frames, one for each demographic subgroup plus overall:
  \item{overall}{Overall population estimates}
  \item{[pstrat_var]}{Estimates by demographic subgroup}

  Each data frame contains:
  \itemize{
    \item factor: Demographic category levels
    \item time: Time periods (if is_timevar=TRUE)
    \item est: Posterior mean estimate
    \item std: Posterior standard deviation
  }
}
\description{
Extracts population-level estimates from poststratification
generated quantities, organizing them by demographic subgroups and time
periods. Provides point estimates and standard errors for each subgroup.
}
\details{
The function:
\enumerate{
  \item Extracts posterior draws for each demographic subgroup
  \item Computes posterior means and standard deviations
  \item Organizes results by subgroup and time period
  \item Matches factor levels to original data labels
}
}
\examples{
\dontrun{
# Extract poststratification estimates
estimates <- get_estimates(
  fit = pstrat_fit,
  new_data = pstrat_data,
  metadata = model_metadata
)

# View overall estimates
print(estimates$overall)

# View estimates by age group
print(estimates$age)
}
}
