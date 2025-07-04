% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data.R
\name{prepare_mrp_custom}
\alias{prepare_mrp_custom}
\title{Prepare MRP data using custom poststratification table}
\usage{
prepare_mrp_custom(
  input_data,
  new_data,
  fips_county_state,
  metadata,
  link_geo = NULL
)
}
\arguments{
\item{input_data}{Data frame containing preprocessed sample data.}

\item{new_data}{Data frame containing custom poststratification data with
demographic columns and population counts (total column).}

\item{fips_county_state}{Data frame containing FIPS code mappings for
geographic name conversion.}

\item{metadata}{List containing analysis metadata including family type
and time-varying flags.}

\item{link_geo}{Character string specifying geographic linking variable
(e.g., "zip", "county", "state"). Must exist in both datasets.}
}
\value{
Named list containing:
  \item{input}{Filtered input data (common geographic areas only)}
  \item{new}{Prepared poststratification data with covariates and time indices}
  \item{levels}{Complete list of factor levels for all variables}
  \item{vars}{Variable lists for model specification (fixed, varying, omit)}
}
\description{
Prepares data for MRP analysis using a custom poststratification
table provided by the user. Filters data to common geographic units, creates
factor levels, appends geographic predictors, and handles time-varying data
by duplicating rows for each time period.
}
\details{
The function:
\enumerate{
  \item Filters both datasets to common geographic areas
  \item Creates factor levels from the filtered data
  \item Appends geographic predictors from input data
  \item Duplicates poststratification rows for each time period
  \item Generates variable lists for modeling
}
}
