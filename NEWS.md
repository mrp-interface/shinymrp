# shinymrp 0.5.0

## New Features

* Initial release of shinymrp package
* Point-and-click interface for Multilevel Regression and Poststratification (MRP)
* Interactive Shiny application for intuitive MRP analysis
* Support for both binomial and normal outcome variables
* Time-varying and cross-sectional analysis capabilities
* Geographic visualization with county and state-level mapping
* Data preprocessing and validation tools
* Model fitting with Stan backend via cmdstanr
* Comprehensive plotting and visualization functions
* Integration with American Community Survey (ACS) data

## Data Processing

* Automatic data cleaning and standardization
* Support for multiple file formats (CSV, Excel, SAS)
* Geographic identifier validation and formatting
* Demographic variable recoding and validation
* Missing value imputation using frequency-based sampling

## Modeling Features

* Hierarchical Bayesian modeling with Stan
* Flexible prior specification for model parameters
* Support for structured and unstructured priors
* Interaction effects modeling
* Time series analysis capabilities
* Model diagnostics and validation

## Visualization

* Interactive maps for geographic data exploration
* Time series plots for temporal analysis
* Demographic comparison plots
* Model estimates visualization with uncertainty
* Sample size distribution maps
* Geographic covariate distribution plots

## Technical Details

* Built with Shiny and bslib for modern UI/UX
* Efficient data handling with dplyr and tidyr
* High-quality plotting with ggplot2 and highcharter
* Robust statistical computing with posterior and loo packages
* Comprehensive testing framework with testthat