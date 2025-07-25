# shinymrp <a href="https://mrp-interface.github.io/shinymrp/"><img src="man/figures/logo.svg" align="right" height="150" alt="shinymrp website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/mrp-interface/shinymrp/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrp-interface/shinymrp/actions/workflows/ci.yaml)
[![Codecov test coverage](https://codecov.io/gh/mrp-interface/shinymrp/graph/badge.svg)](https://app.codecov.io/gh/mrp-interface/shinymrp)
<!-- badges: end -->

The shinymrp package provides interfaces for applications of Multilevel Regression and Poststratification to a variety of datasets, from hospital records to political polling data. It scaffolds the steps of an end-to-end Bayesian data analysis process.

- **Data cleaning**: Preprocess and display the input data.
- **Descriptive statistics**: Visualize key summary statistics.
- **Model building**: Users can specify and fit models with various predictors and fixed or varying effects. The interface offers detailed diagnostics to evaluate and compare models, facilitating model selection.
- **Result visualization**: Generate graphs to illustrate estimates for the target population and demographic and geographic subgroups for the selected model.


## Getting Started

The package offers two options for executing the workflow: either through a graphical user interface (GUI) created with Shiny or through the programmatic interface (API). The latter is for users who are comfortable with R, particularly those familiar with the R6 class and object-oriented programming. If you're new to shinymrp, we recommend starting with the Shiny app.

- [Getting started with shinymrp](https://mrp-interface.github.io/shinymrp/articles/getting-started)
- [Shiny app tutorial video]()
- [Try the live demo Shiny app](https://mrpinterface.shinyapps.io/shinymrp/)

## Installation

### Installing the R package

Install the latest development version from **GitHub**

```R
# install.packages('remotes')
remotes::install_github('mrp-interface/shinymrp')
```

Vignettes do not come with the installation, but they are always available online at [https://mrp-interface.github.io/shinymrp/articles/](https://mrp-interface.github.io/shinymrp/articles/)

### Installing CmdStan

The package uses [CmdStanR](https://mc-stan.org/cmdstanr/) to handle the statistical modeling step of MRP, so its prerequisite, [CmdStan](https://mc-stan.org), is required. For more details about the setup, refer to the ["Getting started with shinymrp"](https://mrp-interface.github.io/shinymrp/articles/getting-started) vignette.

> *This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.*
