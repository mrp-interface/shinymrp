# shinymrp: Applying Multilevel Regression and Poststratification in R <a href="https://mrp-interface.github.io/shinymrp/"><img src="man/figures/logo.svg" align="right" height="150" alt="shinymrp website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/mrp-interface/shinymrp/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrp-interface/shinymrp/actions/workflows/ci.yaml)
[![Codecov test coverage](https://codecov.io/gh/mrp-interface/shinymrp/graph/badge.svg)](https://app.codecov.io/gh/mrp-interface/shinymrp)
<!-- badges: end -->


**shinymrp** allows users to apply Multilevel Regression and Poststratification (MRP) methods to a variety of datasets, from electronic health records to sample survey data, through an end-to-end Bayesian data analysis workflow. Whether you’re a researcher, analyst, or data engineer, **shinymrp** provides robust tools for data cleaning, exploratory analysis, flexible model building, and insightful result visualization.


- **Data preparation**: Clean, preprocess and display the input data.
- **Descriptive statistics**: Visualize key summary statistics.
- **Model building**: Specify and fit models with various predictors as fixed or varying effects. Guide your model selection with detailed model diagnostics and comparison metrics.
- **Result visualization**: Generate graphs to convey population-level and subgroup estimates, facilitating interpretation and communication of your findings.

## Getting Started

You can use **shinymrp** in two flexible ways:

### Shiny App

The graphical user interface (GUI), built with the Shiny framework, is designed for newcomers and those looking for an interactive, code-free analysis experience.

Launch the app locally in R with:

```r
shinymrp::run_app()
```

#### Try the Demo

Explore the Shiny app without installation via our [online demo](https://mrpinterface.shinyapps.io/shinymrp/).

Need a walkthrough? Watch our step-by-step [video tutorial](https://youtu.be/CUcRYn92fmU?si=EhcAbuwuG2XM-0N0).

### Object-Oriented Programming Interface

Leverage the full flexibility of the exported R6 classes for a programmatic workflow, ideal for advanced users and those integrating MRP into larger R projects.

Import **shinymrp** in scripts or R Markdown documents just like any other R package:

```r
library(shinymrp)
```

### Installation

To get started, install the latest development version of **shinymrp** from [GitHub](https://github.com/mrp-interface/shinymrp) using `remotes`:

```r
# If 'remotes' is not installed:
install.packages("remotes") 
remotes::install_github("mrp-interface/shinymrp")
```

The package installation does not automatically install all prerequisites. Specifically, **shinymrp** uses [CmdStanR](https://mc-stan.org/cmdstanr/) as the bridge to run [Stan](https://mc-stan.org/), a state-of-the-art platform for Bayesian modeling. Stan requires a modern C++ toolchain (compiler and GNU Make build utility). 

- For setting up your toolchain, see [Stan’s documentation](https://mc-stan.org/docs/cmdstan-guide/installation.html#cpp-toolchain).
- Once ready, follow the [CmdStanR installation instructions](https://mc-stan.org/cmdstanr/articles/cmdstanr.html#installing-cmdstan) to install CmdStanR and CmdStan.


## Learn More

For detailed guidance, check our introductory vignette: [Getting started with shinymrp](https://mrp-interface.github.io/shinymrp/articles/getting-started).

> *This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.*
