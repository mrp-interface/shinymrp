# shinymrp <a href="https://mrp-interface.github.io/shinymrp/"><img src="man/figures/logo.svg" align="right" height="150" alt="shinymrp website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/mrp-interface/shinymrp/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrp-interface/shinymrp/actions/workflows/ci.yaml)


The shinymrp package provides interfaces for applications of Multilevel Regression and Poststratification to a variety of datasets, from hospital records to political polling data. It scaffolds the steps of an end-to-end Bayesian data analysis workflow.

- **Data cleaning**: Preprocess and display the input data.
- **Descriptive statistics**: Visualize key summary statistics.
- **Model building**: Users can specify and fit models with various predictors and fixed or varying effects. The interface offers detailed diagnostics to evaluate and compare models, facilitating model selection.
- **Result visualization**: Generate graphs to illustrate estimates for the target population and demographic and geographic subgroups for the selected model.

## Getting Started

The package offers two options for executing our MRP workflow: either through a graphical user interface (GUI) created with the Shiny framework or using the exported [R6](https://r6.r-lib.org) classes. If you're new to shinymrp, we recommend starting with the Shiny app. Once you installed the package, you can launch it as follows:

```R
shinymrp::run_app()
```

If you want to test it out without installing the package, you can try the online demo version of the app at [https://mrpinterface.shinyapps.io/shinymrp/](https://mrpinterface.shinyapps.io/shinymrp/). We also provide a [video tutorial](https://youtu.be/CUcRYn92fmU?si=EhcAbuwuG2XM-0N0) that walks you through the app's features and functionalities.

For users who are comfortable with R, particularly those familiar with R6 classes or object-oriented programming, the exported classes can offer more flexibility. To get started, you can check out the [Getting started with shinymrp](https://mrp-interface.github.io/shinymrp/articles/getting-started) vignette for an overview of the workflow.

## Installation

### Installing the R package

Install the latest development version from **GitHub**

```R
# install.packages('remotes')
remotes::install_github('mrp-interface/shinymrp')
```

Vignettes do not come with the installation, but they are always available online at [https://mrp-interface.github.io/shinymrp/articles/](https://mrp-interface.github.io/shinymrp/articles/)

### Installing CmdStanR

The package uses [CmdStanR](https://mc-stan.org/cmdstanr/) to handle the statistical modeling step of MRP, so its prerequisites are required for proper functioning. For more details about the setup, refer to the ["Getting started with shinymrp"](https://mrp-interface.github.io/shinymrp/articles/getting-started) vignette.

> *This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.*
