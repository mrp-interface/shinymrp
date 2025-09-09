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


You can use **shinymrp** in two flexible ways, both available through a single easy installation:

1. Shiny App

The graphical user interface (GUI), built with the Shiny framework, is designed for newcomers and those looking for an interactive, code-free analysis experience.

2. Object-Oriented Programming Interface

Leverage the full flexibility of the exported R6 classes for a programmatic workflow, ideal for advanced users and those integrating MRP into larger R projects.

### Installation 

To get started, install the latest development version from [GitHub](https://github.com/mrp-interface/shinymrp):

```R
# If you don't have 'remotes', install it first:
install.packages('remotes')
remotes::install_github('mrp-interface/shinymrp')
```
### Launch the Shiny App

New to **shinymrp**? We recommend starting with the Shiny app:

```R
shinymrp::run_app()
```

### Import programmatic components

For those experienced with R and object-oriented programming, use the package in scripts or R Markdown documents:

```R
library(shinymrp)
```

## Try the Demo

Explore the **shinymrp** features instantly, no installation required, via our [online demo](https://mrpinterface.shinyapps.io/shinymrp/). 

Need a walkthrough? Watch our step-by-step [video tutorial](https://youtu.be/CUcRYn92fmU?si=EhcAbuwuG2XM-0N0).

## Learn More

For detailed guidance, check our introductory vignette: [Getting started with shinymrp](https://mrp-interface.github.io/shinymrp/articles/getting-started).

> *This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.*
