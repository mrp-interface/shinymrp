# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "shinymrp", # The Name of the package containing the App
  pkg_title = "Point-and-click Interface for MRP", # The Title of the package containing the App
  pkg_description = "A GUI designed for intuitive applications of Multilevel Regression and Poststratification to cross-sectional and spatio-temporal datasets.", # The Description of the package containing the App
  author_first_name = "Toan", # Your First Name
  author_last_name = "Tran", # Your Last Name
  author_email = "tntoan@umich.edu", # Your Email
  repo_url = "https://github.com/trannttoan/shinymrp", # The URL of the GitHub Repo (optional),
  pkg_version = "0.0.1" # The Version of the package containing the App
)

## Set {golem} options ----
golem::set_golem_options()

## Install the required dev dependencies ----
golem::install_dev_deps()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
golem::use_favicon() # path = "path/to/ico". Can be an online file.

