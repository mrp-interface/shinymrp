# shinymrp

<!-- badges: start -->
[![R-CMD-check](https://github.com/mrp-interface/shinymrp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mrp-interface/shinymrp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

### Prerequisites:  C++ Toolchain

shinymrp requires a modern C++ compiler and the GNU Make build utility (a.k.a. “gmake”) for compiling Stan programs. These vary by different operating systems.


#### Linux

On most systems, the GNU Make utility is pre-installed and is the default `make` utility.
There is usually a pre-installed C++ compiler as well, however, it may not be new enough.
To check your machine, run the commands:

```bash
g++ --version
make --version
```

If these are at least at `g++` version 4.9.3 or later and
`make` version 3.81 or later, no additional installations are
necessary. It may still be desirable to update the C++ compiler `g++` because later versions are faster.

A modern C++ compiler and GNU make are bundled into the meta-package `build-essential`,
and can be installed via the command:

```bash
sudo apt-get install build-essential

# then rerun checks
g++ --version
make --version
```

#### Mac

On Mac, the C++ compiler and GNU Make are included with Xcode, the Apple toolset for software developers.
To check if you have the Clang C++ compiler:

```bash
clang --version
```

If this command fails, then install Xcode via the following command

```bash
xcode-select --install
```


#### Windows

For Windows, [RTools](https://cran.r-project.org/bin/windows/Rtools/) is a toolchain bundle that includes the necessary C++ toolchain for compiling Stan programs. Install the appropriate version based on the version of R on your machine.

### shinymrp Installation
R version 4.1.0 or beyond is required. Install the app by executing the R code below:
```
# install remotes if necessary
install.packages('remotes')

# install the app from GitHub using remotes
remotes::install_github('mrp-interface/shinymrp')

# launch the app
shinymrp::run_app()
```


---
*Note: This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.*