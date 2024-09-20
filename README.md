# shinymrp

## Installation

### Prerequisites:  C++ Toolchain

shinymrp requires a modern C++ compiler and the GNU Make build utility (a.k.a. “gmake”). These vary by operating system.


#### Linux

On most systems the GNU Make utility is pre-installed and is the default `make` utility.
There is usually a pre-installed C++ compiler as well, however, it may not be new enough.
To check your machine, run commands:

```bash
g++ --version
make --version
```

If these are at least at `g++` version 4.9.3 or later and
`make` version 3.81 or later, no additional installations are
necessary. It may still be desirable to update the C++ compiler `g++`, because later versions are faster.

A modern C++ compiler and GNU make are  are bundled into the meta-package `build-essential`,
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

::: {.callout-note}
We don't recommend trying to use the GNU C++ compiler, available via Homebrew,
based on the number of reports of installation difficulties from Mac users on GitHub
as well as the Stan forums.
:::


#### Windows

For Windows, CmdStanR ensures that Windows installations have the appropriate toolchain. Install CmdStanR as well as downstream analysis packages `posterior`, `loo`, `bayesplot`.

In a fresh R session, run this command

```r
install.packages(c("cmdstanr", "posterior", "loo", "bayesplot"),
                           repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
```

Next, use CmdStanR to download, build CmdStan

```r
library(cmdstanr)
install_cmdstan()

# check the installation
cmdstan_version()
cmdstan_path()
```

See CmdStanR documentation:  [https://mc-stan.org/cmdstanr/articles/cmdstanr.html#installing-cmdstan](https://mc-stan.org/cmdstanr/articles/cmdstanr.html#installing-cmdstan)

### shinymrp Installation
R version 4.1.0 or beyond is required. Install the app by executing the R code below:
```
# install remotes if necessary
install.packages('remotes')

# install app from GitHub using remotes
remotes::install_github('mrp-interface/shinymrp')

# launch the app
shinymrp::run_app()
```


---
*Note: This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.*