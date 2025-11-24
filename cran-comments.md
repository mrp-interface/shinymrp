## Resubmission

This is a resubmission addressing CRAN’s feedback.

- Add references to the description field of the DESCRIPTION file
- Reduced package size to less than 5MB
- Removed examples for unexported functions
- Replaced `if (FALSE)` with \donttest{} in examples and gated them with `requireNamespace("cmdstanr", quietly = TRUE)`

## Test environments
- macOS (local): R 4.5.1
- Ubuntu 22.04 (GitHub Actions): R-devel, R-release, R-oldrel-1
- Windows (GitHub Actions): R-release
- macOS (GitHub Actions): R-release

## R CMD check results
0 errors | 0 warnings | 1 note
  Maintainer: ‘Toan Tran <trannttoan97@gmail.com>’
  
  New submission

  Suggests or Enhances not in mainstream repositories:
    cmdstanr
  Availability using Additional_repositories specification:
    cmdstanr   yes   https://stan-dev.r-universe.dev/

## Optional dependency
- The package optionally uses `cmdstanr` (not on CRAN). It is listed in Suggests only.
  All examples/tests/vignettes depending on it are conditionally skipped when unavailable.
