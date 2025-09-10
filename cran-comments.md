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
  
  Size of tarball: 7031849 bytes

## Optional dependency
- The package optionally uses `cmdstanr` (not on CRAN). It is listed in Suggests only.
  All examples/tests/vignettes depending on it are conditionally skipped when unavailable.
