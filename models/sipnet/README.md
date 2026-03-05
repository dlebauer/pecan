# PEcAn.SIPNET

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![PEcAn.SIPNET status badge](https://pecanproject.r-universe.dev/badges/PEcAn.SIPNET)](https://pecanproject.r-universe.dev/PEcAn.SIPNET)

<!-- badges: end -->

PEcAn Functions Used for Ecological Forecasts and Reanalysis

## Installation

You can install the development version of `PEcAn.SIPNET` from r-universe like so:

``` r
# Enable repository from pecanproject
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install PEcAn.SIPNET in R
install.packages('PEcAn.SIPNET')
```

Or you can install directly from GitHub with the remotes package like so:

``` r
library(remotes)
install_github('pecanproject/pecan',  subdir = "models/sipnet")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PEcAn.SIPNET)
## basic example code
```

## Runtime / Performance Notes

- `data.table` is a required dependency. `model2netcdf.SIPNET` uses `data.table::fread` for `sipnet.out` parsing.
- `model2netcdf.SIPNET` supports two `sipnet.out` header styles:
  - header on first line
  - `Notes:` preamble line before header
- Internal NetCDF worker default is `max(1, parallel::detectCores() - 1)`.
  - Override with `PECAN_SIPNET_NC_WORKERS=<N>`.
- `conflict=TRUE` requires `cdo` in `PATH`; function fails fast if `cdo` is unavailable.
- `PECAN_SIPNET_PROFILE=1` enables timing summaries (optional CSV via `PECAN_SIPNET_PROFILE_CSV`).
- `PECAN_SIPNET_VERBOSE=1` enables per-run config input logging in `write.config.SIPNET`.
