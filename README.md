
<!-- badges: start -->

[![rOG-badge](https://ropengov.github.io/rogtemplate/reference/figures/ropengov-badge.svg)](http://ropengov.org/)
[![R build
status](https://github.com/rOpenGov/pxweb/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/pxweb/actions)
[![codecov](https://codecov.io/gh/rOpenGov/pxweb/branch/master/graph/badge.svg?token=zYtxsus27g)](https://codecov.io/gh/rOpenGov/pxweb)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/pxweb)](https://cran.r-project.org/package=pxweb)
[![Downloads](http://cranlogs.r-pkg.org/badges/pxweb)](https://cran.r-project.org/package=pxweb)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/pxweb)](https://cran.r-project.org/package=pxweb)
[![Gitter](https://badges.gitter.im/rOpenGov/pxweb.svg)](https://gitter.im/rOpenGov/pxweb?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Watch on
GitHub](https://img.shields.io/github/watchers/ropengov/pxweb.svg?style=social)](https://github.com/ropengov/pxweb/watchers)
[![Star on
GitHub](https://img.shields.io/github/stars/ropengov/pxweb.svg?style=social)](https://github.com/ropengov/pxweb/stargazers)
[![Follow](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
[![R-CMD-check](https://github.com/rOpenGov/pxweb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rOpenGov/pxweb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<br>

# R tools to access PX-WEB API - the pxweb R package <a href='https://ropengov.github.io/pxweb/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

The pxweb R package provides tools to interface with the PX-WEB API for
data search, download, manipulation and visualization purposes. This is
used by a large number of statistical authorities world-wide. It offers
methods to utilize information about the data hierarchy stored behind
the PXWEB API.

Many API services are still in their early stages, and data quality is
sometimes compromised. Issue reports are welcome.

## Installation

The easiest way to use pxweb is to simply install it from CRAN:

``` r
install.packages("pxweb")
```

Alternatively, you can get the latest stable development version:

``` r
library(remotes)
remotes::install_github("ropengov/pxweb")
```

In some cases, the organization requires manual proxy settings. This can
be set as follows:

``` r
library(remotes)
library(httr)
set_config(
  use_proxy("64.251.21.73", 8080) # Note! This is an example
)
remotes::install_github("ropengov/pxweb")
```

## Using the package

For examples, check the
[tutorial/vignette](https://ropengov.github.io/pxweb/articles/pxweb.html).

## Problems?

See
[TROUBLESHOOTING.md](https://github.com/rOpenGov/pxweb/blob/master/TROUBLESHOOTING.md)
or [open an issue](https://github.com/ropengov/pxweb/issues).

# Contributing

You are welcome to contact us:

- [Submit suggestions and bug
  reports](https://github.com/ropengov/pxweb/issues) (provide the output
  of `sessionInfo()` and `packageVersion("pxweb")`)
- [Send a pull request](https://github.com/ropengov/pxweb)
- [Star us on the Github page](https://github.com/ropengov/pxweb)
- [Join the discussion in Gitter](https://gitter.im/rOpenGov/pxweb)

### Acknowledgements

``` text
citation("pxweb")
Kindly cite the pxweb R package as follows:

Kindly cite the 'pxweb' R package as follows:

  Magnusson M, Kainu M, Huovari J, Lahti L (2024). _pxweb: R Interface
  to PXWEB APIs_. doi:10.32614/CRAN.package.pxweb
  <https://doi.org/10.32614/CRAN.package.pxweb>, R package version
  0.17.0, <https://github.com/rOpenGov/geofi>.

A BibTeX entry for LaTeX users is

  @Manual{R-pxweb,
    title = {{pxweb: R Interface to PXWEB APIs}},
    doi = {10.32614/CRAN.package.pxweb},
    author = {Mans Magnusson and Markus Kainu and Janne Huovari and Leo Lahti},
    year = {2024},
    version = {0.17.0},
    note = {R package version 0.17.0},
    url = {https://github.com/rOpenGov/geofi},
  }
```

We are grateful to all
[contributors](https://github.com/rOpenGov/pxweb/graphs/contributors)!
This project is part of [rOpenGov](http://ropengov.github.io).
