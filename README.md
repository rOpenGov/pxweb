---
output: 
  html_document: 
    keep_md: yes
---




<br>


[![Build Status](https://travis-ci.org/rOpenGov/pxweb.svg?branch=master)](https://travis-ci.org/rOpenGov/pxweb) [![Build status](https://ci.appveyor.com/api/projects/status/40abe0fpxw2jftf3/branch/master?svg=true)](https://ci.appveyor.com/project/MansMeg/pxweb/branch/master)
[![Stories in Ready](https://badge.waffle.io/ropengov/pxweb.png?label=TODO)](http://waffle.io/ropengov/pxweb)
[![Coverage Status](https://coveralls.io/repos/github/rOpenGov/pxweb/badge.svg?branch=master)](https://coveralls.io/github/rOpenGov/pxweb?branch=master)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/pxweb)](https://cran.r-project.org/package=pxweb)
[![Downloads](http://cranlogs.r-pkg.org/badges/pxweb)](https://cran.r-project.org/package=pxweb)
[![Gitter](https://badges.gitter.im/rOpenGov/pxweb.svg)](https://gitter.im/rOpenGov/pxweb?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Watch on GitHub][github-watch-badge]][github-watch]
[![Star on GitHub][github-star-badge]][github-star]
[![Follow](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)

<br>

# PC Axis R tools: pxweb package

<!-- README.md is generated from README.Rmd. Please edit that file -->


The pxweb R package provides tools to interface with the PX-WEB API
for data search, download, manipulation and visualization
purposes. This is used by a large number of statistical authorities
world-wide.  It offers methods to utilize information about the data
hierarchy stored behind the PXWEB API.

Many API services are still in their early stages, and data quality is
sometimes compromised. Issue reports are welcome.

## Installation 

The easiest way to use pxweb is to simply install it from CRAN:

```r
install.packages('pxweb')
```

Alternatively, you can get the latest stable development version:

```r
library(devtools)
install_github('ropengov/pxweb')
```

In some cases, the organization requires manual proxy settings. This can be set as follows:

```r
library(devtools)
library(httr)
set_config(
  use_proxy(url="organizationProxyURL", port=1234)
)
install_github('ropengov/pxweb')
```
 
## Using the package

For examples, check the [tutorial/vignette](https://htmlpreview.github.io/?https://github.com/rOpenGov/pxweb/blob/test/vignettes/pxweb.html).  


# Contributing

You are welcome to contact us:

  * [Submit suggestions and bug reports](https://github.com/ropengov/pxweb/issues) (provide the output of `sessionInfo()` and `packageVersion("pxweb")`)
  * [Send a pull request](https://github.com/ropengov/pxweb/)
  * [Star us on the Github page](https://github.com/ropengov/pxweb)
  * [Join the discussion in Gitter](https://gitter.im/rOpenGov/pxweb)  


### Acknowledgements


**Kindly cite this work** as follows: [Måns Magnusson](https://github.com/mansmeg), Markus Kainu, Janne Huovari, and [Leo Lahti](https://github.com/antagomir). Retrieval and analysis of PC Axis data with the pxweb package. R package version 0.8.32. URL: [http://ropengov.github.io/pxweb](http://ropengov.github.io/pxweb)

We are gratetful to all [contributors](https://github.com/rOpenGov/pxweb/graphs/contributors)! This project is part of [rOpenGov](http://ropengov.github.io).




[github-watch-badge]: https://img.shields.io/github/watchers/ropengov/pxweb.svg?style=social
[github-watch]: https://github.com/ropengov/pxweb/watchers
[github-star-badge]: https://img.shields.io/github/stars/ropengov/pxweb.svg?style=social
[github-star]: https://github.com/ropengov/pxweb/stargazers


