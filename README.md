pxweb
=======

[![Build Status](https://api.travis-ci.org/rOpenGov/pxweb.png)](https://travis-ci.org/rOpenGov/pxweb)
[![Stories in Ready](https://badge.waffle.io/ropengov/pxweb.png?label=TODO)](http://waffle.io/ropengov/pxweb)



## Introduction

pxweb is an R package to interface with the PX-WEB API, and it offers
methods to fetch information about the data hierarchy stored behind
the PX-WEB API; extract metadata; fetch actual data; and clean up the
results.

The R package is in a development phase and the code is based on the
[sweSCB](https://github.com/rOpenGov/sweSCB) package and the [demo
version of the Statistics Finland
API](http://pxwebapi2.stat.fi/api1.html). For more information, see
[StatFi page](http://www.stat.fi/org/avoindata/api.html). We are now
integrating the generic parts from these packages into a shared API
package. Comments, issues, bug fixes and pull requests are very
welcome.

The pxweb package is a part of the international R open government
data and computation project [rOpenGov](http://ropengov.github.io/).


## A brief note on using the API

The API is a RESTful API. The data consists of a metadata part and a data part. The metadata part is structured in a hierarchical node tree, where each node contains information about any (sub-)nodes that are below it in the tree structure or, if the nodes is at the bottom of the tree structure, the data referenced by the node as well as what dimensions are available for the data at that subnode.

## Installation

Use the `devtools` package to install the latest version:
```r
library("devtools")
devtools::install_github("pxweb","rOpenGov")
library(pxweb)
```

A tutorial is included with the package with:
```r
vignette(topic="pxweb")
```

## Easy access to PX-web data

Various web services host PX-web data. To use the service, you need to
specify the api, version and language. For available options,
use for instance:

```r
# List options
pars <- api_parameters() 
api <- names(pars)[[2]]
version <- pars[[api]]$version
language <- pars[[api]]$lang
print(c(api, version, language))
```


Data in the API is structured in a data tree and a wrapper function `interactive_pxweb()` has been written for easy navigation and access to data through the API. To get data, run the following from the R command line:

```r
# Define the api-specific base URL:
baseURL <- base_url(api = "api.scb.se", version = "v1", lang = "sv")

# Fetch the data from the specified api:
d <- interactive_pxweb(baseURL)

# Fetch the data from sweSCB (Swedish Statistics bureau)
d <- interactive_pxweb(base_url(api = "api.scb.se", version = "v1", lang = "sv"))

# Fetch the data from statfi (Statistics Finland)
d <- interactive_pxweb(base_url(api = "pxwebapi2.stat.fi", version = "v1", lang = "fi"))
```

The function will also automatically print (if requested) the exact
command to reproduce the access to the selected data set.


## A last word of caution

Many API services seems to still be in their early stages, and data
quality is sometimes not perfect. If you find an obvious error in your
data and it's not obvious that this is because of programming errors
in `pxweb`, please file a bug report to the [developer
community](http://ropengov.github.io/contribute/).

## Development information

The package can be used to construct a simple menu system, to mine the
PX-web APIs for data, or to discover new data. Further work is needed
to improve usability and widen the range of possible applications. You
are invited to contribute to package development in any way you can
and want to. You will, of course, be given due credit for your work.

## Reporting bugs

Please use the GitHub issue tracker for reporting bugs and making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the developers easier by submitting the following information along with your bug report:
- The output of `sessionInfo()`
- The output of `packageVersion("pxweb")`

