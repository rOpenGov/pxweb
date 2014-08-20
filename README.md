pxweb v0.3.5
=======

## Introduction

pxweb is an R package to interface with the PX-WEB API.

This R package is in a preliminary alpha phase and the code is based
on the very closely related
[sweSCB](https://github.com/rOpenGov/sweSCB) package and the [demo
version of the Statistics Finland
API](http://pxwebapi2.stat.fi/api1.html). For more information, see
[StatFi page](http://www.stat.fi/org/avoindata/api.html). We are now
integrating the generic parts from these packages into a shared API
package. All comments, issues, bug fixes and pull requests are very
welcome.

This package is a part of the international R open government data and
computation project [rOpenGov](http://ropengov.github.io/).

The package offers methods to fetch information about the data
hierarchy stored behind the PX-WEB API; extract metadata; fetch actual
data; and clean up the results.


## A brief note on using the API

The API is a RESTful API. The data consists of a metadata part and a data part. The metadata part is structured in a hierarchical node tree, where each node contains information about any (sub-)nodes that are below it in the tree structure or, if the nodes is at the bottom of the tree structure, the data referenced by the node as well as what dimensions are available for the data at that subnode.

Use the `devtools` package to install the latest version:
```r
library("devtools")
devtools::install_github("pxweb","rOpenGov")
library(pxweb)
```

## Easy access to PX-web data

Various web services host PX-web data. To use the service, you need to
specify the database, version and language. For available options,
use for instance:

```r
# List options
pars <- api_parameters() 
database <- names(pars)[[2]]
version <- pars[[database]]$version
language <- pars[[database]]$lang
print(c(database, version, language))
```


Data in the API is structured in a data tree and a wrapper function `findData()` has been written for easy navigation and access to data through the API. To get data, run the following from the R command line:

```r
# Define the database-specific base URL:
baseURL <- base_url(database = "sweSCB", version = "v1", language = "sv")

# Fetch the data from the specified database:
d <- findData(baseURL)

# Fetch the data from sweSCB (Swedish Statistics bureau)
d <- findData(base_url(database = "sweSCB", version = "v1", language = "sv"))

# Fetch the data from statfi (Statistics Finland)
d <- findData(base_url(database = "statfi", version = "v1", language = "fi"))
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

This package is still in its early development stages. The package can
already be used in its present form to construct a simple menu system,
to mine the PX-web APIs for data, or to discover new data. However,
work is needed to improve usability and widen the range of possible
applications. You are invited to contribute to package development in
any way you can and want to. You will, of course, be given due credit
for your work.

## Reporting bugs

Please use the GitHub issue tracker for reporting bugs and making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the developers easier by submitting the following information along with your bug report:
- The output of `sessionInfo()`
- The output of `packageVersion("pxweb")`

## Open source license

Please note that all source code contained in this project is open source licensed under the Affero Gnu Public License v3. This means that you are allowed to modify, use, and spread the source code freely withoug any permission from the author. HOWEVER, this source code and ANY derivatives thereof MUST be licensed with the same open source license. For further information about the AGPLv3, see LICENSE included with the source code of this package.
