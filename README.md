finSCB v0.3.4
=======

## Introduction

finSCB is an R package to interface with the API of Statistics Finland

This R package is in a preliminary alpha phase and the code is based
on the very closely related
[sweSCB](https://github.com/rOpenGov/sweSCB) package and the [demo
version of the Statistics Finland
API](http://pxwebapi2.stat.fi/api1.html). For more information, see
[StatFi page](http://www.stat.fi/org/avoindata/api.html). We are
aiming to integrate these into a generic API package in the
future. All comments, issues, bug fixes and pull requests are very
welcome.

This package is a part of the international R open government data and computation project [rOpenGov](http://ropengov.github.io/). 

The package offers methods to fetch information about the data hierarchy stored behind the API; extract metadata; fetch actual data; and clean up the results.

## A brief note on using the SCB API

The API is a RESTful API. The data consists of a metadata part and a data part. The metadata part is structured in a hierarchical node tree, where each node contains information about any (sub-)nodes that are below it in the tree structure or, if the nodes is at the bottom of the tree structure, the data referenced by the node as well as what dimensions are available for the data at that subnode.

Use the `devtools` package for easy installation of the latest version from GitHub:
```r
library("devtools")
devtools::install_github("finSCB","rOpenGov")
library(finSCB)
```

## Easy access to SCB data

Data in the SCB API is structured in a data tree and a wrapper function `findSCBdata()` has been written for easy navigation and access to data through the SCB api. To get data from SCB simply run the function from the R command line:
```r
d <- findData()
```
The function will automatically print (if requested) the code needed to reproduce the access to SCB data.


## Advanced access to SCB data (not tested yet)

### Exploring the top node of the API data tree
Data in the SCB API is structured in a data tree. To explore the top node of the data tree, use `scbGetMetadata()`. Provided with no parameters, the function fetches metadata from the top node in the API:
```r
topNode <- scbGetMetadata()
View(topNode)
```

### Traversing the node tree
The node tree can be ascended by adding the id of the next subnode to the URL of the base URL. This id is stored in the "id" column of the topNode object created above. The example below goes down the node tree one level at the third position in the "id" column:

```r
nextNode <- scbGetMetadata(topNode$id[3])
View(nextNode)
```
This can be repeated until we reach a node that references data instead of subnodes. Once you reach the bottom node, `scbGetMetadata` warns the user that the bottom node has been reached.

Note that `scbGetMetadata` actually takes the name of a node as input parameter, so if you know the name of a subnode in the node tree or a bottom (data) node, you can supply this directly to the function instead of explorin the data tree as we did above.

### Getting data dimensions
Next, we want to find the dimensions of the data at a particular bottom node, e.g. the node "KPIFastM" which is the bottom node in the following tree branch:

PR -> PR0101 -> PR0301B -> HMPIM07

The following code fetches dimensions for the variable `HMPIM07`:

```r
bottomNode <- scbGetMetadata("HMPIM07")
dims <- scbGetDims(bottomNode)
```

The first call, `scbGetMetadata("HMPIM07")`, returns a friendly message informing the user that this is a bottom node in the data tree. This call also provides us with a resulting object, `bottomNode`, which contains the URL to the node. We will recycle this when getting 'real' data from the database.

The second call, `scbGetDims(bottomNode)` also prints out a friendly message stating that the dimensions for the data at this node are "ContentsCode" and "Tid". We can now either pass on a wildcard ("*") to these dimensions, or a value, or a range of values on vector form.

To see what values are allowed for each dimension, have a look at the `dims` object using `print(dims)`. You will see that this is actually a list, and the values for each of the dimensions can be found by looking closer at each of the elements in the list by calling `dims[[n]]` where `n` is the number of the dimension you want to look at.

### Getting the data
The information we got above, i.e. the URL to the data node and the dimensions required for the call, can now be used to construct a call to the API to fetch the actual data:
```r
sdata <- scbGetData(bottomNode$URL, list(SPIN2007 = "*", ContentsCode = "PR0301I4", Tid = c("2010M02","2011M03")))
```

The data can now be inspected, e.g. by doing `View(sdata)`.


### Cleaning up the results
Unfortunately, the beta version of the SCB web API often returns faulty formatted data, which can cause a lot of pain. In version 0.3 of finSCB this can be handled directly by setting the parameter `clean = TRUE` in the `scbGetData()` function. So to get cleaned and molten data for the same call as in the previous example, just add `clean = TRUE`:
```r
sdata <- scbGetData(bottomNode$URL, 
  list(SPIN2007 = "*", ContentsCode = "PR0301I4", Tid = c("2010M02","2011M03")),
  clean=TRUE)
```
The data should now be ready for use. 

## A last word of caution
The SCB web API seems to still be in its early stages, and data quality is sometimes not perfect. If you find an obvious error in your data and it's not obvious that this is because of programming errors in `finSCB`, please consider filing a bug report to the developers at SCB. Follow [this link](http://www.scb.se/api) to find information on how to contact them.

## Further examples
Further examples of advanced package usage are included in the "examples" folder installed with this package. To locate this folder on your system, run `system.file(package = "finSCB")` from the R terminal.

## Development information
This package is still in its early development stages. The package can already be used in its present form to construct a simple menu system, to mine the SCB API for data, or to discover new data. However, work is needed to improve usability and widen the range of possible applications. You are invited to contribute to package development in any way you can and want to. You will, of course, be given due credit for your work.

## Reporting bugs
Please use the GitHub issue tracker for reporting bugs and making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the developers easier by submitting the following information along with your bug report:
- The output of `sessionInfo()`
- The output of `packageVersion("finSCB")`

## Open source license
Please note that all source code contained in this project is open source licensed under the Affero Gnu Public License v3. This means that you are allowed to modify, use, and spread the source code freely withoug any permission from the author. HOWEVER, this source code and ANY derivatives thereof MUST be licensed with the same open source license. For further information about the AGPLv3, see LICENSE included with the source code of this package.
