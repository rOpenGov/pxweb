<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{pxweb}
-->

PX-WEB API Interface for R
===========

This R package provides tools to access [PX-WEB
API](http://www.scb.se/Grupp/OmSCB/API/API-description.pdf). Your
[contributions](http://ropengov.github.io/contribute/) and [bug
reports and other feedback](https://github.com/ropengov/pxweb) are
welcome!

More information on the PX-Web/PC-Axis API can be found [here](http://www.scb.se/Grupp/OmSCB/API/API-description.pdf).

## Table of contents

[Installation](#installation) (Installation)  
[Examples](#examples) (Examples)  

## Available data sources and tools

[A number of organizations](http://www.scb.se/sv_/PC-Axis/Programs/PX-Web/PX-Web-examples/) use to distribute hierarchical data. You can browse the available data sets at:

* [Statistics Sweden](http://www.statistikdatabasen.scb.se/pxweb/en/ssd/) with [API Description](http://www.scb.se/Grupp/OmSCB/API/API-description.pdf)
* [Statistics Finland](http://tilastokeskus.fi/til/aihealuejako.html) [StatFi API Description](http://pxnet2.stat.fi/api1.html)
* [Other organizations using PX-WEB](http://www.scb.se/sv_/PC-Axis/Programs/PX-Web/PX-Web-examples/)

## <a name="installation"></a>Installation


Install the stable release version in R:


```r
install.packages("pxweb")
```

Install the latest version from github:


```r
install.packages("devtools")
devtools::install_github("pxweb","rOpenGov")
```

Test the installation by loading the library:


```r
library(pxweb)
```

A tutorial is included with the package with:
```r
vignette(topic="pxweb")
```


### Other issues

We also recommend setting the UTF-8 encoding:


```r
Sys.setlocale(locale="UTF-8") 
```

## <a name="examples"></a>Examples

Some examples on using the R tools to fetch px-web API data.

### Fetching data from PX-WEB API:

Interactive API query:


```r
# Navigate through all pxweb api:s installed.
d <- interactive_pxweb()

# Get data from SCB (Statistics Sweden)
d <- interactive_pxweb(api = "api.scb.se")

# Fetching data from the swedish SCB (Statistics Sweden) pxweb API:
d <- interactive_pxweb(api = "api.scb.se", version = "v1", lang = "sv")

# Fetching data from statfi (Statistics Finland)
d <- interactive_pxweb(api = "pxwebapi2.stat.fi")
```


Example of download data from the Statistics Sweden API using `get_pxweb_data()`:


```r
pxweb_test_data <- 
  get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet", 
                 dims = list(ContentsCode = c('PR0101A1'), 
                             Tid = c('*')),
                 clean = FALSE)
```



## Licensing and Citations

This work can be freely used, modified and distributed under the open license specified in the [DESCRIPTION file](https://github.com/rOpenGov/pxweb/blob/master/DESCRIPTION).

Kindly cite the work as follows


```r
citation("pxweb")
```

```
## 
## Kindly cite the pxweb R package as follows:
## 
##   (C) Mans Magnusson, Leo Lahti and Love Hansson (rOpenGov 2014).
##   pxweb: R tools for PX-WEB API.  URL:
##   http://github.com/ropengov/pxweb
## 
## A BibTeX entry for LaTeX users is
## 
##   @Misc{,
##     title = {pxweb: R tools for PX-WEB API},
##     author = {Mans Magnusson and Leo Lahti and Love Hansson},
##     year = {2014},
##   }
```

## About the API

The data in this RESTful API consists of a metadata part and a data
part. Metadata is structured in a hierarchical node tree, where each
node contains information about subnodes that are below it in the tree
or, if the nodes is at the bottom of the tree structure, the data
referenced by the node as well as what dimensions are available for
the data at that subnode.


## Session info

This vignette was created with


```r
sessionInfo()
```

```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] sv_SE.UTF-8/sv_SE.UTF-8/sv_SE.UTF-8/C/sv_SE.UTF-8/sv_SE.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] pxweb_0.5.5
## 
## loaded via a namespace (and not attached):
##  [1] data.table_1.9.2 digest_0.6.4     evaluate_0.5.5   formatR_0.10    
##  [5] htmltools_0.2.4  httr_0.5         jsonlite_0.9.8   knitr_1.6       
##  [9] plyr_1.8.1       Rcpp_0.11.1      RCurl_1.95-4.1   reshape2_1.4    
## [13] RJSONIO_1.2-0.2  rmarkdown_0.3.10 stringr_0.6.2    tools_3.1.2     
## [17] yaml_2.1.13
```




