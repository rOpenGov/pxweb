---
title: "PX-WEB API Interface for R"
author: "Mans Magnusson, Leo Lahti et al."
date: "2017-09-16"
output:
  - rmarkdown::html_vignette
  - rmarkdown::md_vignette  
vignette: >
  %\VignetteIndexEntry{pxweb}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}  
---


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
library("devtools")
devtools::install_github("ropengov/pxweb")
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
d <- interactive_pxweb(api = "pxnet2.stat.fi")

# Fetching data from StatBank (Statistics Norway)
d <- interactive_pxweb(api = "data.ssb.no")
```

Example of download data from the Statistics Sweden API using `get_pxweb_data()`:


```r
pxweb_test_data <- 
  get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet", 
                 dims = list(ContentsCode = c('PR0101A1'), 
                             Tid = c('*')),
                 clean = FALSE)
```

Example of download data from the Statistics Norway API using `get_pxweb_data()`:


```r
pxweb_test_data_ssb <- 
  get_pxweb_data(url = "http://data.ssb.no/api/v0/en/table/vf/vf01/aksjer/ASKapital",
                 dims = list(
                   NACE2007 = c('A_U'),
                   ContentsCode = c('ASer'),
                   Tid = c('*')
                 ),
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
##   (C) Mans Magnusson, Leo Lahti and Love Hansson (rOpenGov
##   2014-2016).  pxweb: R tools for PX-WEB API.  URL:
##   http://github.com/ropengov/pxweb
## 
## A BibTeX entry for LaTeX users is
## 
##   @Misc{,
##     title = {pxweb: R tools for PX-WEB API},
##     author = {Mans Magnusson and Leo Lahti and Love Hansson},
##     year = {2014-2016},
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
## R version 3.4.1 (2017-06-30)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 17.04
## 
## Matrix products: default
## BLAS: /usr/lib/openblas-base/libblas.so.3
## LAPACK: /usr/lib/libopenblasp-r0.2.19.so
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.17           pxweb_0.6.36         rmarkdown_1.6.0.9001
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.12      digest_0.6.12     rprojroot_1.2    
##  [4] plyr_1.8.4        R6_2.2.2          backports_1.1.0  
##  [7] magrittr_1.5      evaluate_0.10.1   httr_1.3.1       
## [10] stringi_1.1.5     data.table_1.10.4 RJSONIO_1.3-0    
## [13] tools_3.4.1       stringr_1.2.0     yaml_2.1.14      
## [16] compiler_3.4.1    tcltk_3.4.1       htmltools_0.3.6
```




