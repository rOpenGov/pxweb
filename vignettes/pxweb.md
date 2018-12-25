---
title: "PX-WEB API Interface for R"
author: "Mans Magnusson, Leo Lahti et al."
date: "2018-12-02"
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
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.1 LTS
## 
## Matrix products: default
## BLAS: /home/lei/bin/R-3.5.1/lib/libRblas.so
## LAPACK: /home/lei/bin/R-3.5.1/lib/libRlapack.so
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
## [1] pxweb_0.7.0    rmarkdown_1.10 knitr_1.20     usethis_1.4.0 
## [5] devtools_2.0.1
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0        magrittr_1.5      pkgload_1.0.2    
##  [4] R6_2.3.0          rlang_0.3.0.1     httr_1.3.1       
##  [7] stringr_1.3.1     tools_3.5.1       pkgbuild_1.0.2   
## [10] sessioninfo_1.1.0 cli_1.0.1         withr_2.1.2      
## [13] htmltools_0.3.6   remotes_2.0.2     yaml_2.2.0       
## [16] assertthat_0.2.0  digest_0.6.18     rprojroot_1.3-2  
## [19] crayon_1.3.4      processx_3.2.0    callr_3.0.0      
## [22] base64enc_0.1-3   fs_1.2.6          ps_1.2.0         
## [25] testthat_2.0.1    evaluate_0.12     memoise_1.1.0    
## [28] glue_1.3.0        stringi_1.2.4     compiler_3.5.1   
## [31] desc_1.2.0        backports_1.1.2   prettyunits_1.0.2
## [34] jsonlite_1.5
```




