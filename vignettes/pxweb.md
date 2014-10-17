<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{pxweb}
-->

PX-WEB API Interface for R
===========

This R package provides tools to access [PX-WEB
API](http://www.scb.se/Grupp/OmSCB/API/API-description.pdf). Your
[contributions](http://ropengov.github.com/contact.html) and [bug
reports and other feedback](https://github.com/ropengov/pxweb) are
welcome!



## Available data sources and tools

[Installation](#installation) (Installation)  
[Examples](#examples) (Examples)  

[A number of organizations](http://www.scb.se/sv_/PC-Axis/Programs/PX-Web/PX-Web-examples/) use to distribute hierarchical data. You can browse the available data sets at:

* [SCB](Source: [SCB](http://www.statistikdatabasen.scb.se/pxweb/en/ssd/) (Statistics Sweden)
* [Statistics Finland](http://tilastokeskus.fi/til/aihealuejako.html) (Statistics Finland)
* [Other organizations with PX-WEB API](http://www.scb.se/sv_/PC-Axis/Programs/PX-Web/PX-Web-examples/)

## <a name="installation"></a>Installation


Install the stable release version in R:


```r
install.packages("pxweb")
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

### Listing available database parameters


```r
library(pxweb)
print(api_parameters("statfi"))
```

```
## [[1]]
## api:         statfi
## limit(s):    30 calls per 10 sec. 
##              Max 100000 values per call.
## version(s):  v1 
## language(s): fi 
## base url:
##  http://pxnet2.stat.fi/PXWeb/api/[version]/[lang]/StatFin
```

### Fetching data from [Statistics Finland](http://www.stat.fi/org/avoindata/api.html) PX-WEB API:

Interactive API query (not run):


```r
# Define the api-specific base URL:
baseURL <- base_url(api = "api.scb.se", version = "v1", lang = "sv")
d <- interactive_pxweb(baseURL)

# Fetching data from SCB (Statistics Sweden)
d <- interactive_pxweb(base_url(api = "scb", version = "v1", lang = "sv"))

# Fetching data from statfi (Statistics Finland)
d <- interactive_pxweb(base_url(api = "statfi", version = "v1", lang = "fi"))
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
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-pc-linux-gnu (64-bit)
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
## [1] pxweb_0.4.23 knitr_1.6   
## 
## loaded via a namespace (and not attached):
##  [1] data.table_1.9.2 evaluate_0.5.5   formatR_1.0      httr_0.5        
##  [5] plyr_1.8.1       Rcpp_0.11.2      reshape2_1.4     RJSONIO_1.3-0   
##  [9] stringr_0.6.2    tools_3.1.1
```




