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

We also recommend setting the UTF-8 encoding:


```r
Sys.setlocale(locale="UTF-8") 
```

## <a name="examples"></a>Examples

Some examples on using the R tools to fetch px-web API data.

### Listing available database parameters


```r
library(pxweb)
print(api_parameters())
```

```
## $statfi
## $statfi$url
## [1] "http://pxnet2.stat.fi/PXWeb/api/[version]/[lang]/StatFin"
## 
## $statfi$version
## [1] "v1"
## 
## $statfi$lang
## [1] "fi"
## 
## $statfi$calls_per_period
## [1] 30
## 
## $statfi$period_in_seconds
## [1] 10
## 
## $statfi$max_values_to_download
## [1] 1e+05
## 
## 
## $pxnet2.stat.fi
## $pxnet2.stat.fi$url
## [1] "http://pxnet2.stat.fi/PXWeb/api/[version]/[lang]/StatFin"
## 
## $pxnet2.stat.fi$version
## [1] "v1"
## 
## $pxnet2.stat.fi$lang
## [1] "fi"
## 
## $pxnet2.stat.fi$calls_per_period
## [1] 30
## 
## $pxnet2.stat.fi$period_in_seconds
## [1] 10
## 
## $pxnet2.stat.fi$max_values_to_download
## [1] 1e+05
## 
## 
## $pxwebapi2.stat.fi
## $pxwebapi2.stat.fi$url
## [1] "http://pxwebapi2.stat.fi/PXWeb/api/[version]/[lang]/StatFin"
## 
## $pxwebapi2.stat.fi$version
## [1] "v1"
## 
## $pxwebapi2.stat.fi$lang
## [1] "fi"
## 
## $pxwebapi2.stat.fi$calls_per_period
## [1] 30
## 
## $pxwebapi2.stat.fi$period_in_seconds
## [1] 10
## 
## $pxwebapi2.stat.fi$max_values_to_download
## [1] 1e+05
## 
## 
## $scb
## $scb$url
## [1] "http://api.scb.se/OV0104/[version]/doris/[lang]/ssd"
## 
## $scb$version
## [1] "v1"
## 
## $scb$lang
## [1] "sv" "en"
## 
## $scb$calls_per_period
## [1] 30
## 
## $scb$period_in_seconds
## [1] 10
## 
## $scb$max_values_to_download
## [1] 1e+05
## 
## 
## $api.scb.se
## $api.scb.se$url
## [1] "http://api.scb.se/OV0104/[version]/doris/[lang]/ssd"
## 
## $api.scb.se$version
## [1] "v1"
## 
## $api.scb.se$lang
## [1] "sv" "en"
## 
## $api.scb.se$calls_per_period
## [1] 30
## 
## $api.scb.se$period_in_seconds
## [1] 10
## 
## $api.scb.se$max_values_to_download
## [1] 1e+05
## 
## 
## $foo.bar
## $foo.bar$url
## [1] "http://foo.bar/[version]/[lang]"
## 
## $foo.bar$version
## [1] "v1"
## 
## $foo.bar$lang
## [1] "sv" "en"
## 
## $foo.bar$calls_per_period
## [1] 1
## 
## $foo.bar$period_in_seconds
## [1] 2
## 
## $foo.bar$max_values_to_download
## [1] 1000
```

### Fetching data from [Statistics Finland](http://www.stat.fi/org/avoindata/api.html) PX-WEB API:

Interactive API query (not run):


```r
baseURL <- base_url("statfi", "v1", "fi")
d <- interactive_pxweb(baseURL)
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
## [1] pxweb_0.4.22 knitr_1.6   
## 
## loaded via a namespace (and not attached):
##  [1] data.table_1.9.2 evaluate_0.5.5   formatR_1.0      httr_0.5        
##  [5] plyr_1.8.1       Rcpp_0.11.2      reshape2_1.4     RJSONIO_1.3-0   
##  [9] stringr_0.6.2    tools_3.1.1
```




