---
title: "PX-WEB API Interface for R"
author: "Mans Magnusson, Leo Lahti et al."
date: "2018-12-25"
output:
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{pxweb tutorial}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteDepends{Cairo}  
  \usepackage[utf8]{inputenc}  
---


This R package provides tools to access [PX-WEB
API](http://www.scb.se/Grupp/OmSCB/API/API-description.pdf). Your
[contributions](http://ropengov.github.io/contribute/) and [bug
reports and other feedback](https://github.com/ropengov/pxweb) are
welcome!


More information on the PX-Web/PC-Axis API can be found [here](http://www.scb.se/Grupp/OmSCB/API/API-description.pdf).

## Table of contents

[Introduction](#introduction) (Introduction)  
[Installation](#installation) (Installation)  
[Using the PXWEB R package](#usage) (Using PXWEB from R)  

## <a name="introduction"></a>Introduction

PXWEB is an API structure developed by Statistics Sweden together with other national statistical institutions (NSI) to disseminate public statistics in a structured way. This enables downloading and usage of data from statistical agencies without using a web browser direct over HTTP/HTTPS.

The `pxweb` R package connects any PXWEB API to R and hence facilitate the access, use and referencing of data from PXWEB APIs.

### Available data sources and tools

[A number of organizations](http://www.scb.se/sv_/PC-Axis/Programs/PX-Web/PX-Web-examples/) use PXWEB to distribute hierarchical data. You can browse the available data sets at:

 * [Statistics Sweden](http://www.statistikdatabasen.scb.se/pxweb/en/ssd/) with [API Description](http://www.scb.se/Grupp/OmSCB/API/API-description.pdf)
 * [Statistics Finland](http://tilastokeskus.fi/til/aihealuejako.html) [StatFi API Description](http://pxnet2.stat.fi/api1.html)
 * [Other organizations using PX-WEB](http://www.scb.se/sv_/PC-Axis/Programs/PX-Web/PX-Web-examples/)

### About PXWEB APIs

The data in PXWEB APIs consists of a metadata part and a data
part. Metadata is structured in a hierarchical node tree, where each
node contains information about subnodes that are below it in the tree
or, if the nodes are at the bottom of the tree structure, the data
referenced by the node as well as what dimensions are available for
the data at that subnode.

## <a name="installation"></a>Installation

To install the latest stable release version from CRAN, just use:


```r
install.packages("pxweb")
```


To install the latest stable release version from GitHub, just use:


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


### Installation issues

We also recommend setting the UTF-8 encoding since each individual API may have local specificl letters:


```r
Sys.setlocale(locale="UTF-8") 
```


## <a name="usage"></a>Accessing PXWEB from R

There are two ways of using the `pxweb` R package to access data, either interactively of using the core functions. To access data, two parts are needed, an URL to the data table in the API and a query specifying what data is of interest. 

## <a name="interactive"></a>Interactive use

The simplest way of using `pxweb` is to use it interactively and navigate the API to the data of interest and then set up the data query of interest.


```r
# Navigate through all pxweb api:s in the R package API catalogue
d <- pxweb_interactive()

# Get data from SCB (Statistics Sweden)
d <- pxweb_interactive(api = "api.scb.se")

# Fetching data from statfi (Statistics Finland)
d <- interactive_pxweb("pxnet2.stat.fi")

# Fetching data from StatBank (Statistics Norway)
d <- interactive_pxweb("data.ssb.no")

# To see all available PXWEB APIs use
pxweb_apis <- pxweb_api_catalogue()
```

In the example above we use the interactive functionality from the PXWEB API root, but we could use any path to the API.


```r
# Start with a specific path.
d <- pxweb_interactive("http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A")
```

This also means that we can navigate any PXWEB API, irrespectively of if they are a part of the R package API catalog or not. Just supply an URL to somewhere in the API and then navigate the API from there.

Due to new CRAN policies, it is not possible to use an R function to edit the api catalogue of the R package, but editing the can be done easily from R using `file.edit()`.


```r
file.edit(pxweb_api_catalogue_path())
```

Although, if the `pxweb` is installed again, it will overwrite the old api catalogue. So the easiest way is to do add a PXWEB API to the global catalogue. To do this, just do a pull request at the pxweb GitHub page [here](https://github.com/rOpenGov/pxweb).

## <a name="direct"></a>Direct use

Under the hood, the pxweb package uses the `pxweb_get()` function to access data from the PXWEB API. It also keeps track of the time limits of the API and split up to big queries into optimal downloadable chunks. If we use `pxweb_get()` without a query, the function either returns a PXWEB LEVELS object or a PXWEB METADATA object, depending if the URL points to a table in the API or not. Here is an example of a PXWEB LEVELS object.


```r
# Get PXWEB levels
px_levels <- pxweb_get("http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/")
px_levels
```

```
## PXWEB LEVELS
##   BefolkningNy (t): Population by region, marital status, age and sex.  Year 1968 - 2017
##   BefolkningR1860 (t): Population by age and sex. Year 1860 - 2017
##   FolkmangdNov (t): Population 1 November by region, age and sex. Year 2002 - 2018
##   FolkmangdSmaort (t): Population by smaller localities (places with 50-199 inhabitants). Every fifth year 1995 - 2010
##   FolkmangdTatort (t): Population by localities. Every fifth year 1960 - 2017
##   FolkmangdTatortH (t): Population by localities with older/changed names. Every fifth year 1960 - 1980
##   FolkmangdDistrikt (t): Population by district, Landscape or Part of the country by sex. Year 2015 - 2017
```

And if we use `pxweb_get()` for a table, a PXWEB METADATA object is returned.


```r
# Get PXWEB metadata about a table
px_meta <- pxweb_get("http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy")
px_meta
```

```
## PXWEB METADATA
## Population by region, marital status, age, sex, observations and year 
## variables:
##  [[1]] Region: region
##  [[2]] Civilstand: marital status
##  [[3]] Alder: age
##  [[4]] Kon: sex
##  [[5]] ContentsCode: observations
##  [[6]] Tid: year
```

### Creating data queries

To download data we need both the URL to the table and a query specifying what parts of the table are of interest. An URL to a table is an URL that will return a metadata object if not a query is supplied. Creating a query can be done in three main ways. The first and simplest approach is to use `pxweb_interactive()` to explore the table URL and create a query interactively.


```r
d <- pxweb_interactive("http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy")
```

The interactive function will return the query and the url, even if the data is not downloaded.




```r
d$url
```

```
## [1] "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
```

```r
d$query
```

```
## PXWEB QUERY
## query:
##  [[1]] Region (item):
##    00
##  [[2]] Civilstand (item):
##    OG, G, ÄNKL, SK
##  [[3]] Alder (item):
##    tot
##  [[4]] ContentsCode (item):
##    BE0101N1
##  [[5]] Tid (item):
##    2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017
```

We can also turn the query to a json query that can be used outside R.


```r
pxweb_query_as_json(d$query, pretty = TRUE)
```

```
## {
##   "query": [
##     {
##       "code": "Region",
##       "selection": {
##         "filter": "item",
##         "values": ["00"]
##       }
##     },
##     {
##       "code": "Civilstand",
##       "selection": {
##         "filter": "item",
##         "values": ["OG", "G", "ÄNKL", "SK"]
##       }
##     },
##     {
##       "code": "Alder",
##       "selection": {
##         "filter": "item",
##         "values": ["tot"]
##       }
##     },
##     {
##       "code": "ContentsCode",
##       "selection": {
##         "filter": "item",
##         "values": ["BE0101N1"]
##       }
##     },
##     {
##       "code": "Tid",
##       "selection": {
##         "filter": "item",
##         "values": ["2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"]
##       }
##     }
##   ],
##   "response": {
##     "format": "json"
##   }
## }
```


The second approach is to specify the query either as an R list or a JSON object. Some Statistical Agencies, such as Statistics Sweden, supply queries directly as a JSON object on their web pages. These queries can be used directly. Below is another example of a JSON query for the table above. For details on how to set up a JSON query, see the PXWEB API documentation. 

```
{
  "query": [
    {
      "code": "Civilstand",
      "selection": {
        "filter": "item",
        "values": ["OG", "G", "ÄNKL", "SK"]
      }
    },
    {
      "code": "Kon",
      "selection": {
        "filter": "item",
        "values": ["1", "2"]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": ["BE0101N1"]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": ["2015", "2016", "2017"]
      }
    }
  ],
  "response": {
    "format": "json"
  }
} 
```

To use this JSON query we just store the JSON query as a file and supply the path to the file to the ```pxweb_query()``` function.


```r
pxq <- pxweb_query("path/to/the/json/query.json")
```

Finally, we can create a PXWEB query from an R list where each list element is a variable and selected observation.


```r
pxweb_query_list <- 
  list("Civilstand"=c("*"), # Use "*" to select all
       "Kon"=c("1","2"),
       "ContentsCode"=c("BE0101N1"),
       "Tid"=c("2015","2016","2017"))
pxq <- pxweb_query(pxweb_query_list)
pxq
```

```
## PXWEB QUERY
## query:
##  [[1]] Civilstand (all):
##    *
##  [[2]] Kon (item):
##    1, 2
##  [[3]] ContentsCode (item):
##    BE0101N1
##  [[4]] Tid (item):
##    2015, 2016, 2017
```

The query can be validated against the metadata object to asses that the query can be used. This is done automatically when the data is fetched with ```pxweb_get()```, but can also be done manually.


```r
pxweb_validate_query_with_metadata(pxq, px_meta)
```

### Downloading data

When we have the URL to a data table and a query we can simply download the data with ```pxweb_get()```. The function returns a `pxweb_data` object that contains the downloaded data.


```r
pxd <- pxweb_get("http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy",
                 pxq)
pxd
```

```
## PXWEB DATA
## With 4 variables and 24 observations.
```

If we instead want a JSON-stat object, we just change the response format to JSON-stat and we will get a JSON-stat object returned. Only JSON and JSON-stat formats are implemented in the PXWEB API.


```r
pxq$response$format <- "json-stat"
pxjstat <- pxweb_get("http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy",
                     pxq)
pxjstat
```

```
## {
##   "dataset": {
##     "dimension": {
##       "Civilstand": {
##         "label": ["marital status"],
##         "category": {
##           "index": {
##             "OG": [0],
##             "G": [1],
##             "ÄNKL": [2],
##             "SK": [3]
##           },
##           "label": {
##             "OG": ["single"],
##             "G": ["married"],
##             "ÄNKL": ["widowers/widows"],
##             "SK": ["divorced"]
##           }
##         }
##       },
##       "Kon": {
##         "label": ["sex"],
##         "category": {
##           "index": {
##             "1": [0],
##             "2": [1]
##           },
##           "label": {
##             "1": ["men"],
##             "2": ["women"]
##           }
##         }
##       },
##       "ContentsCode": {
##         "label": ["observations"],
##         "category": {
##           "index": {
##             "BE0101N1": [0]
##           },
##           "label": {
##             "BE0101N1": ["Population"]
##           },
##           "unit": {
##             "BE0101N1": {
##               "base": ["number"],
##               "decimals": [0]
##             }
##           }
##         }
##       },
##       "Tid": {
##         "label": ["year"],
##         "category": {
##           "index": {
##             "2015": [0],
##             "2016": [1],
##             "2017": [2]
##           },
##           "label": {
##             "2015": ["2015"],
##             "2016": ["2016"],
##             "2017": ["2017"]
##           }
##         }
##       },
##       "id": [
##         ["Civilstand"],
##         ["Kon"],
##         ["ContentsCode"],
##         ["Tid"]
##       ],
##       "size": [
##         [4],
##         [2],
##         [1],
##         [3]
##       ],
##       "role": {
##         "metric": [
##           ["ContentsCode"]
##         ],
##         "time": [
##           ["Tid"]
##         ]
##       }
##     },
##     "label": ["Population by marital status, sex, observations and year"],
##     "source": ["Statistics Sweden"],
##     "updated": ["2018-12-25T09:38:00Z"],
##     "value": [
##       [2762601],
##       [2820248],
##       [2870477],
##       [2394842],
##       [2437315],
##       [2477012],
##       [1651482],
##       [1672460],
##       [1687016],
##       [1639519],
##       [1657129],
##       [1671381],
##       [99751],
##       [99654],
##       [99682],
##       [345008],
##       [340709],
##       [335961],
##       [417132],
##       [420985],
##       [425487],
##       [540682],
##       [546653],
##       [553226]
##     ]
##   }
## }
```

If the queries are large (contain more values than the PXWEB API maximum allowed values), the query is chunked into optimal chunks and is then downloaded sequentially. PXWEB data objects are then combined to one large PXWEB data object, while JSON-stat objects are returned as a list of JSON-stat objects.

For more advanced connections to the API, the `pxweb_advanced_get()` gives the flexibility to access the underlying HTTP calls using `httr` as well as logging the HTTP calls for debugging.

The downloaded PXWEB data objects can then be converted to either `data.frame`s or to a character matrix. The character matrix contains the "raw" data while the data.frame returns a data.frame for analysis in a tidy format. This means that missing values (such as ".." are converted to `NA`) in a data.frame. Using the arguments `variable.value.type` and `column.name.type` we can also choose if we want the code or the text column names and value types.


```r
pxdf <- as.data.frame(pxd, column.name.type = "text", variable.value.type = "text")
head(pxdf)
```

```
##   marital status   sex year Population
## 1         single   men 2015    2762601
## 2         single   men 2016    2820248
## 3         single   men 2017    2870477
## 4         single women 2015    2394842
## 5         single women 2016    2437315
## 6         single women 2017    2477012
```



```r
pxdf <- as.data.frame(pxd, column.name.type = "code", variable.value.type = "code")
head(pxdf)
```

```
##   Civilstand Kon  Tid BE0101N1
## 1         OG   1 2015  2762601
## 2         OG   1 2016  2820248
## 3         OG   1 2017  2870477
## 4         OG   2 2015  2394842
## 5         OG   2 2016  2437315
## 6         OG   2 2017  2477012
```

In a similar way, we can access the raw data as a character matrix with `as.matrix`.


```r
pxmat <- as.matrix(pxd, column.name.type = "code", variable.value.type = "code")
head(pxmat)
```

```
##      Civilstand Kon Tid    BE0101N1 
## [1,] "OG"       "1" "2015" "2762601"
## [2,] "OG"       "1" "2016" "2820248"
## [3,] "OG"       "1" "2017" "2870477"
## [4,] "OG"       "2" "2015" "2394842"
## [5,] "OG"       "2" "2016" "2437315"
## [6,] "OG"       "2" "2017" "2477012"
```

### Access data footnotes/comments

In addition to the data, the PXWEB DATA object may also contain comments for the data. This can be accessed using `pxweb_data_comments()` function.


```r
pxdc <- pxweb_data_comments(pxd)
pxdc
```

```
## NO PXWEB DATA COMMENTS
```

In this case, we did not have any comments. If we have comments we can turn the comments into a data.frame with one comment per row.


```r
as.data.frame(pxdc)
```

## Citation

Finally, if we use the data, we can easily create a citation for a `pxweb_data` object using the `pxweb_cite()` function. For full reproducibility, please also cite the package.


```r
pxweb_cite(pxd)
```

```
## 
## Statistics Sweden (2018). "Population by region, marital status,
## age, sex, observations and year." [Data accessed 2018-12-25
## 11:38:56 using pxweb R package 0.8.32], <URL:
## http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy>.
## 
## A BibTeX entry for LaTeX users is
## 
##   @Misc{,
##     title = {Population by region, marital status, age, sex, observations and year},
##     author = {{Statistics Sweden}},
##     organization = {Statistics Sweden},
##     address = {Stockholm, Sweden},
##     year = {2018},
##     url = {http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy},
##     note = {[Data accessed 2018-12-25 11:38:56 using pxweb R package 0.8.32]},
##   }
```

```
## 
## Kindly cite the pxweb R package as follows:
## 
##   (C) Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti
##   (rOpenGov 2014-2016).  pxweb: R tools for PXWEB API.  URL:
##   http://github.com/ropengov/pxweb
## 
## A BibTeX entry for LaTeX users is
## 
##   @Misc{,
##     title = {pxweb: R tools for PX-WEB API},
##     author = {Mans Magnusson and Markus Kainu and Janne Huovari and Leo Lahti},
##     year = {2014-2018},
##   }
```




### Known issues

Currently, the `pxweb` package is not thread-safe, and hence it is not safe to runt multiple get functions in parallel or in different R sessions.

## Licensing

This work can be freely used, modified and distributed under the open license specified in the [DESCRIPTION file](https://github.com/rOpenGov/pxweb/blob/master/DESCRIPTION).


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
## [1] pxweb_0.8.32   rmarkdown_1.10 knitr_1.20    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0      digest_0.6.18   rprojroot_1.3-2 R6_2.3.0       
##  [5] jsonlite_1.5    backports_1.1.2 magrittr_1.5    evaluate_0.12  
##  [9] httr_1.3.1      stringi_1.2.4   curl_3.2        checkmate_1.8.5
## [13] tools_3.5.1     stringr_1.3.1   yaml_2.2.0      compiler_3.5.1 
## [17] htmltools_0.3.6
```
