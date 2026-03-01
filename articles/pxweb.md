# PX-WEB API Interface for R

This R package provides tools to access [PX-WEB
API](https://www.scb.se/en/services/open-data-api/api-for-the-statistical-database/).
Your [contributions](https://ropengov.org/community/) and [bug reports
and other feedback](https://github.com/ropengov/pxweb) are welcome!

We can find more information on the PX-Web/PC-Axis API
[here](https://www.scb.se/en/services/statistical-programs-for-px-files/px-web/).

## Introduction

PXWEB is an API structure developed by Statistics Sweden and other
national statistical institutions (NSI) to disseminate public statistics
in a structured way. This API enables downloading and using data from
statistical agencies without using a web browser direct over HTTP/HTTPS.

The `pxweb` R package connects any PXWEB API to R and facilitates the
access, use and referencing of data from PXWEB APIs.

### Available data sources and tools

[A number of
organizations](https://www.scb.se/en/services/statistical-programs-for-px-files/px-web/pxweb-examples/)
use PXWEB to distribute hierarchical data. You can browse the available
data sets at:

- [Statistics
  Sweden](http://www.statistikdatabasen.scb.se/pxweb/en/ssd/) with [API
  Description](https://www.scb.se/en/services/open-data-api/api-for-the-statistical-database/)
- [Statistics Finland](https://tilastokeskus.fi/til/aihealuejako.html)
  [StatFi API Description](https://statfin.stat.fi/api1.html)
- [Other organizations using
  PX-WEB](https://www.scb.se/en/services/statistical-programs-for-px-files/px-web/pxweb-examples/)

### About PXWEB APIs

The data in PXWEB APIs consists of metadata and data parts. Metadata is
structured in a hierarchical node tree, where each node contains
information about subnodes. The leaf nodes have information on which the
dimensions are available for the data at that leaf node.

## Installation

To install the latest stable release version from CRAN, just use:

``` r
install.packages("pxweb")
```

To install the latest stable release version from GitHub, just use:

``` r
library("remotes")
remotes::install_github("ropengov/pxweb")
```

Test the installation by loading the library:

``` r
library(pxweb)
```

A tutorial is included with the package with:

``` r
vignette(topic="pxweb")
```

### Installation issues

We also recommend setting the UTF-8 encoding since each API may have
local specific letters:

``` r
Sys.setlocale(locale = "UTF-8")
```

## Accessing PXWEB from R

There are two ways of using the `pxweb` R package to access data, either
interactively or using the core functions. To access data, two parts are
needed, an URL to the data table in the API and a query specifying what
data is of interest.

## Interactive use

The simplest way of using `pxweb` is to use it interactively, navigate
the API to the data of interest, and then set up the query of interest.

``` r
# Navigate through all pxweb api:s in the R package API catalogue
d <- pxweb_interactive()

# Get data from SCB (Statistics Sweden)
d <- pxweb_interactive("api.scb.se")

# Fetching data from statfi (Statistics Finland)
d <- pxweb_interactive("statfin.stat.fi")

# Fetching data from StatBank (Statistics Norway)
d <- pxweb_interactive("data.ssb.no")

# To see all available PXWEB APIs use
pxweb_apis <- pxweb_api_catalogue()
```

In the example above, we use the interactive functionality from the
PXWEB API root, but we could use any path to the API.

``` r
# Start with a specific path.
d <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A")
```

This functionality also means that we can navigate any PXWEB API,
irrespectively of if they are a part of the R package API catalogue or
not. Just supply an URL to somewhere in the API and then navigate the
API from there.

Due to new CRAN policies, it is not possible to use an R function to
edit the API catalogue of the R package, but editing them can be done
quickly from R using
[`file.edit()`](https://rdrr.io/r/utils/file.edit.html).

``` r
file.edit(pxweb_api_catalogue_path())
```

Although, if the `pxweb` is installed again, it will overwrite the old
API catalogue. So the easiest way is to add a PXWEB API to the global
catalogue. To do this, do a pull request at the pxweb GitHub page
[here](https://github.com/rOpenGov/pxweb).

## Direct use

Under the hood, the pxweb package uses the
[`pxweb_get()`](https://ropengov.github.io/pxweb/reference/pxweb_get.md)
function to access data from the PXWEB API. It also keeps track of the
API’s time limits and splits big queries into optimal downloadable
chunks. If we use
[`pxweb_get()`](https://ropengov.github.io/pxweb/reference/pxweb_get.md)
without a query, the function either returns a PXWEB LEVELS object or a
PXWEB METADATA object. What is returned depends on if the URL points to
a table in the API or not. Here is an example of a PXWEB LEVELS object.

``` r
# Get PXWEB levels
px_levels <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/")
px_levels
```

    ## PXWEB LEVELS
    ##   BefolkningNy (t): Population by region, marital status, age and sex.  Year 1968 - 2022
    ##   FolkmangdNov (t): Population 1 November by region, age and sex. Year 2002 - 2023
    ##   FolkmangdDistrikt (t): Population by district, Landscape or Part of the country by sex. Year 2015 - 2022
    ##   BefolkManad (t): Population per month by region, age and sex. Year 2000M01 - 2023M11
    ##   BefolkningR1860N (t): Population by age and sex. Year 1860 - 2022

And if we use
[`pxweb_get()`](https://ropengov.github.io/pxweb/reference/pxweb_get.md)
for a table, a PXWEB METADATA object is returned.

``` r
# Get PXWEB metadata about a table
px_meta <- pxweb_get("https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy")
px_meta
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

### Creating data queries

To download data, we need both the URL to the table and a query
specifying what parts of the table are of interest. An URL to a table is
an URL that will return a metadata object if not a query is supplied.
Creating a query can be done in three main ways. The first and most
straightforward approach is to use
[`pxweb_interactive()`](https://ropengov.github.io/pxweb/reference/pxweb_interactive.md)
to explore the table URL and create a query interactively.

``` r
d <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy")
```

The interactive function will return the query and the URL, even if the
data is not downloaded.

``` r
d$url
```

    ## [1] "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"

``` r
d$query
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

We can also turn the query into a JSON query that we can use outside R.

``` r
pxweb_query_as_json(d$query, pretty = TRUE)
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

The second approach is to specify the query either as an R list or a
JSON object. Some Statistical Agencies, such as Statistics Sweden,
supply queries directly as a JSON object on their web pages. We can use
these queries directly. Below is another example of a JSON query for the
table above. For details on setting up a JSON query, see the PXWEB API
documentation.

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

To use this JSON query, we store the JSON query as a file and supply the
path to the file to the
“[`pxweb_query()`](https://ropengov.github.io/pxweb/reference/pxweb_query.md)”function.

``` r
pxq <- pxweb_query("path/to/the/json/query.json")
```

Finally, we can create a PXWEB query from an R list where each list
element is a variable and selected observation.

``` r
pxweb_query_list <-
  list(
    "Civilstand" = c("*"), # Use "*" to select all
    "Kon" = c("1", "2"),
    "ContentsCode" = c("BE0101N1"),
    "Tid" = c("2015", "2016", "2017")
  )
pxq <- pxweb_query(pxweb_query_list)
pxq
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

We can validate the query against the metadata object to asses that we
can use the query. This validation is done automatically when the data
is fetched with
[`pxweb_get()`](https://ropengov.github.io/pxweb/reference/pxweb_get.md)
but can also be done manually.

``` r
pxweb_validate_query_with_metadata(pxq, px_meta)
```

### Downloading data

When we have the URL to a data table and a query, we can download the
data with
“[`pxweb_get()`](https://ropengov.github.io/pxweb/reference/pxweb_get.md)”.
The function returns a `pxweb_data` object that contains the downloaded
data.

``` r
pxd <- pxweb_get(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy",
  pxq
)
pxd
```

    ## PXWEB DATA
    ## With 4 variables and 24 observations.

If we instead want a JSON-stat object, we change the response format to
JSON-stat, and we will get a JSON-stat object returned.

``` r
pxq$response$format <- "json-stat"
pxjstat <- pxweb_get(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy",
  pxq
)
pxjstat
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
    ##         },
    ##         "extension": {
    ##           "show": ["value"]
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
    ##         },
    ##         "link": {
    ##           "describedby": [
    ##             {
    ##               "extension": {
    ##                 "Kon": ["Kön"]
    ##               }
    ##             }
    ##           ]
    ##         },
    ##         "extension": {
    ##           "show": ["value"]
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
    ##         },
    ##         "extension": {
    ##           "show": ["value"]
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
    ##         },
    ##         "extension": {
    ##           "show": ["code"]
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
    ##     "updated": ["2023-02-09T07:57:00Z"],
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
    ##     ],
    ##     "extension": {
    ##       "px": {
    ##         "infofile": ["BE0101"],
    ##         "tableid": ["TAB638"],
    ##         "decimals": [0]
    ##       }
    ##     }
    ##   }
    ## }

Some return formats return files. Then, these responses are stored in
the R [`tempdir()`](https://rdrr.io/r/base/tempfile.html) folded, and
the file paths are returned by
[`pxweb_get()`](https://ropengov.github.io/pxweb/reference/pxweb_get.md).
Currently, `px` and `sdmx` formats can be downloaded as files, but file
an issue if you need other response formats.

``` r
pxq$response$format <- "px"
pxfp <- pxweb_get(
  "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy",
  pxq
)
pxfp
```

    ## [1] "/var/folders/x9/dsgck_4s5mx2nrzzs8zd64rc0000gq/T//RtmpFdmiD7/50026bd2b2d8df2e3f190ca568b3b587d8207465.px"

If the queries are large (contain more values than the PXWEB API maximum
allowed values), the query is chunked into optimal chunks and is then
downloaded sequentially. PXWEB data objects are then combined into one
large PXWEB data object, while JSON-stat objects are returned as a list
of JSON-stat objects, and other files are stored in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) as separate files.

For more advanced connections to the API, the
[`pxweb_advanced_get()`](https://ropengov.github.io/pxweb/reference/pxweb_advanced_get.md)
gives the flexibility to access the underlying HTTP calls using `httr`
and log the HTTP calls for debugging.

We can then convert the downloaded PXWEB data objects to a `data. frame`
or to a character matrix. The character matrix contains the “raw” data
while `data. frame` returns an R `data.frame` in a tidy format. This
conversion means missing values (such as “..” are converted to `NA`) in
a `data. frame`. Using the arguments `variable.value.type` and
`column.name.type`, we can choose if we want the code or the text column
names and value types.

``` r
pxdf <- as.data.frame(pxd, column.name.type = "text", variable.value.type = "text")
head(pxdf)
```

    ##   marital status   sex year Population
    ## 1         single   men 2015    2762601
    ## 2         single   men 2016    2820248
    ## 3         single   men 2017    2870477
    ## 4         single women 2015    2394842
    ## 5         single women 2016    2437315
    ## 6         single women 2017    2477012

``` r
pxdf <- as.data.frame(pxd, column.name.type = "code", variable.value.type = "code")
head(pxdf)
```

    ##   Civilstand Kon  Tid BE0101N1
    ## 1         OG   1 2015  2762601
    ## 2         OG   1 2016  2820248
    ## 3         OG   1 2017  2870477
    ## 4         OG   2 2015  2394842
    ## 5         OG   2 2016  2437315
    ## 6         OG   2 2017  2477012

Similarly, we can access the raw data as a character matrix with
`as.matrix`.

``` r
pxmat <- as.matrix(pxd, column.name.type = "code", variable.value.type = "code")
head(pxmat)
```

    ##      Civilstand Kon Tid    BE0101N1 
    ## [1,] "OG"       "1" "2015" "2762601"
    ## [2,] "OG"       "1" "2016" "2820248"
    ## [3,] "OG"       "1" "2017" "2870477"
    ## [4,] "OG"       "2" "2015" "2394842"
    ## [5,] "OG"       "2" "2016" "2437315"
    ## [6,] "OG"       "2" "2017" "2477012"

### Access data footnotes/comments

In addition to the data, the PXWEB DATA object may also contain comments
for the data. This can be accessed using
[`pxweb_data_comments()`](https://ropengov.github.io/pxweb/reference/pxweb_data_comments.md)
function.

``` r
pxdc <- pxweb_data_comments(pxd)
pxdc
```

    ## NO PXWEB DATA COMMENTS

In this case, we did not have any comments. If we have comments, we can
turn the comments into a `data. frame` with one comment per row.

``` r
as.data.frame(pxdc)
```

## Citation

Finally, if we use the data, we can easily create a citation for a
`pxweb_data` object using the
[`pxweb_cite()`](https://ropengov.github.io/pxweb/reference/pxweb_cite.md)
function. For full reproducibility, please also cite the package.

``` r
pxweb_cite(pxd)
```

    ## Statistics Sweden (2024). “Population by region, marital status, age,
    ## sex, observations and year.” [Data accessed 2024-01-27 16:19:42.712139
    ## using pxweb R package 0.16.3],
    ## <https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Misc{,
    ##     title = {Population by region, marital status, age, sex, observations and year},
    ##     author = {{Statistics Sweden}},
    ##     organization = {Statistics Sweden},
    ##     address = {Stockholm, Sweden},
    ##     year = {2024},
    ##     url = {https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy},
    ##     note = {[Data accessed 2024-01-27 16:19:42.712139 using pxweb R package 0.16.3]},
    ##   }
    ## Kindly cite the pxweb R package as follows:
    ## 
    ##   Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti
    ##   (rOpenGov).  pxweb: R tools for PXWEB API.  URL:
    ##   http://github.com/ropengov/pxweb
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Misc{,
    ##     title = {pxweb: R tools for PX-WEB API},
    ##     author = {Mans Magnusson and Markus Kainu and Janne Huovari and Leo Lahti},
    ##     year = {2019},
    ##   }

## Known issues and troubleshooting

See
[TROUBLESHOOTING.md](https://github.com/rOpenGov/pxweb/blob/master/TROUBLESHOOTING.md)
for a list of current known issues.

## Licensing

This work can be freely used, modified and distributed under the open
license specified in the [DESCRIPTION
file](https://github.com/rOpenGov/pxweb/blob/master/DESCRIPTION).

## Session info

We created this vignette with

``` r
sessionInfo()
```

    ## R version 4.5.2 (2025-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] pxweb_0.17.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] backports_1.5.0   digest_0.6.39     desc_1.4.3        R6_2.6.1         
    ##  [5] fastmap_1.2.0     xfun_0.56         cachem_1.1.0      knitr_1.51       
    ##  [9] htmltools_0.5.9   rmarkdown_2.30    lifecycle_1.0.5   cli_3.6.5        
    ## [13] sass_0.4.10       pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4  
    ## [17] systemfonts_1.3.1 compiler_4.5.2    tools_4.5.2       ragg_1.5.0       
    ## [21] checkmate_2.3.4   bslib_0.10.0      evaluate_1.0.5    yaml_2.3.12      
    ## [25] jsonlite_2.0.0    rlang_1.1.7       fs_1.6.6          htmlwidgets_1.6.4
