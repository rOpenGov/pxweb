# Do a GET call to PXWEB API

Do a GET call to PXWEB API

## Usage

``` r
pxweb_get(url, query = NULL, verbose = TRUE)
```

## Arguments

- url:

  a `pxweb` object or url that can be coherced to a `pxweb` object.

- query:

  a json string, json file or list object that can be coherced to a
  `pxweb_query` object.

- verbose:

  should large queries print out progress.

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
px_meta_data <- pxweb_get(url)

url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
px_levels <- pxweb_get(url)

url <- "https://api.scb.se/OV0104/v1/doris/sv"
px_levels <- pxweb_get(url)

url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_query_example.json"
)
px_data <- pxweb_get(url = url, query = query)

# Convert to data.frame
as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get raw data
as.matrix(px_data, column.name.type = "code", variable.value.type = "code")

# Get data comments
pxweb_data_comments(px_data)

# Get jsonstat data
jstat <- query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json-stat_query_example.json"
)
jstat_data <- pxweb_get(url = url, query = query)

# Get very large datasets (multiple downloads needed)
big_query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_big_query_example.json"
)
px_data <- pxweb_get(url = url, query = big_query)

# Get json-stat2 data from statfin using downloaded json query
jstat2_url <- "https://pxdata.stat.fi:443/PxWeb/api/v1/fi/StatFin/eot/statfin_eot_pxt_132a.px"
jstat2_query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "sq-api_table_statfin_eot_pxt_132a.px.json"
)
jstat2_data <- pxweb_get(url = jstat2_url, query = jstat2_query)
} # }
```
