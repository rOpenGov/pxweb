# Do a GET call to PXWEB API

Do a GET call to PXWEB API

## Usage

``` r
pxweb_get(url, query = NULL, verbose = TRUE, output_format = "json-stat2")
```

## Arguments

- url:

  a `pxweb` object or url that can be coherced to a `pxweb` object.

- query:

  a json string, json file or list object that can be coherced to a
  `pxweb_query` object.

- verbose:

  should large queries print out progress.

- output_format:

  output format for PXWEB API v2 data requests. Defaults to
  `"json-stat2"`.

## Value

If `query = NULL`, returns metadata about the API path: a `pxweb_levels`
object for API branches or a `pxweb_metadata` object for data tables. If
`query` is supplied, returns the parsed data response, typically a
`pxweb_data`, `pxweb_data_jsonstat`, `pxweb_data_jsonstat2`, or
`pxweb_data_v2` object depending on the API version and response format.
File-based response formats, such as `"px"` and `"sdmx"`, are written to
a temporary file and the file path is returned. Returns `NULL` if the
URL host cannot be reached.

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

# Get json-stat2 data from StatFin using a downloaded json query
jstat2_url <- "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/vaenn/139e.px"
jstat2_query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "statfin_vaenn_139e_query.json"
)
jstat2_data <- pxweb_get(url = jstat2_url, query = jstat2_query)
} # }
```
