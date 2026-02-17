# Do a GET call to PXWEB API and return a data.frame

Do a GET call to PXWEB API and return a data.frame

## Usage

``` r
pxweb_get_data(
  url,
  query,
  verbose = TRUE,
  column.name.type = "text",
  variable.value.type = "text"
)
```

## Arguments

- url:

  a `pxweb` object or url that can be coherced to a `pxweb` object.

- query:

  a json string, json file or list object that can be coherced to a
  `pxweb_query` object.

- verbose:

  should large queries print out progress.

- column.name.type:

  character: should `code` or `text` be used as column names?

- variable.value.type:

  character: should `code` or `text` be used as values in columns?

## Details

The functions use will do a `pxweb_query` to a PXWEB `url` and return a
`data.frame`. This is a wrapper for the `pxweb_get` function.

## See also

See
[`pxweb_get`](https://ropengov.github.io/pxweb/reference/pxweb_get.md)
for mor general usage and
[`pxweb_query`](https://ropengov.github.io/pxweb/reference/pxweb_query.md)
for details on PXWEB queries.

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_query_example.json"
)
df <- pxweb_get_data(url = url, query = query)
} # }
```
