# Validate a `pxweb_query` with a `pxweb_metadata` object

Validate a `pxweb_query` with a `pxweb_metadata` object

## Usage

``` r
pxweb_validate_query_with_metadata(pxq, pxmd)
```

## Arguments

- pxq:

  a `pxweb_query` object.

- pxmd:

  a `pxweb_metadata` object.

## Details

Validate a query with a metadata object to asses that the query can be
used to query the table.

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
json_query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_query_example.json"
)
pxq <- pxweb_query(json_query)
pxweb_validate_query_with_metadata(pxq, pxweb_get(url))
} # }
```
