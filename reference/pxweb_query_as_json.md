# Convert a `pxweb_query` object to a `json` string

Convert a `pxweb_query` object to a `json` string

## Usage

``` r
pxweb_query_as_json(pxq, ...)
```

## Arguments

- pxq:

  a `pxweb_query` object.

- ...:

  further argument to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

## See also

[`pxweb_query`](https://ropengov.github.io/pxweb/reference/pxweb_query.md),
[`pxweb_query_as_rcode`](https://ropengov.github.io/pxweb/reference/pxweb_query_as_rcode.md)

## Examples

``` r
json_query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_query_example.json"
)
pxq <- pxweb_query(json_query)
json <- pxweb_query_as_json(pxq, pretty = TRUE)
```
