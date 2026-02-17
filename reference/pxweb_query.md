# Create a PXWEB query

Creates a pxweb query object from either a list with named values, a
json query file or json query string. See examples below.

## Usage

``` r
pxweb_query(x)

# S3 method for class 'character'
pxweb_query(x)

# S3 method for class 'json'
pxweb_query(x)

# S3 method for class 'pxweb_query'
pxweb_query(x)

# S3 method for class 'list'
pxweb_query(x)

# S3 method for class 'response'
pxweb_query(x)

# S3 method for class 'pxweb_explorer'
pxweb_query(x)
```

## Arguments

- x:

  an object to cast as a pxweb_query object.

## See also

[`pxweb_query_as_json`](https://ropengov.github.io/pxweb/reference/pxweb_query_as_json.md),
[`pxweb_query_as_rcode`](https://ropengov.github.io/pxweb/reference/pxweb_query_as_rcode.md)

## Examples

``` r
dims <- list(
  Alue = c("*"),
  "Asuntokunnan koko" = c("*"),
  Talotyyppi = c("S"),
  Vuosi = c("*")
)
pxq1 <- pxweb_query(dims)

json_query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_query_example.json"
)
pxq2 <- pxweb_query(json_query)
```
