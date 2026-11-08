# Print a `pxweb_query` object as R code

Print a `pxweb_query` object as R code

## Usage

``` r
pxweb_query_as_rcode(pxq)
```

## Arguments

- pxq:

  a `pxweb_query` object.

## See also

[`pxweb_query_as_json`](https://ropengov.github.io/pxweb/reference/pxweb_query_as_json.md),
[`pxweb_query`](https://ropengov.github.io/pxweb/reference/pxweb_query.md)

## Examples

``` r
query_file <- system.file(
  "extdata", "examples", "json_query_example.json", package = "pxweb"
)
pxq <- pxweb_query(query_file)
pxweb_query_as_rcode(pxq)
#> pxweb_query_list <- 
#>   list("ContentsCode"=c("BE0101N1","BE0101N2"),
#>        "Tid"=c("2010","2011","2012"))
```
