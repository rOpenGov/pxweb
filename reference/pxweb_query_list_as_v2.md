# Convert a named list to a PXWEB API v2 request.

Convert a named list to a PXWEB API v2 request.

## Usage

``` r
pxweb_query_list_as_v2(x)
```

## Arguments

- x:

  a named list with character values or `pxweb_query_selection` helper
  objects.

## Value

A list with a `body` slot containing a `pxweb_query_v2` object and an
`extra_query` slot containing v2 query parameters such as
`codelist[Variable]` and `outputValues[Variable]`.
