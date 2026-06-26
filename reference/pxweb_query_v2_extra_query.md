# Get PXWEB API v2 extra query parameters.

Returns the extra URL query parameters attached to a `pxweb_query` by v2
selection helpers, for example aggregation and valueset codelists.

## Usage

``` r
pxweb_query_v2_extra_query(pxq)
```

## Arguments

- pxq:

  a `pxweb_query` object.

## Value

A named list of extra v2 query parameters, such as `codelist[Variable]`
and `outputValues[Variable]`.
