# Construct a pragmatic `pxweb_data_v2` object.

A thin wrapper around PXWEB API v2 JSON-stat2 data. The original
response is kept as-is while data-frame coercion is provided through
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Usage

``` r
pxweb_data_v2(x)
```

## Arguments

- x:

  a list returned from a PXWEB API v2 data endpoint.

## Value

a `pxweb_data_v2` object.
