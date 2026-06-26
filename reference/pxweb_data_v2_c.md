# Combine PXWEB API v2 data objects.

Combines a list of `pxweb_data_v2` JSON-stat2 data objects returned from
split PXWEB API v2 queries. The combined object keeps the v2 data shape
and rebuilds dimension indexes, labels, sizes, and values so it can be
converted with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Usage

``` r
pxweb_data_v2_c(x)
```

## Arguments

- x:

  a list of `pxweb_data_v2` objects.

## Value

a combined `pxweb_data_v2` object.
