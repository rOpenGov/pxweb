# Get the PXWEB API v2 selection type for a query variable.

The selection type is recorded when a query is built from v2 helper
objects such as
[`pxweb_latest`](https://ropengov.github.io/pxweb/reference/pxweb_query_helpers.md)
or
[`pxweb_aggregation`](https://ropengov.github.io/pxweb/reference/pxweb_query_helpers.md).

## Usage

``` r
pxweb_query_v2_selection_type(pxq, variable_code)
```

## Arguments

- pxq:

  a `pxweb_query` object.

- variable_code:

  PXWEB variable code.

## Value

A character scalar with the v2 selection type for `variable_code`.
