# Create a PXWEB query selection

Creates a query selection for use inside a named list passed to
[`pxweb_query`](https://ropengov.github.io/pxweb/reference/pxweb_query.md)
or
[`pxweb_get`](https://ropengov.github.io/pxweb/reference/pxweb_get.md).
This is useful when a PXWEB table requires an explicit API filter such
as an aggregation or value set filter.

## Usage

``` r
pxweb_selection(filter = "item", values)
```

## Arguments

- filter:

  a PXWEB selection filter, for example `"item"`, `"all"`,
  `"agg:_Regions 2026.agg"`, or `"vs:Some valueset"`.

- values:

  selected value codes for the filter.

## Value

A `pxweb_query_selection_filter` object.

## Examples

``` r
query <- list(
  alue_23_20260101 = pxweb_selection("agg:_Regions 2026.agg", "MK01"),
  timeperiod_y = "2025"
)

pxq <- pxweb_query(query)
```
