# PXWEB API v2 query helpers

Helper functions for expressing PXWEB API v2 query selections without
relying on magic strings in user code.

## Usage

``` r
pxweb_all()

pxweb_latest(code = "9999")

pxweb_aggregation(
  id,
  value_codes = "*",
  output_values = c("aggregated", "single")
)

pxweb_valueset(
  id,
  value_codes = "*",
  output_values = c("aggregated", "single")
)

pxweb_top(n)

pxweb_bottom(n)
```

## Arguments

- code:

  placeholder value used before the latest available value is resolved
  against table metadata. The default `"9999"` is replaced by the last
  metadata value before the v2 data request is sent.

- id:

  codelist id, for example an aggregation id returned by a PXWEB API v2
  metadata response.

- value_codes:

  value codes to request from the selected codelist.

- output_values:

  whether the API should return aggregated values or single values for
  the codelist.

- n:

  number of values to request.

## Value

A `pxweb_query_selection` object.

## Examples

``` r
query <- list(
  Region = pxweb_all(),
  Alder = pxweb_aggregation("agg_Ålder5år_1"),
  Tid = pxweb_latest()
)
```
