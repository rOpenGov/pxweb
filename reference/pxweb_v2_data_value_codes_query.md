# Build PXWEB API v2 value code query parameters.

Creates the `valueCodes[Variable]` URL query parameters used when a
split v2 request cannot send its selection in the JSON body.

## Usage

``` r
pxweb_v2_data_value_codes_query(pxq)
```

## Arguments

- pxq:

  a `pxweb_query` object.

## Value

A named list of `valueCodes[Variable]` query parameters, one for each
query dimension.
