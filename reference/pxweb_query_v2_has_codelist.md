# Test whether a query variable uses a PXWEB API v2 codelist.

Codelist selections are carried as URL query parameters in v2 requests
and should not be validated or expanded against the ordinary metadata
values.

## Usage

``` r
pxweb_query_v2_has_codelist(pxq, variable_code)
```

## Arguments

- pxq:

  a `pxweb_query` object.

- variable_code:

  PXWEB variable code.

## Value

`TRUE` if `variable_code` uses a v2 codelist query parameter, otherwise
`FALSE`.
