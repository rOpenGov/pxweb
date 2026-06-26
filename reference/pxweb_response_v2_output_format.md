# Get the requested PXWEB API v2 output format from a response URL.

Extracts the `outputFormat` query parameter from a PXWEB API v2 response
URL. The value is lower-cased so it can be used for response routing. If
the response URL has no `outputFormat` parameter, the function returns
`NULL`.

## Usage

``` r
pxweb_response_v2_output_format(x)
```

## Arguments

- x:

  a `httr` response object.

## Value

a lower-case output format string, or `NULL`.
