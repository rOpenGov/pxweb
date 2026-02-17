# Parse the response from a PXWEB API (advanced)

The function parses the response from a call made to a PXWEB API using
the `httr` R package. In this way it is possible to parse the content of
calls made outside the pxweb R package.

## Usage

``` r
pxweb_parse_response(x)

is_pxweb_response(x)
```

## Arguments

- x:

  a `httr` response object from a PXWEB call.
