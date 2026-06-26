# Construct a `pxweb_metadata` object from a PXWEB API v2 metadata response.

Converts the JSON-stat2-style metadata returned by PXWEB API v2
`/tables/{tableId}/metadata` endpoints to the existing `pxweb_metadata`
shape used by the package internals. The original v2 payload is
preserved as the `pxweb_metadata_v2` attribute.

## Usage

``` r
pxweb_metadata_v2(x)
```

## Arguments

- x:

  a list returned from a PXWEB API v2 metadata endpoint.

## Value

a `pxweb_metadata` object.
