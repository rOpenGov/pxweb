# Build PXWEB API v2 data endpoint query parameters.

Creates the query parameter list sent to PXWEB API v2
`/tables/{tableId}/data` requests. The language is taken from the URL
when present, otherwise from the raw v2 metadata attached to `pxmd`.

## Usage

``` r
pxweb_v2_data_query_params(px, pxmd = NULL, output_format = "json-stat2")
```

## Arguments

- px:

  a `pxweb` object.

- pxmd:

  optional `pxweb_metadata` object created from a v2 metadata response.

- output_format:

  data output format requested from the API. Defaults to `"json-stat2"`.

## Value

a named list of query parameters.
