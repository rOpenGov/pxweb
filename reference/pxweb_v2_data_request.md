# Build a PXWEB API v2 data request.

Creates the request pieces used to POST a query to the PXWEB API v2
`/tables/{tableId}/data` endpoint. The returned body is the v2
`selection` JSON generated from a `pxweb_query`; the query parameters
include language and output format.

## Usage

``` r
pxweb_v2_data_request(px, pxq, pxmd, output_format = "json-stat2")
```

## Arguments

- px:

  a `pxweb` object for a PXWEB API v2 endpoint.

- pxq:

  a `pxweb_query` object.

- pxmd:

  a `pxweb_metadata` object created from v2 metadata.

- output_format:

  data output format requested from the API. Defaults to `"json-stat2"`.

## Value

a list with `url`, `body`, and `query` elements.
