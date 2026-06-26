# Extract a PXWEB API v2 table id.

Gets the table id used by PXWEB API v2 `/tables/{tableId}` endpoints.
When v2 metadata is available, the id is read from the preserved raw
metadata attribute; otherwise it is parsed from the URL path.

## Usage

``` r
pxweb_v2_table_id(x, pxmd = NULL)
```

## Arguments

- x:

  a `pxweb` object, `url` object, or URL string.

- pxmd:

  optional `pxweb_metadata` object created from a v2 metadata response.

## Value

a single table id string.
