# Build the PXWEB API v2 metadata URL for a table.

Creates a `/tables/{tableId}/metadata` URL from any PXWEB API v2 table,
metadata, or data URL. If the input URL contains a `lang` query
parameter, it is preserved on the metadata URL.

## Usage

``` r
pxweb_v2_table_metadata_url(x)
```

## Arguments

- x:

  a `pxweb` object, `url` object, or URL string.

## Value

a metadata endpoint URL string.
