# List PXWEB API v2 codelists in metadata.

Extracts aggregation and value set codelists from a PXWEB API v2
metadata object. Use the returned `id` values with
[`pxweb_aggregation`](https://ropengov.github.io/pxweb/reference/pxweb_query_helpers.md)
or
[`pxweb_valueset`](https://ropengov.github.io/pxweb/reference/pxweb_query_helpers.md).

## Usage

``` r
pxweb_codelists(x, variable = NULL, type = NULL)
```

## Arguments

- x:

  a `pxweb_metadata` object created from PXWEB API v2 metadata, or a raw
  PXWEB API v2 metadata list.

- variable:

  optional variable code or label to filter on.

- type:

  optional codelist type to filter on, for example `"Aggregation"` or
  `"Valueset"`.

## Value

A `data.frame` with columns `variable_code`, `variable_text`, `id`,
`label`, `type`, and `href`.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- pxweb_get("https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata?lang=sv")
pxweb_codelists(meta, variable = "Alder")
} # }
```
