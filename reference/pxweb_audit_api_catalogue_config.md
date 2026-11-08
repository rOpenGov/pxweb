# Audit PXWEB API catalogue config endpoints

Checks whether each version and language combination in a PXWEB API
catalogue entry exposes a valid config endpoint.

## Usage

``` r
pxweb_audit_api_catalogue_config(
  apis = pxweb_api_catalogue(),
  timeout = 10,
  verbose = TRUE
)
```

## Arguments

- apis:

  a list of pxweb_api_catalogue_entry elements

- timeout:

  Time limit in seconds for each config endpoint request.

- verbose:

  Print progress while auditing.

## Value

A data.frame with one row per catalogue URL.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check only the Statistics Sweden entry, rather than the whole catalogue.
apis <- pxweb_api_catalogue()["api.scb.se"]
pxweb_audit_api_catalogue_config(apis, timeout = 5, verbose = FALSE)
} # }
```
