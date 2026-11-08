# Cite a PXWEB data object

Cite a PXWEB data object

## Usage

``` r
pxweb_cite(x, style = "citation")
```

## Arguments

- x:

  a `pxweb_data` or `pxweb_data_v2` object to cite.

- style:

  see [`bibentry`](https://rdrr.io/r/utils/bibentry.html).

## Details

Functionality to automatic cite PXWEB data objects.

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB638/metadata?lang=en"
query <- list(
  Region = "00", Civilstand = "OG", Alder = "0", Kon = "1",
  ContentsCode = "BE0101N1", Tid = "2024"
)
population <- pxweb_get(url, query = query)
pxweb_cite(population)
} # }
```
