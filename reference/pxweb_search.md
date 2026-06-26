# Search PXWEB API tables

Search for tables in a PXWEB API. PXWEB API v2 search uses the standard
`/tables?query=` endpoint. PXWEB API v1 search is supported for API
database roots that expose search, such as Statistics Finland.

## Usage

``` r
pxweb_search(
  query,
  api_url,
  lang = NULL,
  page_number = 1,
  page_size = NULL,
  past_days = NULL,
  include_discontinued = FALSE,
  ...
)
```

## Arguments

- query:

  a search string.

- api_url:

  a PXWEB API root URL. For v2, use an API root such as
  `"https://statistikdatabasen.scb.se/api/v2"`. For v1, use a searchable
  database root such as
  `"https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin"`.

- lang:

  language code for PXWEB API v2. For v1, the language is part of
  `api_url`.

- page_number:

  page number for PXWEB API v2 searches.

- page_size:

  page size for PXWEB API v2 searches.

- past_days:

  if supplied, restrict PXWEB API v2 searches to tables updated in the
  last `past_days` days.

- include_discontinued:

  include discontinued PXWEB API v2 tables.

- ...:

  further arguments passed to
  [`httr::GET`](https://httr.r-lib.org/reference/GET.html).

## Value

a `data.frame` with search hits and table URLs.

## Examples

``` r
if (FALSE) { # \dontrun{
pxweb_search(
  "population",
  api_url = "https://statistikdatabasen.scb.se/api/v2",
  lang = "en"
)

pxweb_search(
  "population",
  api_url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin"
)
} # }
```
