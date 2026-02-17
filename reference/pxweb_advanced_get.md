# Do a GET call to PXWEB API for advanced users

Do a GET call to PXWEB API for advanced users

## Usage

``` r
pxweb_advanced_get(
  url,
  query = NULL,
  verbose = TRUE,
  log_http_calls = FALSE,
  pxmdo = NULL,
  ...
)
```

## Arguments

- url:

  a `pxweb` object or url that can be coherced to a `pxweb` object.

- query:

  a json string, json file or list object that can be coherced to a
  `pxweb_query` object.

- verbose:

  should large queries print out progress.

- log_http_calls:

  Should the http calls to the API be logged (for debugging reasons). If
  TRUE, all calls and responses are logged and written to
  "log_pxweb_api_http_calls.txt" in the working directory.

- pxmdo:

  A `pxweb_metadata` object to use for query.

- ...:

  Further arguments sent to
  [`httr::POST`](https://httr.r-lib.org/reference/POST.html) (for
  queries) or [`httr::GET`](https://httr.r-lib.org/reference/GET.html)
  (for query = `NULL`). If used with query, also supply a
  `pxweb_metadata` object. Otherwise the same parameters are sent to
  both [`httr::POST`](https://httr.r-lib.org/reference/POST.html) and
  [`httr::GET`](https://httr.r-lib.org/reference/GET.html).

## Details

This function is intended for more advanced users that want to supply
specific arguments in `httr` calls or what to debug `httr` calls.

[`pxweb_get()`](https://ropengov.github.io/pxweb/reference/pxweb_get.md)
is a wrapper for standard use.
