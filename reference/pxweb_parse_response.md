# Parse the response from a PXWEB API (advanced)

The function parses the response from a call made to a PXWEB API using
the `httr` R package. In this way it is possible to parse the content of
calls made outside the pxweb R package.

## Usage

``` r
pxweb_parse_response(x)

is_pxweb_response(x)
```

## Arguments

- x:

  a `httr` response object from a PXWEB call.

## Value

`pxweb_parse_response()` returns the parsed response or, for file-based
formats, a path to the downloaded file. `is_pxweb_response()` returns
`TRUE` if the response can be parsed and `FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB638/metadata?lang=en"
response <- httr::GET(url, httr::timeout(10))
httr::stop_for_status(response)
if (is_pxweb_response(response)) {
  metadata <- pxweb_parse_response(response)
  print(metadata)
}
} # }
```
