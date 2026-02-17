# http_was_redirected

http_was_redirected

## Usage

``` r
http_was_redirected(r)
```

## Arguments

- r:

  an httr response object, e.g. from a call to httr::GET()

## Value

list with slots `was_redirected`, `redirected_from` and `redirected_to`

## References

Function in large parts taken from
<https://petermeissner.de/blog/2018/11/07/using-httr-to-detect-redirects/>.

## Examples

``` r
if (FALSE) { # \dontrun{
r <- httr::GET("http://httpbin.org/redirect/2")
pxweb:::http_was_redirected(r)
} # }
```
