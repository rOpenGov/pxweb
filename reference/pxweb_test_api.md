# Test a full or a part of a PXWEB api.

The function can be used to test a whole pxweb api by using the api base
url. By using a branch in a tree the api is tested below this branch.

## Usage

``` r
pxweb_test_api(
  url,
  test_type = "first",
  n = 1,
  verbose = TRUE,
  time_limit = Inf
)
```

## Arguments

- url:

  The base url to the pxweb api (or a branch of the metadata tree)

- test_type:

  What type of test should be done. The `first` observation of each
  table. A random `sample` of size `n`. Download all `full` tables.
  `touch` the api by only downloading the first table metadata. This is
  minimal testing of the API.

- n:

  sample size if `test_type` is `sample`.

- verbose:

  The function will print information.

- time_limit:

  Time limit in second the API is allowed to be tested.

## Value

Function returns a data.frame with information on each node Two
variables are added: `checked` : The node has been checked `error` :
Whether there were errors encountered with the call `download_error` :
Whether there were errors encountered during download

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
res <- pxweb_test_api(url)
res <- pxweb_test_api(url, test_type = "touch")
} # }
```
