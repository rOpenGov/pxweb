# S3 constructor for `pxweb` api object.

The pxwebapi object contain all information to do calls to the pxweb api
and keep count of the number of calls. The object is constructed The
object will also be cached in R temp folder to minimize calls to api.
All urls should be passed through the constructor to set up the pxweb
api config.

*Garantuees*: The base_url has been pinged The sub_path has been checked
The config has been captured from the API The url has been checked to be
a pxweb api (through config)

## Usage

``` r
pxweb(url)

is.pxweb(x)

# S3 method for class 'pxweb'
print(x, ...)
```

## Arguments

- url:

  an url to a pxweb api including language and version of the api. See
  examples.

- x:

  an an object to test if it is a `pxweb` object.

- ...:

  further arguments supplied to
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

A `pxweb` object.

## Examples

``` r
if (FALSE) { # \dontrun{
pxapi_1 <-
  pxweb("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
pxapi_2 <-
  pxweb(url = "https://api.scb.se/OV0104/v1/doris/sv")
} # }
```
