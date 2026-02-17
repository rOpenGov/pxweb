# Create a `pxweb_explorer` object.

`position` the current position in the api, as a character vector from
the root. Note position is not alway a correct url. Metadata and other
choices are part of position

`root` is the bottom path (as position) that the user can go. If length
0, user can essentially go to hostname.

paste(root_path + position, collapse = "/") is used to construct the
path to the position in case of url.

print out a bar for separation purposes

## Usage

``` r
pxweb_explorer(x = NULL)

# S3 method for class '`NULL`'
pxweb_explorer(x)

# S3 method for class 'character'
pxweb_explorer(x)

# S3 method for class 'pxweb'
pxweb_explorer(x)

# S3 method for class 'pxweb_api_catalogue_entry'
pxweb_explorer(x)

assert_pxweb_explorer(x)

# S3 method for class 'pxweb_explorer'
print(x, ...)

print_bar()

pxe_print_choices(x)
```

## Arguments

- x:

  a `pxweb` object, a PXWEB url, `NULL` or an api in the api catalogue.

## Examples

``` r
## The functions below are internal generic functions
## x <- pxweb_explorer()
## url <- "api.scb.se"
## x <- pxweb_explorer(x = url)
## url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/"
## x <- pxweb_explorer(x = url)
## url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
## x <- pxweb_explorer(x = url)
```
