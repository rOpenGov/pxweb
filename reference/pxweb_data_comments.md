# Construct a `pxweb_data_comments` object.

An object that contain the comments for a given PXWEB table.

## Usage

``` r
pxweb_data_comments(x)

# S3 method for class 'pxweb_data'
pxweb_data_comments(x)
```

## Arguments

- x:

  a `pxweb_data` object.

## Value

a `pxweb_data_comments` object

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
json_query <-
  file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
pxd <- pxweb_get(url = url, query = json_query)
pxdcs <- pxweb_data_comments(x = pxd)
pxdc_df <- as.data.frame(pxdcs, stringsAsFactors = TRUE)
} # }
```
