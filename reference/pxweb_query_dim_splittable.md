# Get vector indicating splittable variables

Get vector indicating splittable variables

## Usage

``` r
pxweb_query_dim_splittable(pxq, pxmd)
```

## Arguments

- pxq:

  a `pxweb_query` object.

## Value

a named logical vector.

## Details

Splitable variables are variables that can be split. Content variables
cannot be split, nor variables with filter == "top".

Currently, we can only be sure that time variables and eliminated
variables can be split. Hopefully the next API makes this more clear.
