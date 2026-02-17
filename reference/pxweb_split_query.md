# Split query in optimal sub-queries

Split query in optimal sub-queries

## Usage

``` r
pxweb_split_query(pxq, px, pxmd)
```

## Arguments

- pxq:

  a `pxweb_query` object.

- px:

  a `pxweb` object.

- pxmd:

  a `pxweb_metadata` object.

## Value

a list with `pxweb_query` objects.

## Details

Computes (brute-force) the optimal split of a query to match the api
maximum value limit. It also take into account that time variables and
content variables should not be split. Also variables with filter "top"
should not be split, since the top filter does not supply the individual
levels, just a number. This can probably be improved further.
