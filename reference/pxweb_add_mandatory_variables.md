# Add mandatory variables to query

Add mandatory variables to query

## Usage

``` r
pxweb_add_mandatory_variables(pxq, pxmd)
```

## Arguments

- pxq:

  a `pxweb_query` object.

- pxmd:

  a `pxweb_metadata` object.

## Details

Complement queries that lack explicit requests for variables with
requests for every value of these variables.
