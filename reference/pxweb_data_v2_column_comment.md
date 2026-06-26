# Construct a v2 dimension note comment.

Converts a JSON-stat2 dimension-level note to a v1-style
`column_comment` object.

## Usage

``` r
pxweb_data_v2_column_comment(x, column_idx, comment)
```

## Arguments

- x:

  a `pxweb_data_v2` object.

- column_idx:

  integer index of the dimension column.

- comment:

  a normalized comment string.

## Value

a `pxweb_data_comment` object.
