# Construct a v2 category note comment.

Converts a JSON-stat2 category-level note to a v1-style `value_comment`
object and indexes every data-frame row containing the noted value.

## Usage

``` r
pxweb_data_v2_value_comment(x, column_idx, value_code, comment)
```

## Arguments

- x:

  a `pxweb_data_v2` object.

- column_idx:

  integer index of the dimension column.

- value_code:

  category value code that carries the note.

- comment:

  a normalized comment string.

## Value

a `pxweb_data_comment` object.
