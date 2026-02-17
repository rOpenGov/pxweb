# Coerce a `pxweb_data` object to a `data.frame`

Coerce a `pxweb_data` object to a `data.frame`

## Usage

``` r
pxweb_as_data_frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  column.name.type = "text",
  variable.value.type = "text"
)

# S3 method for class 'pxweb_data'
pxweb_as_data_frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  column.name.type = "text",
  variable.value.type = "text"
)

# S3 method for class 'pxweb_data_comments'
pxweb_as_data_frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE
)

# S3 method for class 'pxweb_data_comment'
pxweb_as_data_frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE
)

# S3 method for class 'pxweb_levels'
pxweb_as_data_frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE
)

# S3 method for class 'pxweb_data'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  column.name.type = "text",
  variable.value.type = "text"
)

# S3 method for class 'pxweb_data_comments'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE
)

# S3 method for class 'pxweb_levels'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE
)

# S3 method for class 'pxweb_metadata'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE
)

pxweb_as_matrix(
  x,
  row.names = NULL,
  column.name.type = "text",
  variable.value.type = "text"
)

# S3 method for class 'pxweb_data'
pxweb_as_matrix(
  x,
  row.names = NULL,
  column.name.type = "text",
  variable.value.type = "text"
)

# S3 method for class 'pxweb_data'
as.matrix(
  x,
  ...,
  row.names = NULL,
  column.name.type = "text",
  variable.value.type = "text"
)

pxweb_pxd_slot_idx_pos(x)
```

## Arguments

- x:

  an object to convert to `data.frame`.

- row.names:

  See [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  See [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  See [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- stringsAsFactors:

  See [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).

- column.name.type:

  character: should `code` or `text` be used as column names?

- variable.value.type:

  character: should `code` or `text` be used as values in columns?

## See also

[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html).
