# Construct a v2 dataset note comment.

Converts a JSON-stat2 dataset-level note to an `obs_comment` object.
Dataset notes do not point to a specific row or column, so both index
positions are `NA`.

## Usage

``` r
pxweb_data_v2_dataset_note_comment(x, comment)
```

## Arguments

- x:

  a `pxweb_data_v2` object.

- comment:

  a normalized comment string.

## Value

a `pxweb_data_comment` object.
