# Construct v2 status comments.

Converts JSON-stat2 `status` entries to a list of v1-style `obs_comment`
objects. JSON-stat2 status indexes are zero-based, so they are shifted
to the one-based row indexes used by `pxweb_data_comments`.

## Usage

``` r
pxweb_data_v2_status_comments(x)
```

## Arguments

- x:

  a `pxweb_data_v2` object.

## Value

a list of `pxweb_data_comment` objects.
