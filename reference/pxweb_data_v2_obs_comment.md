# Construct a v2 observation comment.

Converts a JSON-stat2 observation-level annotation, currently cell
`status`, to a v1-style `obs_comment` object.

## Usage

``` r
pxweb_data_v2_obs_comment(x, obs_idx, comment)
```

## Arguments

- x:

  a `pxweb_data_v2` object.

- obs_idx:

  one-based observation row index.

- comment:

  a normalized comment string.

## Value

a `pxweb_data_comment` object.
