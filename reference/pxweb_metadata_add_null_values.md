# Add values to NULL value variables in PXWEB metadata objects

Add values to NULL value variables in PXWEB metadata objects

## Usage

``` r
pxweb_metadata_add_null_values(x, px)
```

## Arguments

- x:

  an object to check if is a `pxweb_metadata` object to which we should
  add values.

- px:

  a `pxweb` object

## Details

Some metadata objects may have NULL values. In these cases the values
are downloaded and added to the metadata object.
