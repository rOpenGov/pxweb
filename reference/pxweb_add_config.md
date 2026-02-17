# Add the config slot to a pxweb object

Add the config slot to a pxweb object

## Usage

``` r
pxweb_add_config(obj)
```

## Arguments

- obj:

  an object to add config to

## Details

Checks if there exist a config object in the object. Otherwise it query
the api to get it and add that call to the call stack.
