# Add an api call to a pxweb_api_s3 object

The pxweb_add_call function add a call the the api in the call stack,
then compute

Promise: The stored rda api object will always have the latest calls
This is not thread safe so only one session at a time should call the
api.

## Usage

``` r
pxweb_add_call(obj, time_stamp = Sys.time())
```

## Arguments

- obj:

  a `pxweb_api_s3` object
