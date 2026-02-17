# Setup a structure to log all API calls

Setup a structure to log all API calls

## Usage

``` r
pxweb_http_log_on(fn = "log_pxweb_api_http_calls.txt")

pxweb_http_log_off()

pxweb_log_paths_path()

pxweb_http_log_is_on()

pxweb_http_log_response(r)

pxweb_response_to_log_as_json(r)
```

## Arguments

- fn:

  The file name of the log file.

## Details

Set up internal structure to handle http PXWEB API calls. http calls are
stored in a log file. To access the path to this log file, the path is
stored in the pxweb folder in the tempdir(). The path is given by
`pxweb_log_paths_path()`. If the file exists, the http calls should be
logged (appended) to the log file where the object contains the path.
