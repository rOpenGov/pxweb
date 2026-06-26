# Convert a PXWEB API v2 output format to a file extension.

Maps a PXWEB API v2 `outputFormat` value to the file extension used when
non JSON-stat2 responses are written to a temporary file. Most formats
are already valid extensions; `json-px` is normalized to `json`.

## Usage

``` r
pxweb_v2_output_file_extension(output_format)
```

## Arguments

- output_format:

  a PXWEB API v2 output format string.

## Value

a file extension string.
