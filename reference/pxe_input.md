# Get (allowed) inputs for a `pxweb_input_allowed` object.

Get (allowed) inputs for a `pxweb_input_allowed` object.

## Usage

``` r
pxe_input(allowed_input, title = NULL, test_input = NULL)

pxe_parse_input(user_input, allowed_input)
```

## Arguments

- allowed_input:

  a `pxweb_input_allowed`.

- title:

  Print (using cat()) before ask for the allowed choices.

- test_input:

  supplying a test input (for testing only)

## Details

It handles input and checks if the input is allowed.
