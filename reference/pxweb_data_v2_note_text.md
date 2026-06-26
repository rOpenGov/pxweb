# Normalize PXWEB API v2 note/status text.

Collapses JSON-stat2 note-like fields to a single character string.
Empty and missing values return `NULL`, while multiple note fragments
are joined with newlines.

## Usage

``` r
pxweb_data_v2_note_text(x)
```

## Arguments

- x:

  a JSON-stat2 note/status field.

## Value

a character string, or `NULL` when no text is available.
