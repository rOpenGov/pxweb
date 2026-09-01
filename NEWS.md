# Version 1.0.0

- Added support for PXWEB API v2, including v2 metadata, table, data and
  JSON-stat2 response handling while preserving the existing v1 workflows.
- Added v2 query helpers for common selections such as all values, latest
  values, top/bottom selections, aggregations and value sets.
- Added `pxweb_search()` for searching PXWEB tables through both v2 table
  search endpoints and searchable v1 database roots.
- Added `pxweb_codelists()` and expanded comment extraction so v2 codelists,
  dataset notes, dimension notes, value notes and observation status comments
  can be inspected from R.
- Improved `pxweb_get()`, query validation, data-frame conversion and request
  splitting to work with both v1 and v2 APIs.
- Updated the bundled API catalogue, README, vignette examples and
  documentation for the new v2 workflows.
- Expanded automated coverage with mocked SCB v1/v2 fixtures, unit tests,
  live-test workflows, example checks and API catalogue audits.

# Version 0.16.2

- Return better error messages when using as.data.frame() for pxweb metadata objects.
- Fix Statistics Swedens new https API.


# Version 0.16.1

- Remove tests that are failing on CRAN.


# Version 0.16.0

- exposed pxweb_parse_response() and is_pxweb_response() to the package API for more advanced users that want to make their own `httr` calls to a PXWEB API.

# Version 0.15.0

- Added possibility to download px and sdmx response formats as files
- Added StatSI API
- Added codecov code coverage stats
- Fixed some minor API configs

# Version 0.14.0

- Added feature to print pxweb_query objects to R code

# Version 0.10.4

- Bug fixes
- More informative error messages

# Version 0.9.2

- Added API link to Visit Finland (Rudolf)

# Version 0.8

- Package rewritten in order to enhance the efficiency and design


# Version 0.6.37

## New features

- Added data.ssb.no in the API list in inst/extdata/api.json
- Added NEWS.md file
