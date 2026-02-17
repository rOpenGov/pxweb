# Get the api name, rootpath, subpath, path or dbpath

Get the api name, rootpath, subpath, path or dbpath

## Usage

``` r
pxweb_api_name(x)

# S3 method for class 'url'
pxweb_api_name(x)

# S3 method for class 'pxweb'
pxweb_api_name(x)

# S3 method for class 'pxweb_api_catalogue_entry'
pxweb_api_name(x)

# S3 method for class 'pxweb_explorer'
pxweb_api_name(x)

pxweb_api_rootpath(x)

# S3 method for class 'url'
pxweb_api_rootpath(x)

# S3 method for class 'pxweb'
pxweb_api_rootpath(x)

# S3 method for class 'pxweb_explorer'
pxweb_api_rootpath(x)

pxweb_api_subpath(x, init_slash = TRUE, as_vector = FALSE)

# S3 method for class 'pxweb'
pxweb_api_subpath(x, init_slash = TRUE, as_vector = FALSE)

# S3 method for class 'pxweb_explorer'
pxweb_api_subpath(x, init_slash = TRUE, as_vector = FALSE)

pxweb_api_path(x, init_slash = TRUE, as_vector = FALSE)

# S3 method for class 'url'
pxweb_api_path(x, init_slash = TRUE, as_vector = FALSE)

# S3 method for class 'pxweb'
pxweb_api_path(x, init_slash = TRUE, as_vector = FALSE)

# S3 method for class 'pxweb_explorer'
pxweb_api_path(x, init_slash = TRUE, as_vector = FALSE)

pxweb_api_dbpath(x, init_slash = TRUE, as_vector = FALSE)

# S3 method for class 'pxweb'
pxweb_api_dbpath(x, init_slash = TRUE, as_vector = FALSE)

# S3 method for class 'pxweb_explorer'
pxweb_api_path(x, init_slash = TRUE, as_vector = FALSE)

assert_path(x)

pxe_position_path(
  x,
  init_slash = TRUE,
  as_vector = FALSE,
  include_rootpath = FALSE
)

pxe_metadata_path(x, as_vector = FALSE)
```

## Arguments

- x:

  object to get the name or path for

- init_slash:

  should `subpath` and `path` start with a `/`. Default is `TRUE`.

- as_vector:

  should `subpath` and `path` be a vector split by /. Default is
  `FALSE`.

- include_rootpath:

  Should the rootpath be included? Default is FALSE

## Details

The PXWEB API contain the following path:
API-NAME/API-VERSION/LANGUAGE/DATABASE-ID/\<LEVELS\>/TABLE-ID

The full url is made up by the `rootpath`, `subpath`, and `path`. The
`rootpath` is made up of the protocol and the API-NAME / hostname and
protocol (if any).

The `subpath` contain the API-VERSION and LANGUAGE but can contain other
parts as well. The subpath is the shortest the config can be called for.
It can be seen as the base for the API.

The `dbpath`, the data base path, contain
DATABASE-ID/\<LEVELS\>/TABLE-ID.

The `path`, is the standar path of an url, i.e. `subpath` + `dbpath`.

No path ends with slash, but `subpath` and `dbpath` may begin with
slash, see the parameters
