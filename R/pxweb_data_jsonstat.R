#' Construct a \code{pxweb_data_jsonstat} object.
#'
#' @description
#' An object that contain the data for a given PXWEB table.
#'
#' @param x a list returned from a PXWEB API to convert to a \code{pxweb_data_jsonstat} object.
#'
#' @return
#' a \code{pxweb_data_jsonstat} object.
#'
#' @keywords internal
pxweb_data_jsonstat <- function(x) {
  checkmate::assert_class(x, "list")
  assert_pxweb_data_jsonstat(x)
  jsonlite::toJSON(x, pretty = TRUE)
}

#' Assert that x is a correct \code{pxweb_data_jsonstat} object.
#' Assert a json-stat version 1.0 or later object
#' @param x an object to check.
#' @keywords internal
assert_pxweb_data_jsonstat <- function(x) {
  checkmate::assert_class(x, c("list"))
  checkmate::assert_names(names(x), identical.to = c("dataset"))
  checkmate::assert_names(names(x$dataset), must.include = c("dimension", "value"))
}

#' Construct a \code{pxweb_data_jsonstat2} object.
#'
#' @description
#' An object that contains JSON-stat2 data returned by a PXWEB API v2
#' \code{/tables/{tableId}/data} endpoint.
#'
#' @param x a list returned from a PXWEB API v2 data endpoint.
#'
#' @return
#' a \code{pxweb_data_jsonstat2} object.
#'
#' @keywords internal
pxweb_data_jsonstat2 <- function(x) {
  checkmate::assert_class(x, "list")
  assert_pxweb_data_jsonstat2(x)
  class(x) <- c("pxweb_data_jsonstat2", "list")
  x
}

#' Construct a pragmatic \code{pxweb_data_v2} object.
#'
#' @description
#' A thin wrapper around PXWEB API v2 JSON-stat2 data. The original response
#' is kept as-is while data-frame coercion is provided through
#' \code{as.data.frame()}.
#'
#' @param x a list returned from a PXWEB API v2 data endpoint.
#'
#' @return
#' a \code{pxweb_data_v2} object.
#'
#' @keywords internal
pxweb_data_v2 <- function(x) {
  x <- pxweb_data_jsonstat2(x)
  class(x) <- c("pxweb_data_v2", class(x))
  x
}

#' Assert that x is a PXWEB API v2 JSON-stat2 data response.
#'
#' @param x an object to check.
#'
#' @keywords internal
assert_pxweb_data_jsonstat2 <- function(x) {
  checkmate::assert_class(x, "list")
  checkmate::assert_names(names(x), must.include = c("version", "class", "id", "size", "dimension", "value"))
  checkmate::assert_string(x$version, pattern = "^2\\.")
  checkmate::assert_string(x$class, pattern = "^dataset$")

  variable_ids <- unlist(x$id, use.names = FALSE)
  size <- unlist(x$size, use.names = FALSE)
  checkmate::assert_character(variable_ids, min.len = 1)
  checkmate::assert_integerish(size, lower = 1, len = length(variable_ids))
  checkmate::assert_list(x$dimension, min.len = length(variable_ids))
  checkmate::assert_subset(variable_ids, choices = names(x$dimension))

  value_length <- length(x$value)
  checkmate::assert_true(value_length > 0)
  checkmate::assert_true(value_length == prod(size))

  for (variable_id in variable_ids) {
    dim <- x$dimension[[variable_id]]
    checkmate::assert_names(names(dim), must.include = c("label", "category"))
    checkmate::assert_string(dim$label)
  }
}

pxweb_data_jsonstat2_values <- function(x) {
  values <- x$value
  if (is.list(values)) {
    return(vapply(values, function(value) {
      if (is.null(value) || length(value) == 0) {
        return(NA_real_)
      }
      as.numeric(value[[1]])
    }, numeric(1)))
  }
  as.numeric(values)
}
