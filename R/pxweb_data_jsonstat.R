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
