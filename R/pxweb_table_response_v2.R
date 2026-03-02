#' Construct a \code{table_response_v2} object.
#'
#' @description
#' An object that contain the data for a given PXWEB TableResponse (API v2).
#'
#' @param x a list returned from a PXWEB API to convert to a \code{table_response_v2} object.
#'
#' @return
#' a \code{pxweb_table_v2} object.
#'
#' @keywords internal
pxweb_table_response_v2 <- function(x) {
  checkmate::assert_class(x, "list")
  assert_pxweb_table_response_v2(x)
  class(x) <- c("pxweb_table_response_v2", "list")
  x
}

#' Assert that x is a correct \code{pxweb_data_jsonstat} object.
#' Assert a json-stat version 1.0 or later object
#' @param x an object to check.
#' @keywords internal
assert_pxweb_table_response_v2 <- function(x) {
  checkmate::assert_class(x, c("list"))
  checkmate::assert_names(names(x), must.include = c("language", "id", "label", "updated", "firstPeriod", "lastPeriod", "variableNames", "links"))
}

