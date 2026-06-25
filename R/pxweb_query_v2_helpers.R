#' PXWEB API v2 query helpers
#'
#' @description
#' Helper functions for expressing PXWEB API v2 query selections without
#' relying on magic strings in user code.
#'
#' @return
#' A \code{pxweb_query_selection} object.
#'
#' @examples
#' query <- list(
#'   Region = pxweb_all(),
#'   Alder = pxweb_aggregation("agg_Alder5ar_1"),
#'   Tid = pxweb_latest()
#' )
#'
#' @name pxweb_query_helpers
NULL

#' @rdname pxweb_query_helpers
#' @export
pxweb_all <- function() {
  pxweb_query_selection(
    type = "all",
    value_codes = "*",
    class = "pxweb_query_all"
  )
}

#' @rdname pxweb_query_helpers
#' @param code value code used to represent the latest time period before it is
#'   resolved against table metadata.
#' @export
pxweb_latest <- function(code = "9999") {
  checkmate::assert_string(code, min.chars = 1)

  pxweb_query_selection(
    type = "latest",
    value_codes = code,
    class = "pxweb_query_latest"
  )
}

#' @rdname pxweb_query_helpers
#' @param id codelist id, for example an aggregation id returned by a PXWEB API
#'   v2 metadata response.
#' @param value_codes value codes to request from the selected codelist.
#' @param output_values whether the API should return aggregated values or
#'   single values for the codelist.
#' @export
pxweb_aggregation <- function(id, value_codes = "*", output_values = c("aggregated", "single")) {
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_character(value_codes, min.len = 1, any.missing = FALSE)
  output_values <- match.arg(output_values)

  pxweb_query_selection(
    type = "aggregation",
    value_codes = value_codes,
    codelist = id,
    output_values = output_values,
    class = c("pxweb_query_aggregation", "pxweb_query_codelist")
  )
}

#' @rdname pxweb_query_helpers
#' @export
pxweb_valueset <- function(id, value_codes = "*", output_values = c("aggregated", "single")) {
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_character(value_codes, min.len = 1, any.missing = FALSE)
  output_values <- match.arg(output_values)

  pxweb_query_selection(
    type = "valueset",
    value_codes = value_codes,
    codelist = id,
    output_values = output_values,
    class = c("pxweb_query_valueset", "pxweb_query_codelist")
  )
}

#' @rdname pxweb_query_helpers
#' @param n number of values to request.
#' @export
pxweb_top <- function(n) {
  checkmate::assert_int(n, lower = 1)

  pxweb_query_selection(
    type = "top",
    value_codes = paste0("top(", n, ")"),
    class = "pxweb_query_top"
  )
}

#' @rdname pxweb_query_helpers
#' @export
pxweb_bottom <- function(n) {
  checkmate::assert_int(n, lower = 1)

  pxweb_query_selection(
    type = "bottom",
    value_codes = paste0("bottom(", n, ")"),
    class = "pxweb_query_bottom"
  )
}

#' Construct a PXWEB API v2 query selection helper.
#'
#' @param type selection type.
#' @param value_codes values to use in the v2 \code{valueCodes} selection.
#' @param codelist optional codelist id.
#' @param output_values optional output values mode.
#' @param class additional S3 class names.
#'
#' @keywords internal
pxweb_query_selection <- function(type, value_codes, codelist = NULL, output_values = NULL, class = character()) {
  checkmate::assert_string(type, min.chars = 1)
  checkmate::assert_character(value_codes, min.len = 1, any.missing = FALSE)
  checkmate::assert_string(codelist, min.chars = 1, null.ok = TRUE)
  checkmate::assert_choice(output_values, choices = c("aggregated", "single"), null.ok = TRUE)

  structure(
    list(
      type = type,
      value_codes = value_codes,
      codelist = codelist,
      output_values = output_values
    ),
    class = c(class, "pxweb_query_selection", "list")
  )
}

#' Test for PXWEB API v2 query selection helpers.
#'
#' @param x an object to check.
#'
#' @keywords internal
is_pxweb_query_selection <- function(x) {
  inherits(x, "pxweb_query_selection")
}

#' Convert a named list to a PXWEB API v2 request.
#'
#' @param x a named list with character values or \code{pxweb_query_selection}
#'   helper objects.
#'
#' @return
#' A list with a \code{body} slot containing a \code{pxweb_query_v2} object and
#' an \code{extra_query} slot containing v2 query parameters such as
#' \code{codelist[Variable]} and \code{outputValues[Variable]}.
#'
#' @keywords internal
pxweb_query_list_as_v2 <- function(x) {
  checkmate::assert_list(x, min.len = 1)
  checkmate::assert_names(names(x), type = "unique", what = "names")

  selection <- vector("list", length(x))
  extra_query <- list()

  for (i in seq_along(x)) {
    variable_code <- names(x)[i]
    value <- pxweb_query_value_as_v2(x[[i]], variable_code)
    selection[[i]] <- value$selection
    extra_query <- c(extra_query, value$extra_query)
  }

  pxweb_query_v2_request(
    body = pxweb_query_v2(list(selection = selection)),
    extra_query = extra_query
  )
}

#' Convert one query value to a PXWEB API v2 selection fragment.
#'
#' @param x a character vector or \code{pxweb_query_selection} object.
#' @param variable_code PXWEB variable code.
#'
#' @keywords internal
pxweb_query_value_as_v2 <- function(x, variable_code) {
  checkmate::assert_string(variable_code, min.chars = 1)

  if (is_pxweb_query_selection(x)) {
    value_codes <- x$value_codes
    extra_query <- pxweb_query_selection_extra_query(x, variable_code)
  } else {
    checkmate::assert_character(x, min.len = 1, any.missing = FALSE)
    value_codes <- x
    extra_query <- list()
  }

  list(
    selection = list(
      variableCode = variable_code,
      valueCodes = as.list(value_codes)
    ),
    extra_query = extra_query
  )
}

#' Build codelist-related query parameters for a v2 selection helper.
#'
#' @param x a \code{pxweb_query_selection} object.
#' @param variable_code PXWEB variable code.
#'
#' @keywords internal
pxweb_query_selection_extra_query <- function(x, variable_code) {
  checkmate::assert_class(x, "pxweb_query_selection")
  checkmate::assert_string(variable_code, min.chars = 1)

  if (!inherits(x, "pxweb_query_codelist")) {
    return(list())
  }

  stats::setNames(
    list(x$codelist, x$output_values),
    c(
      paste0("codelist[", variable_code, "]"),
      paste0("outputValues[", variable_code, "]")
    )
  )
}

#' Construct a PXWEB API v2 request object.
#'
#' @param body a \code{pxweb_query_v2} object.
#' @param extra_query additional URL query parameters.
#'
#' @keywords internal
pxweb_query_v2_request <- function(body, extra_query = list()) {
  checkmate::assert_class(body, "pxweb_query_v2")
  checkmate::assert_list(extra_query)

  structure(
    list(
      body = body,
      extra_query = extra_query
    ),
    class = c("pxweb_query_v2_request", "list")
  )
}
