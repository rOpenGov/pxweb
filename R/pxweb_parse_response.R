#' Parse the response from a PXWEB API (advanced)
#'
#' @description
#' The function parses the response from a call made to a PXWEB API
#' using the \code{httr} R package. In this way it is possible to parse the
#' content of calls made outside the pxweb R package.
#'
#' @param x a \code{httr} response object from a PXWEB call.
#'
#' @export
pxweb_parse_response <- function(x) {
  checkmate::assert_class(x, "response")

  pxq <- pxweb_query(x)
  v2_output_format <- pxweb_response_v2_output_format(x)
  if (is.null(pxq) && !is.null(v2_output_format) && !v2_output_format %in% c("json-stat2")) {
    obj <- suppressWarnings(httr::content(x, as = "raw"))
    obj_path <- file.path(tempdir(), paste0(digest::sha1(obj), ".", pxweb_v2_output_file_extension(v2_output_format)))
    writeBin(con = obj_path, object = obj)
    return(obj_path)
  } else if (is.null(pxq) || pxq$response %in% c("json", "json-stat", "json-stat2")) {
    obj <- suppressWarnings(httr::content(x, as = "parsed"))
  } else if (pxq$response %in% pxweb_file_response_formats()) {
    obj <- suppressWarnings(httr::content(x, as = "raw"))
    obj_path <- file.path(tempdir(), paste0(digest::sha1(obj), ".", pxq$response))
    writeBin(con = obj_path, object = obj)
    return(obj_path)
  }

  try_obj <- try(pxweb_database_list(obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    try_obj <- as_pxweb_levels(try_obj)
    return(try_obj)
  }

  try_obj <- try(pxweb_levels(obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    return(try_obj)
  }

  try_obj <- try(pxweb_metadata_v2(obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    return(try_obj)
  }

  try_obj <- try(pxweb_metadata(obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    return(try_obj)
  }

  try_obj <- try(pxweb_data(x = obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    return(try_obj)
  }

  try_obj <- try(pxweb_data_jsonstat(x = obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    return(try_obj)
  }

  try_obj <- try(pxweb_data_v2(x = obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    return(try_obj)
  }

  try_obj <- try(pxweb_table_response_v2(x = obj), silent = TRUE)
  if (!inherits(try_obj, "try-error")) {
    return(try_obj)
  }

  stop("Incorrect return response from PXWEB API url: \n", x$url, call. = FALSE)
}


#' @rdname pxweb_parse_response
#' @export
is_pxweb_response <- function(x) {
  !inherits(try(pxweb_parse_response(x), silent = TRUE), "try-error")
}

#' Get the requested PXWEB API v2 output format from a response URL.
#'
#' @description
#' Extracts the \code{outputFormat} query parameter from a PXWEB API v2
#' response URL. The value is lower-cased so it can be used for response
#' routing. If the response URL has no \code{outputFormat} parameter, the
#' function returns \code{NULL}.
#'
#' @param x a \code{httr} response object.
#'
#' @return
#' a lower-case output format string, or \code{NULL}.
#'
#' @keywords internal
pxweb_response_v2_output_format <- function(x) {
  checkmate::assert_class(x, "response")
  u <- try(httr::parse_url(x$url), silent = TRUE)
  if (inherits(u, "try-error")) {
    return(NULL)
  }
  output_format <- u$query$outputFormat
  if (is.null(output_format)) {
    return(NULL)
  }
  tolower(output_format)
}

#' Convert a PXWEB API v2 output format to a file extension.
#'
#' @description
#' Maps a PXWEB API v2 \code{outputFormat} value to the file extension used
#' when non JSON-stat2 responses are written to a temporary file. Most formats
#' are already valid extensions; \code{json-px} is normalized to \code{json}.
#'
#' @param output_format a PXWEB API v2 output format string.
#'
#' @return
#' a file extension string.
#'
#' @keywords internal
pxweb_v2_output_file_extension <- function(output_format) {
  checkmate::assert_string(output_format, min.chars = 1)
  if (identical(output_format, "json-px")) {
    return("json")
  }
  output_format
}
