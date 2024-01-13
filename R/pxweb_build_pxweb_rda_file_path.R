#' Build or get the tmp_rda_file_path from an url or pxweb_api_s3 object
#'
#' @details
#' The hostname is used to
#'
#' @param x object to create tmp_file_path for.
#'
#' @keywords internal
build_pxweb_rda_file_path <- function(x) {
  UseMethod("build_pxweb_rda_file_path")
}

#' @rdname build_pxweb_rda_file_path
#' @keywords internal
build_pxweb_rda_file_path.character <- function(x) {
  checkmate::assert_string(x)
  parsed_url <- parse_url_or_fail(x)
  build_pxweb_rda_file_path(parsed_url)
}

#' @rdname build_pxweb_rda_file_path
#' @keywords internal
build_pxweb_rda_file_path.url <- function(x) {
  checkmate::assert_class(x, "url")
  tmp_dir <- pxweb_tempdir()
  file.path(tmp_dir, paste0(make.names(pxweb_api_name(x)), ".rda"))
}

#' @rdname build_pxweb_rda_file_path
#' @keywords internal
build_pxweb_rda_file_path.pxweb <- function(x) {
  checkmate::assert_class(x, "pxweb")
  x$paths$rda_file_path
}

#' @rdname build_pxweb_rda_file_path
#' @keywords internal
build_pxweb_rda_file_path.list <- function(x) {
  assert_pxweb_url(x)
  build_pxweb_rda_file_path(x$url)
}
