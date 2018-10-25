#' Build the url to get the config from a PXWEB api
#' 
#' @keywords internal
#' 
build_pxweb_config_url <- function(x){
  UseMethod("build_pxweb_config_url")
}

#' @rdname build_pxweb_config_url
#' @keywords internal
build_pxweb_config_url.list <- function(x) {
  assert_pxweb_url(x)
  build_pxweb_config_url(x$url)
}

#' @rdname build_pxweb_config_url
#' @keywords internal
build_pxweb_config_url.pxweb <- function(x) {
  build_pxweb_config_url(x$url)
}

#' @rdname build_pxweb_config_url
#' @keywords internal
build_pxweb_config_url.url <- function(x) {
  scheme <- x$scheme
  hostname <- x$hostname
  if (!is.null(x$port)) {
    port <- paste0(":", x$port)
  } else {
    port <- NULL
  }
  path <- paste(gsub("^/", "", x$path), collapse = "/")
  paste0(scheme, "://", hostname, port, "/", path, "?config")
}

