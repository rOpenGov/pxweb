#' Build the url to a PXWEB api
#' 
#' @keywords internal
#' 
build_pxweb_url <- function(x){
  UseMethod("build_pxweb_url")
}

#' @rdname build_pxweb_url
#' @keywords internal
build_pxweb_url.list <- function(x) {
  assert_pxweb_url(x)
  build_pxweb_url(x$url)
}

#' @rdname build_pxweb_url
#' @keywords internal
build_pxweb_url.character <- function(x) {
  build_pxweb_url(httr::parse_url(x))
}

#' @rdname build_pxweb_url
#' @keywords internal
build_pxweb_url.pxweb <- function(x) {
  build_pxweb_url(x$url)
}

#' @rdname build_pxweb_url
#' @keywords internal
build_pxweb_url.pxweb_api_catalogue_entry <- function(x) {
  base_url <- x$url
  base_url <- gsub("\\[version\\]", base_url, replacement = x$version[1])
  base_url <- gsub("\\[lang\\]", base_url, replacement = x$lang[1])
  build_pxweb_url(base_url)
}

#' @rdname build_pxweb_config_url
#' @keywords internal
build_pxweb_url.url <- function(x) {
  scheme <- x$scheme
  hostname <- x$hostname
  if (!is.null(x$port)) {
    port <- paste0(":", x$port)
  } else {
    port <- NULL
  }
  path <- paste(gsub("^/", "", x$path), collapse = "/")
  paste0(scheme, "://", hostname, port, "/", path)
}


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
  paste0(build_pxweb_url(x), "?config")
}

