#' Build the url to get the config from a PXWEB api
#' 
#' @keywords internal
build_pxweb_api_s3_config_url <- function(url) {
  checkmate::assert_class(url, "url")
  scheme <- url$scheme
  hostname <- url$hostname
  if (!is.null(url$port)) {
    port <- paste0(":", url$port)
  } else {
    port <- NULL
  }
  path <- paste(gsub("^/", "", url$path), collapse = "/")

  paste0(scheme, "://", hostname, port, "/", path, "?config")
}